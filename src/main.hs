{-# LANGUAGE DoAndIfThenElse  #-}
{-# LANGUAGE FlexibleContexts #-}

import           Text.Regex.PCRE


import           Control.Concurrent.Async
import qualified Data.Conduit               as C
import qualified Data.Conduit.Binary        as CB
-- import qualified Data.Conduit.Base64 as CB64

import qualified Data.ByteString            as B
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as BL
-- import qualified Data.ByteString.Lazy.Char8 as BLC

import           Control.Applicative
import           Control.Arrow              (second, (>>>))
import           Control.Monad
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Maybe
import           Data.Char                  (ord)
import           Data.String.Utils          (replace)
import           System.Directory           (createDirectoryIfMissing, getHomeDirectory,
                                             getCurrentDirectory, getDirectoryContents, renameFile)
import           System.Environment         (getArgs)
import qualified System.IO                  as IO

isAttachement :: BL.ByteString -> Bool
isAttachement line = line =~ "Content-Disposition:\\s*attachment;|filename(\\*[0-9]+\\*?)?="

isFromSender :: BL.ByteString -> Bool
isFromSender line = line =~ "From\\s*:"

isBase64 :: BL.ByteString -> Bool
isBase64 line = line =~ "^[/=+A-Za-z0-9]+$"


-- Controle l'execution du parser
-- elimine toutes les lignes jusqu'à trouver une qui match "condition" puis concataine les lignes
-- suivantes qui match $ Retourne B.empty si EOF
readUntil ::  (BL.ByteString -> Bool) -> C.ResumableSource IO B.ByteString
              -> IO (C.ResumableSource IO B.ByteString, B.ByteString)
readUntil condition resourceT =
    second (B.concat . BL.toChunks ) <$> loop condition resourceT False

    where
        loop cc rr exit = do
            val <- runEitherT $ parserRead cc rr exit
            case val of
                Left ret -> case ret of
                                Nothing -> return (rr, BL.empty)
                                Just r -> if exit
                                              then return (r, BL.empty)
                                              else loop condition r False
                Right (res, str) -> second (BL.append str) <$> loop condition res True



-- Try to read a line respecting condition
parserRead :: (BL.ByteString -> Bool) -> C.ResumableSource IO B.ByteString -> Bool
              -> EitherT (Maybe (C.ResumableSource IO B.ByteString)) IO (C.ResumableSource IO B.ByteString, BL.ByteString)
parserRead condition resourceT putBackIfFail = do
    (r, _) <- liftIO $ dropSpaceFrom resourceT
    (r1, line) <- liftIO $ getLineFrom r

    when (line == BL.empty) $ left Nothing -- Check END of File

    if condition line
        then right (r1, line)
        else left . Just $ if putBackIfFail
                               then r
                               else r1
    -- Return the previous ressource if we read one line too much (when not matching anymore)



    where
        getLineFrom r =  r C.$$++ CB.takeWhile (/= (fromIntegral $ ord '\n')) C.=$ CB.sinkLbs
        dropSpaceFrom r  =  r C.$$++ CB.takeWhile (== (fromIntegral $ ord '\n')) C.=$ CB.sinkLbs


-- TODO : betting handling of filename encoding
-- Extract the filename of the attachment
extractFilename :: B.ByteString -> B.ByteString
extractFilename str = if isEncoded str
                       then if isEncodedPrintable str
                            then  BC.pack . replaceEncodedChars . BC.unpack $ getFilename rEncodedBytes --Use some mixed of ascii and hexa
                            else B64.decodeLenient $ getFilename rEncodedBytes --Base64 yeahh !!!
                       else getFilename rASCIIBytes
    where
        isEncoded line = line =~ "filename(\\*[0-9]+)?=\"=\\?[^\\?]+\\?[BQ]\\?"
        isEncodedPrintable line = line =~ "filename(\\*[0-9]+\\*?)?=\"=\\?[^\\?]+\\?Q\\?"
        rEncodedBytes = "=\\?[^\\?]+\\?[BQ]\\?([^\\?]+)\\?="
        rASCIIBytes = "filename\\*?[0-9]*=\"?([^\";]+)\"?;?"
        getFilename regex =  foldl (\x y -> x `B.append` (y !! 1)) B.empty
                                   ((\m -> getAllTextSubmatches $ m =~ regex :: [B.ByteString])
                                   <$> (getAllTextMatches $ str =~ regex :: [B.ByteString]))


        -- replaceEncodedChars :: String -> String
        replaceEncodedChars line = do
                                  let m = line =~ "=[0-9A-Za-z]{2}" :: String
                                  if null m
                                      then line
                                      else replaceEncodedChars $ replace m "_" line

-- Extract the email address of the sender
extractSenderEmail :: B.ByteString -> B.ByteString
extractSenderEmail str = getAllTextSubmatches ((=~) str $ if str =~ "From:\\s*[^<]*<([^>\\s]+)>"
                                                         then "From:\\s*[^<]*<([^>\\s]+)>"
                                                         else "From:\\s*(.*)"
                                              ) !! 1


-- Try to extract attachment file while we found some
parseWhilePossible :: C.ResumableSource IO B.ByteString -> String -> IO [Async ()]
parseWhilePossible resourceT senderName = loop resourceT []
    where
        loop res buffer = do
            ret <- runMaybeT $ parseAttachement res senderName (getSenderDirectory senderName)
            case ret of
                Just (r, lock) -> loop r (buffer ++ [lock])
                _ -> return buffer


-- Parse an attachment file -- If filename empty it's the end
parseAttachement ::  C.ResumableSource IO B.ByteString -> String -> IO String
                     -> MaybeT IO (C.ResumableSource IO B.ByteString, Async ())
parseAttachement r senderName getSenderDir = do
    (res1, attachmentStr) <- liftIO $ readUntil isAttachement r
    (res2, attachmentBody) <- liftIO $ readUntil isBase64 res1

    -- Extracting absolute path where to store the attachment
    let filename = BC.unpack $ extractFilename attachmentStr

    guard(not . null $ filename)
    senderDir <- liftIO getSenderDir
    liftIO $ print senderDir
    liftIO $ print filename

    -- Write file asynchronously
    _ <- liftIO $ writeToImapMail senderName senderDir filename
    lock <- liftIO $ async $ B.writeFile (senderDir ++ filename) (B64.decodeLenient attachmentBody)
    return (res2, lock)


getSenderDirectory :: String -> IO String
getSenderDirectory senderName = do
    args <- getArgs
    rootDir <- if null args
                            then getCurrentDirectory
                            else return $ head args

    let senderDir = rootDir ++ "/" ++ escapeDirectoryName senderName ++ "/"
    _ <- createDirectoryIfMissing True senderDir
    return senderDir

    where
    escapeDirectoryName line = do let m = line =~ "[\\.@]" :: String
                                  if null m
                                      then line
                                      else escapeDirectoryName $ replace m "_" line

getMailAttachmentDirectory :: IO String
getMailAttachmentDirectory = do
    pathDir <- (++ "/.maildir/.PiecesJointes/cur/") <$> getHomeDirectory
    print pathDir
    _ <- createDirectoryIfMissing True pathDir

    return pathDir

writeToImapMail :: String -> String -> String -> IO ()
writeToImapMail senderName senderDir filename = do
    pathDir <- getMailAttachmentDirectory
    test <- getDirectoryContents pathDir
    let matches = filter (=~ senderName) test
    if null matches
        then createNewFile pathDir
        else appendToExistingFile (head matches) pathDir


    where
        appendToExistingFile file pathDir = do
            _ <- B.appendFile (pathDir ++ file) (BC.pack getFileURL)
            renameFile (pathDir ++ file) (pathDir ++ senderName ++ ":2,a")

        createNewFile pathDir =
           B.writeFile (pathDir ++ senderName ++ ":2,a") (BC.pack $ "From: " ++ senderName ++ "\n" ++ "To: piecesjointes@erebe.eu\nSubject: piecesjointes\n\n" ++ getFileURL)

        getFileURL = filename ++ " --> https://cloud.erebe.eu/public.php?service=files&t=102ad9a0c36bf17737e2aa2205d47bb3&download&path=/" ++ reverse (takeWhile (/= '/') $ drop 1 $ reverse senderDir) ++ "/" ++ filename ++ "\n"

main :: IO ()
main = do
    (res, _) <- CB.sourceHandle IO.stdin C.$$+ (CB.drop 1 C.=$ CB.sinkLbs)

    -- Lets shoot ourself in the foor with arrow for the next we will read this code
    (locks, _) <- ( second  (BC.unpack . extractSenderEmail)
                    >>> (\(x,y) -> (parseWhilePossible x y, y))
                  ) <$> readUntil isFromSender res

    locks >>= mapM_ wait

