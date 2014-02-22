{-# LANGUAGE DoAndIfThenElse  #-}
{-# LANGUAGE FlexibleContexts #-}


module MailParser where

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
import           Data.Char                  (ord, toLower)
import           Data.String.Utils          (replace)
import           System.Directory           (createDirectoryIfMissing, getHomeDirectory,
                                             getDirectoryContents, renameFile)
import qualified System.IO                  as IO
import Data.Time.Clock.POSIX

isAttachement :: BL.ByteString -> Bool
isAttachement line = line =~ "Content-Disposition:\\s*attachment;|filename(\\*[0-9]+\\*?)?="

isFromSender :: BL.ByteString -> Bool
isFromSender line = line =~ "Return-Path\\s*:"

isBase64 :: BL.ByteString -> Bool
isBase64 line = line =~ "^[/=+A-Za-z0-9]+$"


data Config = Config { attachmentPath     :: String
                     , imapAttachmentPath :: String
                     , publicURL          :: String
                     } deriving (Show, Read)

data MailInfo = MailInfo { getSenderName    :: String
                         , senderDir        :: String
                         , writeToSenderDir :: String -> B.ByteString -> IO ()
                         , getConf          :: Config
                         }


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
extractFilename str = escapeChars $ if isEncoded str
                                       then if isEncodedPrintable str
                                            then BC.pack . replaceEncodedChars . BC.unpack $ getFilename rEncodedBytes --Use some mixed of ascii and hexa
                                            else B64.decodeLenient $ getFilename rEncodedBytes --Base64 yeahh !!!
                                       else getFilename rASCIIBytes
    where
        isEncoded line = line =~ "filename(\\*[0-9]+)?=[\r\n\t ]*\"=\\?[^\\?]+\\?[BQ]\\?"
        isEncodedPrintable line = line =~ "filename(\\*[0-9]+\\*?)?=[\r\n\t ]*\"=\\?[^\\?]+\\?Q\\?"
        rEncodedBytes = "=\\?[^\\?]+\\?[BQ]\\?([^\\?]+)\\?="
        rASCIIBytes = "filename\\*?[0-9]*=[\r\n\t ]*\"?([^\";]+)\"?;?"
        getFilename regex =  foldl (\x y -> x `B.append` (y !! 1)) B.empty
                                   ((\m -> getAllTextSubmatches $ m =~ regex :: [B.ByteString])
                                   <$> (getAllTextMatches $ str =~ regex :: [B.ByteString]))


        -- replaceEncodedChars :: String -> String
        replaceEncodedChars line = do
                                  let m = line =~ "=[0-9A-Za-z]{2}" :: String
                                  if null m
                                      then line
                                      else replaceEncodedChars $ replace m "_" line

        escapeChars = BC.map (\x -> if x `elem` " ',*$&\\~#|²£%µ:;!§?<>=`^+" then '_' else x )

-- Extract the email address of the sender
extractSenderEmail :: B.ByteString -> String
extractSenderEmail str = BC.unpack $ BC.map toLower $ getAllTextSubmatches ((=~) str $ if str =~ "Return-Path:\\s*[^<]*<\\s*([^>\\s]+)\\s*>"
                                                         then "Return-Path:\\s*[^<]*<\\s*([^>\\s]+)\\s*>"
                                                         else "Return-Path:\\s*(.*)"
                                                      ) !! 1


-- Try to extract attachment file while we found some
parseWhilePossible :: C.ResumableSource IO B.ByteString -> MailInfo -> IO ([Async ()], B.ByteString)
parseWhilePossible resourceT mailInfo = loop resourceT ([], BC.pack "")
    where
        loop res (locks, urls) = do
            ret <- runMaybeT $ parseAttachement res mailInfo
            case ret of
                Just (r, lock, url) -> loop r (locks ++ [lock], urls `B.append` url)
                _ -> return (locks, urls)


-- Parse an attachment file -- If filename empty it's the end
parseAttachement ::  C.ResumableSource IO B.ByteString -> MailInfo
                     -> MaybeT IO (C.ResumableSource IO B.ByteString, Async (), B.ByteString)
parseAttachement r mailInfo = do
    (res1, attachmentStr) <- liftIO $ readUntil isAttachement r
    (res2, attachmentBody) <- liftIO $ readUntil isBase64 res1

    -- Extracting absolute path where to store the attachment
    let filename = BC.unpack $ extractFilename attachmentStr

    guard(not . null $ filename)

    -- Write file asynchronously
    lock <- liftIO $ async $  writeToSenderDir mailInfo filename (B64.decodeLenient attachmentBody)
    return (res2, lock, generatePublicURL mailInfo filename)


generatePublicURL :: MailInfo -> String -> B.ByteString
generatePublicURL mailInfo filename = BC.pack $ filename ++ " --> " ++ (publicURL . getConf $ mailInfo)
                                        ++ "/" ++ senderDir mailInfo
                                        ++ "/" ++ filename ++ "\n"

getSenderDir :: MailInfo -> String
getSenderDir mailInfo = escapeDirectoryName (getSenderName mailInfo)
    where
    escapeDirectoryName line = do let m = line =~ "[\\.@]" :: String
                                  if null m
                                      then line
                                      else escapeDirectoryName $ replace m "_" line

getWriterToSenderDirectory ::  MailInfo -> String -> B.ByteString -> IO ()
getWriterToSenderDirectory mailInfo file datas = do
    let userDir = (attachmentPath . getConf $ mailInfo) ++ senderDir mailInfo ++ "/"
    _ <- createDirectoryIfMissing True userDir
    B.writeFile (userDir ++ file) datas

    print $ "Writing file {" ++ file ++ "} to {" ++ userDir ++ "}"


writeToImapMail :: MailInfo -> B.ByteString -> IO ()
writeToImapMail mailInfo urls = do
    uuid <- show . (\x -> round x :: Integer) <$> getPOSIXTime 
    let pathDir = imapAttachmentPath . getConf $ mailInfo
    _ <- createDirectoryIfMissing True pathDir
    fileList <- getDirectoryContents pathDir
    let matches = filter (=~ getSenderName mailInfo) fileList
    if null matches
        then createNewFile pathDir uuid
        else appendToExistingFile (head matches) pathDir uuid


    where
        appendToExistingFile file pathDir uuid = do
                                            _ <- B.appendFile (pathDir ++ file) urls
                                            renameFile (pathDir ++ file) (mailPath pathDir uuid)

        createNewFile pathDir uuid = B.writeFile (mailPath pathDir uuid)
                                            $ BC.pack ("From: " ++ getSenderName mailInfo ++ "\n" ++ "To: piecesjointes@erebe.eu\nSubject: piecesjointes\n\n")
                                            `B.append` urls


        mailPath pathDir uuid = pathDir ++ getSenderName mailInfo ++ uuid ++ ":2,a"

loadConfigFile :: IO Config
loadConfigFile = do
    configFile <- join $ B.readFile <$> (++ "/.attachmentparser.rc") <$> getHomeDirectory
    return $ read (BC.unpack configFile) :: IO Config


run :: IO ()
run = do
    config <- loadConfigFile
    let mailInfo = MailInfo "" "" undefined config


    (res, _) <- CB.sourceHandle IO.stdin C.$$+ (CB.drop 0 C.=$ CB.sinkLbs)

    -- Lets shoot ourself in the foor with arrow for the next we will read this code
    (ret,mInfo) <- (        second (\x -> mailInfo { getSenderName = extractSenderEmail x })
                        >>> second (\x -> x { senderDir = getSenderDir x })
                        >>> second (\x -> x { writeToSenderDir = getWriterToSenderDirectory x })
                        >>> (\(ress, mInfo) -> (parseWhilePossible ress mInfo, mInfo))
                  ) <$> readUntil isFromSender res

    (locks,urls) <- ret
    mapM_ wait locks
    writeToImapMail mInfo urls
    print urls



