import           Test.QuickCheck
import           Text.Regex.PCRE
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString            as B
import           Control.Applicative
import           MailParser


testSenderExtract :: IO ()
testSenderExtract = do
    file <- readFile "test/extractSender.txt"
    quickCheckWith stdArgs { maxSuccess = 5000 } $ forAll (elements (read file :: [String]))
                                                   (\x -> extractSenderEmail (BC.pack x) =~ "^([a-zA-Z0-9_\\-\\.+=~]+)@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,3})$" :: Bool)

testFilenameExtract :: IO ()
testFilenameExtract = do
    file <- BC.unpack <$> B.readFile "test/extractFilename.txt"
    quickCheckWith stdArgs { maxSuccess = 5000 } $ forAll (elements (read file :: [String]))
                                                   (\x -> extractFilename (BC.pack x) =~ "^[^ '\"*$,\n\r\t]+$"  :: Bool)

main :: IO ()
main = do
    _ <- testSenderExtract
    testFilenameExtract
