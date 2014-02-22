import           Test.QuickCheck
import           Text.Regex.PCRE
import qualified Data.ByteString.Char8      as BC
import           MailParser


testSenderExtract :: IO ()
testSenderExtract = do
    file <- readFile "test/extractSender.txt"
    quickCheck $ forAll (elements (read file :: [String])) (\x -> extractSenderEmail (BC.pack x) =~ "^([a-zA-Z0-9_\\-\\.+=~]+)@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,3})$" :: Bool)


main :: IO ()
main = testSenderExtract
