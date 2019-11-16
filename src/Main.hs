module Main where

import qualified Data.ByteString as B
import Data.Receipt.Parsers (pReceipt)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Text.Megaparsec (parseTest)

-- | Load and decode a file into `Text`.
loadFile :: FilePath -> IO Text
loadFile path = decodeUtf8 <$> B.readFile path

-- | AE Receipt.
receipt :: IO Text
receipt = loadFile "resources/receipt.txt"

-- | Try parsing the receipt.
main :: IO ()
main = receipt >>= parseTest pReceipt >>= print
