#!/usr/bin/env stack
{- stack
  script
  --resolver lts-6.35
  --package bytestring
  --package text
  --package cipher-aes
  --package filepath
-}

import           Crypto.Cipher.AES
import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as BS
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO       as T
import           Data.Word          (Word8)
import           System.Environment
import           System.FilePath

main :: IO ()
main = do
  [key, filePath] <- getArgs
  let key_bs = T.encodeUtf8 $ T.pack key
  if BS.length key_bs /= 32
    then error "Length of key must be 32 bytes"
    else do
      allContents <- BS.readFile filePath
      let (iv, input) = BS.splitAt 16 allContents
      let output = decryptCTR (initAES key_bs) iv input
      let newFilePath = dropExtension filePath
      T.writeFile newFilePath $ T.decodeUtf8 output
  return ()
