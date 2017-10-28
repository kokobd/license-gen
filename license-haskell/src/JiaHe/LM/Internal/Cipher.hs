{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module JiaHe.LM.Internal.Cipher where

import           Control.Applicative           (liftA2)
import           Crypto.Cipher.AES             (decryptCBC, encryptCBC, initAES)
import qualified Data.ByteString               as B
import           Data.Digest.CRC32             (crc32)
import qualified Data.Text                     as T
import           Data.Text.Encoding            (decodeUtf8, encodeUtf8)
import           Data.Word                     (Word32, Word8)
import           JiaHe.LM.Internal.Environment (readDefIV)

keyLength :: Int
keyLength = 32

ivLength :: Int
ivLength = 16

defIV :: B.ByteString
defIV = B.pack $(readDefIV)

defByte :: Word8
defByte = 42

-- |'key' must be less than 32 bytes
-- key -> text -> error message or cipher-text
-- stored cipher text format: encrypt with aes ([crc32 of key] ++ [plain text])
encrypt :: B.ByteString -> T.Text -> Either T.Text B.ByteString
encrypt key text = (\k sK t pad -> encryptCBC (initAES k) defIV
                                         (pad `mappend` sK `mappend` t))
                   <$> key' <*> saltedKey <*> text' <*> padding
  where
    key' = fillKeyToLength key
    saltedKey = hashKey <$> key'
    text' = pure $ encodeUtf8 text :: Either T.Text B.ByteString
    padding =
      liftA2 (\k t -> B.replicate (16 - (B.length k + B.length t) `mod` 16) 0)
        saltedKey text'

corruptMsg :: T.Text
corruptMsg = T.pack "The encrypted text has corrupted."

mkEither :: Bool -> a -> b -> Either a b
mkEither p x y = if p then Left x else Right y

-- |'key' must be less than 32 bytes
-- key -> cipher-text -> error message or plain text
decrypt :: B.ByteString -> B.ByteString -> Either T.Text T.Text
decrypt key cText =
  let
    key' = fillKeyToLength key
    saltedKey = hashKey <$> key'
  in
    if B.length cText `mod` 16 /= 0
      then Left corruptMsg
      else do
        k <- key'
        (sK, txt) <- return $
          (B.splitAt 4 . B.dropWhile (==0)) (decryptCBC (initAES k) defIV cText)
        keyMatches <- fmap (==sK) saltedKey
        if not keyMatches
          then Left corruptMsg
          else Right $ decodeUtf8 txt

-- |There will be no zero byte in the result bytestring
hashKey :: B.ByteString -> B.ByteString
hashKey = B.map (\x -> if x == 0 then 1 else x) . word32ToBS . crc32

fillKeyToLength :: B.ByteString -> Either T.Text B.ByteString
fillKeyToLength key =
  if B.length key > keyLength
    then Left $
      T.pack ("key can not be longer than " ++ show keyLength ++ " bytes.")
    else Right $
      key `mappend` B.replicate (keyLength - B.length key) defByte

-- |It is guranteed that the result is of 4 bytes
word32ToBS :: Word32 -> B.ByteString
word32ToBS =
  B.pack . fmap (fromIntegral . (`mod` divisor))
  . take 4 . iterate (`div` divisor)
    where divisor = 1 + fromIntegral (maxBound :: Word8) :: Word32

bsToWord32 :: B.ByteString -> Word32
bsToWord32 =
  sum . zipWith (*) (iterate (*divisor) 1). fmap fromIntegral . B.unpack
    where divisor = 1 + fromIntegral (maxBound :: Word8) :: Word32
