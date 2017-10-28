{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module JiaHe.LM.Internal.LicenseImpl where

import qualified Data.Aeson               as Aeson
import qualified Data.ByteString          as BS
import           Data.Either.Utils        (maybeToEither)
import qualified Data.Text                as Text
import           Data.Time
import           Data.Time.Format         (formatTime)
import           Data.Word
import           GHC.Generics
import           JiaHe.LM.Internal.Cipher
import           Text.Read                (readMaybe)

data License = License
  { siteCount      :: Word32
  , expirationTime :: UTCTime
  , companyName    :: Text.Text
  , softwareName   :: Text.Text
  } deriving (Generic, Show, Eq)

{-|
Encrypts a license with given key.

The encryption might fail,
if the key is too long
-}
encryptLicense :: BS.ByteString -- ^the key for encryption. Shouldn't be longer than 32 bytes.
               -> License -- ^the license to encrypt
               -> Either Text.Text BS.ByteString -- ^the bytestring encrypted from the license
encryptLicense key = encrypt key . writeLicense

{-|
Decrypt a bytestring as a license, using given key.

The decryption will fail if the bytestring is not valid.
-}
decryptLicense :: BS.ByteString -- ^the key for decryption. Shouldn't be longer than 32 bytes.
               -> BS.ByteString -- ^the bytestring to decrypt from
               -> Either Text.Text License -- ^decrypted license
decryptLicense key ctext =
  decrypt key ctext >>= readLicense

timeFormat :: String
timeFormat = "%F/%T"

writeLicense :: License -> Text.Text
writeLicense lcs =
  Text.intercalate (Text.singleton '\n') [
    Text.pack . show . siteCount $ lcs
  , Text.pack $ formatTime defaultTimeLocale timeFormat (expirationTime lcs)
  , companyName lcs
  , softwareName lcs
  ]

readLicense :: Text.Text -> Either Text.Text License
readLicense text =
  case fields of
    (sites':time':company':software':_) ->
      maybeToEither (Text.pack "Ill-formatted content.") $
      License <$> (readMaybe . Text.unpack $ sites')
              <*> parseTimeM False defaultTimeLocale timeFormat (Text.unpack time')
              <*> Just company'
              <*> Just software'
    _ -> Left $ Text.pack "Insufficient fields."
  where
    fields = take 4 $ Text.split (=='\n') text

{--
safeIndex :: (Foldable t, Integral i) => t a -> i -> Maybe a
safeIndex = foldr step (const Nothing)
  where
    step :: Integral i => a -> (i -> Maybe a) -> i -> Maybe a
    step x f i = if i == 0
                   then Just x
                   else f (i - 1)
--}

instance Aeson.ToJSON License where

instance Aeson.FromJSON License where
