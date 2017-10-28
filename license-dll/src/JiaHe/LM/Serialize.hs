{-# LANGUAGE DeriveGeneric #-}

{-|
-}
module JiaHe.LM.Serialize
  ( EncryptLicense()
  , getKey
  , getLicense
  , SByteString(..)
  ) where

import           Data.Aeson
import           Data.Aeson.Types (Parser)
import qualified Data.ByteString  as BS
import           Data.Traversable (traverse)
import           Data.Word        (Word8)
import           GHC.Generics
import qualified JiaHe.LM.License as LM

data EncryptLicense = EncryptLicense
  { key     :: SByteString
  , license :: LM.License
  } deriving (Generic)

getKey :: EncryptLicense -> BS.ByteString
getKey x = case (key x) of
  SByteString bs -> bs

getLicense :: EncryptLicense -> LM.License
getLicense = license

instance FromJSON EncryptLicense where

-- |
-- serialize strict ByteString as JSON array of numbers.
newtype SByteString = SByteString BS.ByteString
                      deriving (Show, Eq)

instance ToJSON SByteString where
  toJSON (SByteString str) = toJSON $ BS.unpack str

instance FromJSON SByteString where
  parseJSON v = (SByteString . BS.pack) <$> (parseJSON v :: Parser [Word8])
