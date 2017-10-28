{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}

module JiaHe.LM.Foreign
  ( encryptLicense
  , decryptLicense
  ) where

import           Data.Aeson
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Either          (either)
import qualified Data.HashMap.Lazy    as HMap
import           Data.Maybe           (fromMaybe)
import           Foreign.C.String
import           Foreign.C.Types
import qualified JiaHe.LM.License     as LM
import qualified JiaHe.LM.Serialize   as Serialize

-- |
--  encrypt license with a key.
--  Parameters and return values are all in JSON format.
--  Sample param value:
--  @
--    { "key":"yourpassphrase"
--    , "license":
--      { "siteCount":42
--      , "expirationTime":"2017-07-19T02:20:25Z"
--      , "companyName":"Sample Company"
--      , "softwareName":"Sample Software"
--      }
--    }
--  @
--  Sample return value:
--  @
--    [71,111,172,231,30,34,253,152,214,77,210,22,175,171,101,118,166,160,
--    19,205,91,69,156,232,142,204,7,92,250,174,204,83,244,140,230,41,233,
--    248,250,141,128,136,60,205,72,196,36,45,157,217,29,152,135,193,1,226,
--    64,126,75,69,34,207,147,229]
--  @
encryptLicense
  :: CString -- ^A JSON object of key and license
  -> IO CString -- ^A JSON array of integers (encrypted byte array)
encryptLicense = wrapFunc encryptLicense'

encryptLicense' :: LBS.ByteString -> LBS.ByteString
encryptLicense' args = fromMaybe "" $ do
  args' <- (decode args :: Maybe Serialize.EncryptLicense)
  let key = Serialize.getKey args'
  let license = Serialize.getLicense args'
  cipherText <- eitherToMaybe (LM.encryptLicense key license)
  (return . encode . Serialize.SByteString) cipherText

decryptLicense :: CString -> IO CString
decryptLicense = undefined

wrapFunc :: (LBS.ByteString -> LBS.ByteString) -> CString -> IO CString
wrapFunc f cstr = do
  param <- LBS.fromStrict <$> BS.packCString cstr
  (flip BS.useAsCString) pure . LBS.toStrict . f $ param

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just
