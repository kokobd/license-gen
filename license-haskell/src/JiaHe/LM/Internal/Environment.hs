{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JiaHe.LM.Internal.Environment
    ( readDefIV
    ) where

import           Data.Configurator   (Worth (..), load, require)
import           Data.Word           (Word8)
import           Language.Haskell.TH
--

readDefIV :: Q Exp
readDefIV = do
  iv :: [Word8] <- runIO $ do
    cfg <- load [Required cipherCfgPath]
    read <$> require cfg "iv"
  return $ ListE $ fmap (LitE . IntegerL . fromIntegral) iv

cipherCfgPath :: FilePath
cipherCfgPath =
#ifdef PRODUCTION
  "config/cipher-production.cfg"
#else
  "config/cipher.cfg"
#endif
