{-# LANGUAGE ForeignFunctionInterface #-}

module Export
    ( encryptLicense
    , decryptLicense
    ) where

import JiaHe.LM.Foreign
import Foreign.C.String

foreign export ccall encryptLicense :: CString -> IO CString
foreign export ccall decryptLicense :: CString -> IO CString
