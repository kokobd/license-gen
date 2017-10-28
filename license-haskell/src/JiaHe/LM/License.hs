{-|
Module : JiaHe.LM.License
Description : Encrypting and decrypting software licenses
Copyright : JiaHe Software, 2016
Maintainer : fengzlin@mail2.sysu.edu.cn
Stability : experimental
Portability : portable

license-haskell is a simple library for encrypting and decrypting
software license. It may help protect our application from being used without
paying.
The API is simple, 'License' represents a license, 'encryptLicense' and
'decryptLicense' convert our license between 'License' and a strict bytestring.
-}
module JiaHe.LM.License
    ( License(..)
    , encryptLicense
    , decryptLicense
    , readLicense
    , writeLicense
    ) where

import JiaHe.LM.Internal.LicenseImpl
