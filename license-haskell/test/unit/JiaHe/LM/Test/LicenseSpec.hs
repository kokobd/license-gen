{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module JiaHe.LM.Test.LicenseSpec
    ( spec
    ) where

import           Test.Hspec
-- import           Test.QuickCheck

import qualified Data.Aeson                    as Aeson
import           Data.ByteString.Lazy          (ByteString)
import qualified Data.Text                     as T
import           Data.Time.Calendar
import           Data.Time.Clock
import           JiaHe.LM.Internal.LicenseImpl
import           Text.RawString.QQ

spec :: Spec
spec = do
  describe "writeLicense" $
    it "formats a license value (sample)" $
      writeLicense sampleLicense `shouldBe` sampleText
  describe "readLicense" $
    it "parses a license's text representation (sample)" $
      readLicense sampleText `shouldBe` Right sampleLicense
  describe "License toJSON" $
    it "conforms this JSON convention" $
      Aeson.encode sampleLicense `shouldBe` sampleJSON

sampleLicense :: License
sampleLicense =
  License
    42
    (UTCTime (fromGregorian 2017 7 19) (secondsToDiffTime 8425))
    (T.pack "Sample Company")
    (T.pack "Sample Software")

sampleText :: T.Text
sampleText = T.pack
  "42\n2017-07-19/02:20:25\nSample Company\nSample Software"

sampleJSON :: ByteString
sampleJSON = [r|{"companyName":"Sample Company","siteCount":42,"softwareName":"Sample Software","expirationTime":"2017-07-19T02:20:25Z"}|]
