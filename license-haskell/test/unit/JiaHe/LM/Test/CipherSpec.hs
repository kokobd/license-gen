module JiaHe.LM.Test.CipherSpec
    ( spec
    ) where

import           Test.Hspec
import           Test.QuickCheck

import qualified Data.ByteString                 as B
import           Data.ByteString.Arbitrary
import           Data.Either                     (either, isLeft)
import qualified Data.Text                       as T
import           Data.Text.Encoding              (encodeUtf8)
import           Data.Word                       (Word32, Word8)
import           JiaHe.LM.Internal.Cipher
import           Test.QuickCheck.Instances.Tuple ((>*<))

spec :: Spec
spec = do
  describe "word32ToBS" $ do
    it "is an inversion of bsToWord32" $
      property $ \x -> (bsToWord32 . word32ToBS) x == (x :: Word32)
    it "converts 257 to [255][002][000][000]" $
      B.unpack (word32ToBS 257) `shouldBe` [1, 1, 0, 0]
  describe "fillKeyToLength" $ do
    it "fills empty keys with default bytes" $
      fillKeyToLength B.empty `shouldBe` Right (B.replicate keyLength defByte)
    it "fails when the length of key is larger than 'keyLength'" $
      property $
        \key -> let key' = fromABS key
                in  B.length key' > keyLength ==> isLeft (fillKeyToLength key')
    it "fills any key with length less than 'keyLength' to 'keyLength'" $
      property $ forAll genShortKeys
        (\key -> fillKeyToLength key `shouldSatisfy`
                either (const False) (\key' -> B.length key' == keyLength))
  describe "encrypt" $ do
    it "is an inversion of decrypt (with sample text and key)" $
      (encrypt sampleKey sampleText >>= decrypt sampleKey) `shouldBe` pure sampleText
    it "is an inversion of decrypt" $
      property $ forAll (genShortKeys >*< fmap T.pack arbitrary)
        (\(key, txt) -> (encrypt key txt >>= decrypt key) == Right txt)

-- |generate random keys with length less than 'keyLength'
genShortKeys :: Gen B.ByteString
genShortKeys = do len <- choose (0, keyLength)
                  resize len $ (fmap B.pack . listOf) (arbitrary :: Gen Word8)

sampleText :: T.Text
sampleText = T.pack "Hello, My name is zelinf."

sampleKey :: B.ByteString
sampleKey = (encodeUtf8 . T.pack) "ufida2016"
