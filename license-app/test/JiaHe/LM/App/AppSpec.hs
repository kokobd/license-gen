module JiaHe.LM.App.AppSpec
    ( spec
    ) where

import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "integration" $ do
    it "first case" $
      pendingWith "Tests not yet implemented."
