module App.Fossa.ProjectInferenceSpec (spec) where

import App.Fossa.ProjectInference (linesWithoutCR)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "linesWithoutCR" $ do
    it "should remove CR from lines if present" $
      linesWithoutCR "a\r\nb\r\n" `shouldBe` ["a", "b"]

    it "should remove CR from lines" $
      linesWithoutCR "a\nb\n" `shouldBe` ["a", "b"]
