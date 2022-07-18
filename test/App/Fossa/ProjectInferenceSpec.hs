module App.Fossa.ProjectInferenceSpec (spec) where

import App.Fossa.ProjectInference (linesWithCR)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "linesWithCR" $ do
    it "should remove CR from lines if present" $
      linesWithCR "a\r\nb\r\n" `shouldBe` ["a", "b"]

    it "should remove CR from lines" $
      linesWithCR "a\nb\n" `shouldBe` ["a", "b"]
