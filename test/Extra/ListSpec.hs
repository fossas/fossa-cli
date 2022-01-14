module Extra.ListSpec (spec) where

import Data.List.Extra (singleton, (!?))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "singleton" $ do
    it "should create a one-item list" $
      singleton (3 :: Int) `shouldBe` [3]

  describe "(!?)" $ do
    it "should return the zero-indexed item at the index" $
      [False, True, False, False, False] !? 1 `shouldBe` Just True

    it "should return Nothing if the index >= length" $ do
      let list = replicate 5 ()
      list !? 5 `shouldBe` Nothing -- index == length
      list !? 6 `shouldBe` Nothing -- index == length + 1
      list !? 20 `shouldBe` Nothing -- index > length
