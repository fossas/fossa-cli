module Extra.ListSpec (spec) where

import Data.List.Extra (head', singleton, (!?))
import Data.Maybe (isJust)
import Data.Void (Void)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
  describe "head'" $ do
    it "should return the first item of a non-empty list" $
      head' [True, False, False] `shouldBe` Just True

    it "should return Nothing for an empty list" $
      head' [] `shouldBe` (Nothing :: Maybe ())

    it "should not loop for infinite lists" $
      head' (True : repeat False) `shouldBe` Just True

    it "should not evaluate any of the items in the list" $
      head' (repeat undefined :: [Void]) `shouldSatisfy` isJust

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
