module Extra.TextSpec (
  spec,
) where

import Data.Text.Extra ( dropPrefix, splitOnceOn, splitOnceOnEnd )
import Test.Hspec ( describe, it, shouldBe, Spec ) 

spec :: Spec
spec = do
  describe "Text splitOnceOn" $ do
    it "should split a string once from the start" $
      splitOnceOn "-" "1-2-3" `shouldBe` ("1", "2-3")
    
    it "should not affect a string that does not include the needle" $
      splitOnceOn "-" "1/2/3" `shouldBe` ("1/2/3", "")

  describe "Text splitOnceonEnd" $
    it "should split a string once from the end" $
      splitOnceOnEnd "-" "1-2-3" `shouldBe` ("1-2", "3")

  describe "Text dropPrefix" $ do
    it "should drop a prefix when present" $ do
      dropPrefix "foo" "foobar" `shouldBe` "bar"
      dropPrefix "foo" "foofoobar" `shouldBe` "foobar"

    it "should leave the string unchanged when the prefix is missing" $ do
      dropPrefix "foo" "bar" `shouldBe` "bar"
      dropPrefix "foo" "" `shouldBe` ""
