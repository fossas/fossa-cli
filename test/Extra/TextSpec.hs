module Extra.TextSpec (
  spec,
) where

import Data.Text.Extra
import Test.Hspec qualified as Test
import Prelude

spec :: Test.Spec
spec = do
  Test.describe "Text splitOnceOn" $
    Test.it "should split a string once from the start" $
      splitOnceOn "-" "1-2-3" `Test.shouldBe` ("1", "2-3")

  Test.describe "Text splitOnceonEnd" $
    Test.it "should split a string once from the end" $
      splitOnceOnEnd "-" "1-2-3" `Test.shouldBe` ("1-2", "3")

  Test.describe "Text dropPrefix" $ do
    Test.it "should drop a prefix when present" $ do
      dropPrefix "foo" "foobar" `Test.shouldBe` "bar"
      dropPrefix "foo" "foofoobar" `Test.shouldBe` "foobar"

    Test.it "should leave the string unchanged when the prefix is missing" $ do
      dropPrefix "foo" "bar" `Test.shouldBe` "bar"
      dropPrefix "foo" "" `Test.shouldBe` ""

  Test.describe "Text breakOnEndAndRemove" $
    Test.it "should break a string from last needle (with last needle removed)" $ do
      breakOnEndAndRemove ":" "a:b" `Test.shouldBe` Just ("a", "b")
      breakOnEndAndRemove ":" "a:b:c" `Test.shouldBe` Just ("a:b", "c")
      breakOnEndAndRemove ":" "some.registry:5000/org/app:tag" `Test.shouldBe` Just ("some.registry:5000/org/app", "tag")
