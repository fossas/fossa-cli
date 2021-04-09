module Extra.TextSpec (
    spec
) where

import qualified Test.Hspec as Test
import Prelude
import Data.Text.Extra

spec :: Test.Spec
spec = do
  Test.describe "Text splitOnceOn" $
    Test.it "should split a string once from the start" $
      splitOnceOn "-" "1-2-3" `Test.shouldBe` ("1", "2-3")

  Test.describe "Text splitOnceonEnd" $
    Test.it "should split a string once from the end" $
      splitOnceOnEnd "-" "1-2-3" `Test.shouldBe` ("1-2", "3")
