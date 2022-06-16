module Effect.LoggerSpec (spec) where

import Control.Algebra (run)
import Control.Carrier.Writer.Strict (execWriter)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Test.Hspec (Spec, describe, it, shouldBe)

import Effect.Logger (Severity (..), logInfo, renderIt, withWriterLogger)

spec :: Spec
spec = do
  describe "Writer-based Logger" $ do
    it "should record logged messages" $ do
      let messages =
            run . execWriter . withWriterLogger @Seq SevInfo $ do
              logInfo "this is a test message"
              logInfo "this is another test message"
      renderIt <$> messages `shouldBe` Seq.fromList ["this is a test message", "this is another test message"]
