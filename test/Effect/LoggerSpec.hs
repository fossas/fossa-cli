module Effect.LoggerSpec (
  spec,
) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Control.Carrier.Writer.Strict (WriterC, execWriter)
import Effect.Logger (AnsiStyle, Doc, LoggerC, Severity (..), logInfo, renderIt, withWriterLogger)

spec :: Spec
spec = do
  describe "Writer-based Logger" $ do
    it "should record logged messages" $ do
      let action :: LoggerC (WriterC [Doc AnsiStyle] IO) () = do
            logInfo "this is a test message"
            logInfo "this is another test message"
      messages <- execWriter . withWriterLogger SevInfo $ action

      renderIt <$> messages `shouldBe` ["this is a test message", "this is another test message"]
