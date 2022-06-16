module Effect.LoggerSpec (spec) where

import Control.Algebra (run)
import Control.Carrier.State.Strict (execState)
import Control.Carrier.Writer.Strict (execWriter)
import Control.Concurrent (getNumCapabilities)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Test.Hspec (Spec, describe, it, shouldBe)

import Control.Carrier.TaskPool (forkTask, withTaskPool)
import Control.Monad (forM_)
import Effect.Logger (Severity (..), logInfo, pretty, renderIt, withStateLogger, withWriterLogger)

spec :: Spec
spec = do
  describe "withWriterLogger" $ do
    it "records logged messages" $ do
      let messages =
            run . execWriter . withWriterLogger @Seq SevInfo $ do
              logInfo "this is a test message"
              logInfo "this is another test message"
      renderIt <$> messages `shouldBe` Seq.fromList ["this is a test message", "this is another test message"]

  describe "withStateLogger" $ do
    it "records logged messages" $ do
      let messages =
            run . execState Seq.empty . withStateLogger @Seq SevInfo $ do
              logInfo "this is a test message"
              logInfo "this is another test message"
      renderIt <$> messages `shouldBe` Seq.fromList ["this is a test message", "this is another test message"]

    it "records logged messages from forked threads" $ do
      capabilities <- getNumCapabilities

      messages <-
        execState Seq.empty
          . withStateLogger @Seq SevInfo
          . withTaskPool capabilities (logInfo . pretty . show)
          $ forM_ [0 .. capabilities] $
            \i -> forkTask $ do
              logInfo $ pretty $ show @(Int, Int) (i, 1)
              logInfo $ pretty $ show @(Int, Int) (i, 2)

      renderIt <$> messages `shouldBe` Seq.fromList ["this is a test message", "this is another test message"]
