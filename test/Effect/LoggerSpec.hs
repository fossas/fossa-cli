module Effect.LoggerSpec (spec) where

import Control.Algebra (run)
import Control.Carrier.Reader (runReader)
import Control.Carrier.Writer.Strict (execWriter)
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Control.Monad (forM_)
import Data.Foldable (traverse_)
import Data.Sequence (Seq, sort)
import Data.Sequence qualified as Seq
import Test.Hspec (Spec, describe, it, shouldBe)

import Control.Carrier.TaskPool (forkTask, withTaskPool)
import Data.Functor.Extra ((<$$>))
import Data.Text.Extra (showT)
import Effect.Logger (Severity (..), logInfo, pretty, renderIt, withConcurrentLogger, withWriterLogger)

spec :: Spec
spec = do
  describe "withWriterLogger" $ do
    it "records logged messages" $ do
      let messages =
            fmap renderIt
              . run
              . execWriter
              . withWriterLogger @Seq SevInfo
              $ do
                logInfo "this is a test message"
                logInfo "this is another test message"
      messages `shouldBe` Seq.fromList ["this is a test message", "this is another test message"]

  describe "withConcurrentLogger" $ do
    it "records logged messages from a single thread" $ do
      logs <- newTVarIO Seq.empty
      runReader logs . withConcurrentLogger @Seq SevInfo $ do
        logInfo "this is a test message"
        logInfo "this is another test message"
      messages <- renderIt <$$> readTVarIO logs
      messages `shouldBe` Seq.fromList ["this is a test message", "this is another test message"]

    it "records logged messages from forked threads" $ do
      capabilities <- getNumCapabilities
      let threads = [0 .. capabilities]
          msgIds :: [Int] = [0 .. 5]

      logs <- newTVarIO Seq.empty
      runReader logs
        . withConcurrentLogger @Seq SevInfo
        . withTaskPool capabilities (const $ pure ())
        $ forM_ threads $ \t -> forkTask $ traverse_ (logInfo . pretty . show . (t,)) msgIds
      messages <- renderIt <$$> readTVarIO logs

      let expected = Seq.fromList $ showT @(Int, Int) <$> [(t, m) | t <- threads, m <- msgIds]
      sort messages `shouldBe` sort expected
