module Effect.LoggerSpec (spec) where

import Control.Algebra (run)
import Control.Carrier.Reader (runReader)
import Control.Carrier.TaskPool (forkTask, withTaskPool)
import Control.Carrier.Writer.Strict (execWriter)
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TMQueue (TMQueue, newTMQueueIO, readTMQueue)
import Data.Foldable (for_, traverse_)
import Data.Functor.Extra ((<$$>))
import Data.List (sort)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text.Extra (showT)
import Effect.Logger (Severity (..), logInfo, pretty, renderIt, withConcurrentWriterLogger, withWriterLogger)
import Test.Hspec (Spec, describe, it, shouldBe)

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

  describe "withTVarLogger" $ do
    it "records logged messages from a single thread" $ do
      logs <- newTMQueueIO
      runReader logs . withConcurrentWriterLogger SevInfo $ do
        logInfo "this is a test message"
        logInfo "this is another test message"
      messages <- renderIt <$$> readAllTMQueue logs
      messages `shouldBe` ["this is a test message", "this is another test message"]

    it "records logged messages from forked threads" $ do
      capabilities <- getNumCapabilities
      let threads = [0 .. capabilities]
          msgIds :: [Int] = [0 .. 5]

      logs <- newTMQueueIO
      runReader logs
        . withConcurrentWriterLogger SevInfo
        . withTaskPool capabilities (const $ pure ())
        $ for_ threads $
          \t -> forkTask $ traverse_ (logInfo . pretty . show . (t,)) msgIds
      messages <- renderIt <$$> readAllTMQueue logs

      let expected = showT @(Int, Int) <$> [(t, m) | t <- threads, m <- msgIds]
      sort messages `shouldBe` sort expected
  where
    readAllTMQueue :: TMQueue a -> IO [a]
    readAllTMQueue q = fmap reverse $ atomically $ readAllTMQueue' q []

    readAllTMQueue' :: TMQueue a -> [a] -> STM [a]
    readAllTMQueue' q acc = do
      message <- readTMQueue q
      case message of
        Just m -> readAllTMQueue' q $ m : acc
        Nothing -> pure acc
