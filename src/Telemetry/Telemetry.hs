{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Telemetry.Telemetry (
  Telemetry,
  TelemetryCtx (..),
  withTelemetry,
  runTelemetry,
  recordRawLogMessage,
  recordFeatureUsage,

  -- * For testing // Draft WIP
  bench, 
) where

import Control.Algebra (Algebra (..), Has, send, type (:+:) (..))
import Control.Carrier.Finally (runFinally)
import Control.Carrier.Output.IO (runOutput)
import Control.Carrier.Reader (ReaderC, runReader)
import Control.Carrier.StickyLogger (StickyLogger, logSticky', runStickyLogger)
import Control.Carrier.TaskPool (Progress (Progress, pCompleted, pQueued, pRunning), TaskPool, forkTask, withTaskPool)
import Control.Concurrent.STM (atomically)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Reader (ask)
import Control.Exception (bracket)
import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (ToJSON (toJSON), Value)
import Data.Aeson.Types (ToJSONKey (toJSONKey), toJSONKeyText)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Effect.Logger (Color (Cyan, Green, Yellow), Pretty (pretty), Severity (SevInfo), annotate, color, withDefaultLogger)
import GHC.Conc.IO (threadDelay)
import Network.HTTP.Req (
  POST (POST),
  ReqBodyJson (ReqBodyJson),
  defaultHttpConfig,
  http,
  ignoreResponse,
  port,
  req,
  runReq,
  (/:),
 )
import System.Random (randomRIO)
import Telemetry.AsyncQueue (
  AsyncQueue,
  closeAsyncQueue,
  newAsyncQueue,
  writeAsyncQueue,
 )
import Telemetry.Instrument (CounterRegistry, getCounterRegistry, incCount, mkCounterRegistry)

-- Todo: Add remaining event handlers
-- ex: LogError, LogWarning, RecordAnalysisEvent, so on ... ...
-- May need add renderTelemetry to ToDiagnostic class for easy compat
data Telemetry m a where
  LogRawMessage :: Text -> Telemetry m ()
  RecordFeatureUsage :: TelemetryFeatureIdentifier -> Telemetry m ()

-- Todo: Add remaining
data TelemetryFeatureIdentifier
  = ExperimentalBinaryDependencyAnalysis
  | ExperimentalGradleConfiguration
  deriving (Show, Eq, Ord)

instance ToJSON TelemetryFeatureIdentifier where
  toJSON = toJSON . show

instance ToJSONKey TelemetryFeatureIdentifier where
  toJSONKey = toJSONKeyText (toText . show)

type FeatureUsageCounters = CounterRegistry TelemetryFeatureIdentifier Int

-- Todo: Add necessary datum for other events
data TelemetryCtx = TelemetryCtx
  { logsQ :: AsyncQueue Text
  , featureCounterRegistry :: FeatureUsageCounters
  }

newtype TelemetryC m a = TelemetryC {runTelemetryC :: ReaderC TelemetryCtx m a}
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

instance (Algebra sig m, MonadIO m, Has (Lift IO) sig m) => Algebra (Telemetry :+: sig) (TelemetryC m) where
  alg hdl sig ctx = case sig of
    L op -> do
      telemetryCtx <- TelemetryC (ask @(TelemetryCtx))
      case op of
        RecordFeatureUsage entry -> do
          ctx <$ sendIO (atomically $ incCount entry (featureCounterRegistry telemetryCtx))
        LogRawMessage entry -> do
          ctx <$ (liftIO . writeAsyncQueue (logsQ telemetryCtx) $ entry)
    R other -> TelemetryC (alg (runTelemetryC . hdl) (R other) ctx)

runTelemetry :: TelemetryCtx -> TelemetryC m a -> m a
runTelemetry telemetryQs = runReader (telemetryQs) . runTelemetryC

-- Need to have a better model for catching async and sync exceptions,
-- .
-- Testbed // Todo:
--  1) unhandled sync/async exception during (action)
--  2) unhandled sync/async exception communicating to receiver (backend)
withTelemetry :: (TelemetryCtx -> IO c) -> IO c
withTelemetry action =
  withCounter $ \featureCounter -> do
    withQ $ \metricQ -> do
      action (TelemetryCtx metricQ featureCounter)
  where
    withCounter =
      bracket
        (atomically mkCounterRegistry)
        ( \counterRegistry -> do
            counterState <- atomically $ getCounterRegistry counterRegistry
            localhostCounterSink $ toJSON (counterState)
        )
    withQ = bracket (newAsyncQueue 10 (localhostQueueSink)) (closeAsyncQueue)


-- Todo: Fix Implementation
-- Todo: Fix sink handling
localhostCounterSink :: MonadIO io => Value -> io ()
localhostCounterSink counterState = runReq defaultHttpConfig $ do
  let body = counterState
  _ <-
    req
      POST
      (http "localhost" /: "post")
      (ReqBodyJson body)
      (ignoreResponse)
      (port 3000)
  pure ()

-- Todo: Fix Implementation
-- Todo: Error Handling
-- Todo: Fail with debug message
localhostQueueSink :: MonadIO io => NonEmpty (Text) -> io ()
localhostQueueSink m = runReq defaultHttpConfig $ do
  let body = toJSON $ toList m
  _ <-
    req
      POST
      (http "localhost" /: "post")
      (ReqBodyJson body)
      (ignoreResponse)
      (port 3000)
  pure ()

recordRawLogMessage :: Has Telemetry sig m => Text -> m ()
recordRawLogMessage telemetryRecordEntry = send (LogRawMessage telemetryRecordEntry)

recordFeatureUsage :: Has Telemetry sig m => TelemetryFeatureIdentifier -> m ()
recordFeatureUsage feature = send (RecordFeatureUsage feature)

-- Example Server for testing: node sample.js
-- 
-- const http = require("http");

-- const server = http.createServer(async (req, res) => {
--     const buffers = [];
  
--     for await (const chunk of req) {
--       buffers.push(chunk);
--     }
  
--     const data = Buffer.concat(buffers).toString();
--     const dataJson = JSON.parse(data);

--     console.log('Got', dataJson) 

--     res.write(JSON.stringify({}));
--     res.end();
-- });

-- server.listen(3000);
-- . 
-- .
-- 
-- cabal repl
-- :l Telemetry.Telemetry
-- bench
bench :: IO ()
bench = do
  withTelemetry $ \telemetry -> do
    _ <-
      withDefaultLogger SevInfo
        . runOutput
        . runStickyLogger SevInfo
        . withTaskPool 4 updateProgress
        . runFinally
        . runTelemetry telemetry
        $ do
          -- Mimic Logs Usage
          forM_
            ([1, 2 .. 800])
            ( \i -> do
                randomDelay <- randomRIO (500, 15000)
                addLogs randomDelay i
            )

          -- Mimic Counter Usage
          forM_
            ([1, 2 .. 700])
            ( \i -> do
                randomDelay <- randomRIO (500, 15000)
                addUsage randomDelay i ExperimentalGradleConfiguration
            )

    pure ()

  print ("----------" :: Text)
  print ("Finished!" :: Text)
  where
    addUsage :: (Has (Lift IO) sig m, Has Telemetry sig m, Has TaskPool sig m) => Int -> Int -> TelemetryFeatureIdentifier -> m ()
    addUsage randomDelay _ feature = do
      _ <- forkTask $ do
        sendIO $ threadDelay (randomDelay)
        recordFeatureUsage feature
      pure ()

    addLogs :: (Has (Lift IO) sig m, Has Telemetry sig m, Has TaskPool sig m) => Int -> Int -> m ()
    addLogs randomDelay iterNumber = do
      _ <- forkTask $ do
        sendIO $ threadDelay (randomDelay)
        recordRawLogMessage . toText $ "Logs: " ++ (show iterNumber)
      pure ()

    updateProgress :: Has StickyLogger sig m => Progress -> m ()
    updateProgress Progress{..} =
      logSticky'
        ( "[ "
            <> annotate (color Cyan) (pretty pQueued)
            <> " Waiting / "
            <> annotate (color Yellow) (pretty pRunning)
            <> " Running / "
            <> annotate (color Green) (pretty pCompleted)
            <> " Completed"
            <> " ]"
        )
