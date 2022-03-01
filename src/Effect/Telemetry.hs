{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Effect.Telemetry (withTelemetry, runTelemetry, Telemetry, TelemetryCtx, recordFeatureUsage, TelemetryC) where

import Control.Algebra (Algebra (..), Has, send, type (:+:) (..))
import Control.Carrier.Reader (ReaderC, runReader)
import Control.Concurrent.STM (atomically)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Reader (ask)
import Control.Exception (bracketOnError)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (ToJSON (toJSON), Value)
import Data.Text (Text)
import Data.Tracing.Instrument (CounterRegistry, getCounterRegistry, incCount, mkCounterRegistry)
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

type FeatureUsageCounters = CounterRegistry Text Int

-- newtype TelemetryCtx = TelemetryCtx
--   { featureCounter :: FeatureUsageCounters
--   }

-- data TelemetryF a where
--   RecordFeatureUsage :: Text -> TelemetryF ()

-- $(deriveRecordable ''TelemetryF)

-- type Telemetry = Simple TelemetryF
-- type TelemetryC = SimpleC TelemetryF
-- type IgnoreTelemetryC = SimpleC TelemetryF

data Telemetry m a where
  LogRawMessage :: Text -> Telemetry m ()

newtype TelemetryCtx = TelemetryCtx
  { featureCounter :: FeatureUsageCounters
  }

newtype TelemetryC m a = TelemetryC {runTelemetryC :: ReaderC TelemetryCtx m a}
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

instance (Algebra sig m, MonadIO m, Has (Lift IO) sig m) => Algebra (Telemetry :+: sig) (TelemetryC m) where
  alg hdl sig ctx = case sig of
    L op -> do
      telemetryCtx <- TelemetryC (ask @(TelemetryCtx))
      case op of
        LogRawMessage entry -> do
          ctx <$ sendIO (atomically $ incCount entry (featureCounter telemetryCtx))
    R other -> TelemetryC (alg (runTelemetryC . hdl) (R other) ctx)

runTelemetry :: TelemetryCtx -> TelemetryC m a -> m a
runTelemetry ctx = runReader (ctx) . runTelemetryC

withTelemetry :: (TelemetryCtx -> IO c) -> IO c
withTelemetry action =
  withCounter $ \featureCounter ->
    action (TelemetryCtx featureCounter)
  where
    withCounter =
      bracketOnError
        (atomically mkCounterRegistry)
        ( \counterRegistry -> do
            counterState <- atomically $ getCounterRegistry counterRegistry
            localhostSink $ toJSON (counterState)
        )

localhostSink :: MonadIO io => Value -> io ()
localhostSink counterState = runReq defaultHttpConfig $ do
  let body = counterState
  _ <-
    req
      POST
      (http "localhost" /: "post")
      (ReqBodyJson body)
      (ignoreResponse)
      (port 3000)
  pure ()

recordFeatureUsage :: Has Telemetry sig m => Text -> m ()
recordFeatureUsage feature = send (LogRawMessage feature)
