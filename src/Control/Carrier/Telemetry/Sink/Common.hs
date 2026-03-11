module Control.Carrier.Telemetry.Sink.Common (
  TelemetrySink (..),
  emitTelemetry,
) where

import Control.Algebra (Has)
import Control.Carrier.Lift (Lift)
import Control.Carrier.Telemetry.Sink.Endpoint (sinkTelemetryToEndpoint)
import Control.Carrier.Telemetry.Sink.File (sinkTelemetryToFile)
import Control.Carrier.Telemetry.Types (TelemetryRecord, TelemetrySink (..))

-- | Emits telemetry to provided sink.
emitTelemetry :: Has (Lift IO) sig m => TelemetrySink -> TelemetryRecord -> m ()
emitTelemetry (TelemetrySinkToFile debugDirRef) = sinkTelemetryToFile debugDirRef
emitTelemetry (TelemetrySinkToEndpoint apiOpts) = sinkTelemetryToEndpoint apiOpts
