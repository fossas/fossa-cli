module App.Fossa.Telemetry.Sink.Common (
  TelemetrySink (..),
  emitTelemetry,
) where

import App.Fossa.Telemetry.Sink.Endpoint (sinkTelemetryToEndpoint)
import App.Fossa.Telemetry.Sink.File (sinkTelemetryToFile)
import App.Fossa.Telemetry.Types (TelemetryRecord, TelemetrySink (..))
import Control.Algebra (Has)
import Control.Carrier.Lift (Lift)

-- | Emits telemetry to provided sink.
emitTelemetry :: Has (Lift IO) sig m => TelemetrySink -> TelemetryRecord -> m ()
emitTelemetry TelemetrySinkToFile r = sinkTelemetryToFile r
emitTelemetry (TelemetrySinkToEndpoint apiOpts) r = sinkTelemetryToEndpoint apiOpts r
