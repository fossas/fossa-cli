module Control.Effect.Telemetry (
  Telemetry (..),
  trackUsage,
  trackTimeSpent,
  trackConfig,
  trackRawLogMessage,
  setSink,
  trackResult,
) where

import Control.Algebra (Has, send)
import Control.Carrier.Telemetry.Types (CountableCliFeature, TelemetrySink)
import Data.Aeson (ToJSON (toJSON), Value)
import Data.Text (Text)
import Diag.Result (Result (Failure, Success), renderFailure, renderSuccess)
import Effect.Logger (Severity (SevError, SevWarn), renderIt)
import Prettyprinter (Doc, unAnnotate)
import Prettyprinter.Render.Terminal (AnsiStyle)

data Telemetry m a where
  TrackUsage :: CountableCliFeature -> Telemetry m ()
  TrackTimeSpent :: Text -> m a -> Telemetry m a
  TrackConfig :: Text -> Value -> Telemetry m ()
  TrackRawLogMessage :: Severity -> Text -> Telemetry m ()
  SetTelemetrySink :: TelemetrySink -> Telemetry m ()

trackUsage :: Has Telemetry sig m => CountableCliFeature -> m ()
trackUsage = send . TrackUsage

trackTimeSpent :: Has Telemetry sig m => Text -> m a -> m a
trackTimeSpent header act = send $ TrackTimeSpent header act

trackConfig :: (ToJSON cfg, Has Telemetry sig m) => Text -> cfg -> m ()
trackConfig cmd config = send . TrackConfig cmd $ toJSON config

-- | Only use this if absolutely necessary, you likely want to use 'trackResult'
trackRawLogMessage :: Has Telemetry sig m => Severity -> Doc AnsiStyle -> m ()
trackRawLogMessage sev msg = send . TrackRawLogMessage sev $ renderIt msg

setSink :: Has Telemetry sig m => TelemetrySink -> m ()
setSink = send . SetTelemetrySink

trackResult :: Has Telemetry sig m => Result a -> m ()
trackResult (Failure ew eg) = send . TrackRawLogMessage SevError $ renderIt . unAnnotate $ renderFailure ew eg "Failed"
trackResult (Success ew _) = do
  let doc = renderSuccess ew "An Issue Occurred"
  case doc of
    Nothing -> pure ()
    Just doc' -> send $ TrackRawLogMessage SevWarn $ renderIt . unAnnotate $ doc'
