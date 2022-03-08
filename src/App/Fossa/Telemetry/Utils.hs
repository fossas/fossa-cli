module App.Fossa.Telemetry.Utils (
  getCurrentCliEnvironment,
  getCurrentCliVersion,
  getSystemInfo,
  getCommandArgs,
  mkTelemetryCtx,
  mkTelemetryRecord,
) where

import App.Fossa.Telemetry.Types (CliEnvironment (..), SystemInfo (SystemInfo), TelemetryCmdConfig (TelemetryCmdConfig), TelemetryCtx (TelemetryCtx, telCounters, telFossaConfig, telId, telLogsQ, telStartUtcTime, telTimeSpent), TelemetryRecord (..))
import App.Version (isDirty, versionOrBranch)
import Control.Algebra (Has)
import Control.Carrier.Lift (Lift, sendIO)
import Control.Concurrent.STM (STM, atomically, newEmptyTMVarIO, tryReadTMVar)
import Control.Concurrent.STM.TBMQueue (TBMQueue, newTBMQueueIO, tryReadTBMQueue)
import Control.Monad (replicateM)
import Data.Maybe (catMaybes, fromMaybe)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Time (diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Tracing.Instrument (
  getCounterRegistry,
  mkCounterRegistry,
 )
import Data.UUID.V4 (nextRandom)
import GHC.Conc.Sync qualified as Conc
import GHC.Environment qualified as Environment
import System.Info qualified as Info

getCurrentCliVersion :: Text
getCurrentCliVersion = versionOrBranch

getCurrentCliEnvironment :: CliEnvironment
getCurrentCliEnvironment =
  if isDirty
    then CliDevelopmentEnvironment
    else CliProductionEnvironment

getCommandArgs :: IO [Text]
getCommandArgs = map toText <$> Environment.getFullArgs

getSystemInfo :: IO SystemInfo
getSystemInfo =
  SystemInfo
    Info.os
    Info.arch
    <$> Conc.getNumCapabilities
    <*> Conc.getNumProcessors

maxOfThousand :: Int
maxOfThousand = 1000

mkTelemetryCtx :: Has (Lift IO) sig m => m TelemetryCtx
mkTelemetryCtx = sendIO $ do
  TelemetryCtx
    <$> nextRandom
    <*> (newTBMQueueIO maxOfThousand)
    <*> newEmptyTMVarIO
    <*> newTBMQueueIO maxOfThousand
    <*> newEmptyTMVarIO
    <*> atomically mkCounterRegistry
    <*> getCurrentTime

mkTelemetryRecord :: Has (Lift IO) sig m => Bool -> TelemetryCtx -> m TelemetryRecord
mkTelemetryRecord seenFatalException ctx = sendIO $ do
  cliSystemInfo <- getSystemInfo
  fossaConfig <- atomically . tryReadTMVar $ telFossaConfig ctx
  cliTelLogs <- atomically $ getItems (telLogsQ ctx)
  cliTimedDurations <- atomically $ getItems (telTimeSpent ctx)
  cliCommandArgs <- getCommandArgs
  cliUsageCounter <- atomically $ getCounterRegistry (telCounters ctx)
  finalTime <- getCurrentTime

  pure $
    TelemetryRecord
      { cliTelemetryId = telId ctx
      , cliVersion = getCurrentCliVersion
      , cliEnvironment = getCurrentCliEnvironment
      , cliResolvedConfig = uncurry TelemetryCmdConfig <$> fossaConfig
      , cliSystemInfo = cliSystemInfo
      , cliExitedFatally = seenFatalException
      , cliCommandArgs = cliCommandArgs
      , cliUsageCounter = cliUsageCounter
      , cliTelLogs = cliTelLogs
      , cliTotalDurationInSec = realToFrac $ nominalDiffTimeToSeconds $ diffUTCTime finalTime (telStartUtcTime ctx)
      , cliTimedDurations = cliTimedDurations
      , cliStartedAt = telStartUtcTime ctx
      }
  where
    getItems :: TBMQueue a -> STM [a]
    getItems q = do
      items <- replicateM maxOfThousand (tryReadTBMQueue q)
      pure $ catMaybes $ fromMaybe Nothing <$> items
