module App.Fossa.Telemetry.Utils (
  getCurrentCliEnvironment,
  getCurrentCliVersion,
  getSystemInfo,
  getCommandArgs,
  mkTelemetryCtx,
  mkTelemetryRecord,
) where

import App.Fossa.Telemetry.Types (CliEnvironment (..), SystemInfo (SystemInfo), SystemMemory (SystemMemory), TelemetryCmdConfig (TelemetryCmdConfig), TelemetryCtx (TelemetryCtx, telCounters, telFossaConfig, telId, telLogsQ, telStartUtcTime, telTimeSpent), TelemetryRecord (..))
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
import GHC.Stats qualified as Stats
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
getSystemInfo = do
  capabilities <- Conc.getNumCapabilities
  processors <- Conc.getNumProcessors
  systemMemory <- do
    rtsStatsEnabled <- Stats.getRTSStatsEnabled
    if rtsStatsEnabled
      then do
        rtsStats <- Stats.getRTSStats
        pure (SystemMemory (Stats.max_live_bytes rtsStats) (Stats.allocated_bytes rtsStats))
      else pure (SystemMemory 0 0)
  pure $
    SystemInfo
      Info.os
      Info.arch
      capabilities
      processors
      systemMemory

thousand :: Int
thousand = 1000

mkTelemetryCtx :: Has (Lift IO) sig m => m TelemetryCtx
mkTelemetryCtx = sendIO $ do
  q <- newTBMQueueIO thousand
  qTimeSpent <- newTBMQueueIO thousand
  telSink <- newEmptyTMVarIO
  telFossaConfig <- newEmptyTMVarIO
  telCounters <- atomically mkCounterRegistry
  currentTime <- getCurrentTime
  uniqueTelemetryId <- nextRandom
  pure $ TelemetryCtx uniqueTelemetryId q telSink qTimeSpent telFossaConfig telCounters currentTime

mkTelemetryRecord :: Has (Lift IO) sig m => Bool -> TelemetryCtx -> m TelemetryRecord
mkTelemetryRecord seenFatalException ctx = do
  let cliVersion = getCurrentCliVersion
      cliEnv = getCurrentCliEnvironment
  sysInfo <- sendIO getSystemInfo
  fossaConfig <- sendIO . atomically . tryReadTMVar $ telFossaConfig ctx
  items <- sendIO . atomically $ getItems (telLogsQ ctx)
  timedItems <- sendIO . atomically $ getItems (telTimeSpent ctx)
  cmdArgs <- sendIO getCommandArgs
  counters <- sendIO . atomically $ getCounterRegistry (telCounters ctx)

  finalTime <- sendIO getCurrentTime

  let timeSpendInSec :: Double = realToFrac $ nominalDiffTimeToSeconds $ diffUTCTime finalTime (telStartUtcTime ctx)

  pure $
    TelemetryRecord
      { cliTelemetryId = telId ctx
      , cliVersion = cliVersion
      , cliEnvironment = cliEnv
      , cliResolvedConfig = uncurry TelemetryCmdConfig <$> fossaConfig
      , cliSystemInfo = sysInfo
      , cliExitedFatally = seenFatalException
      , cliCommandArgs = cmdArgs
      , cliUsageCounter = counters
      , rawLogs = items
      , cliTotalDurationInSec = timeSpendInSec
      , cliTimedDurations = timedItems
      , cliStartedAt = telStartUtcTime ctx
      }
  where
    getItems :: TBMQueue a -> STM [a]
    getItems q = do
      items <- replicateM thousand (tryReadTBMQueue q)
      pure $ catMaybes $ fromMaybe Nothing <$> items
