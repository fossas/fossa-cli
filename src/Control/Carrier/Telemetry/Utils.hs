module Control.Carrier.Telemetry.Utils (
  getCurrentCliEnvironment,
  getCurrentCliVersion,
  getSystemInfo,
  getCommandArgs,
  mkTelemetryCtx,
  mkTelemetryRecord,
  -- for testing
  redactApiKeyFromCmdArgs,
) where

import App.Fossa.Config.Common (fossaApiKeyCmdText)
import App.Version (isDirty, versionOrBranch)
import Control.Algebra (Has)
import Control.Carrier.Lift (Lift, sendIO)
import Control.Carrier.Telemetry.Types (
  CliEnvironment (..),
  SystemInfo (SystemInfo),
  TelemetryCmdConfig (TelemetryCmdConfig),
  TelemetryCtx (TelemetryCtx, telCounters, telFossaConfig, telId, telLogsQ, telStartUtcTime, telTimeSpentQ),
  TelemetryRecord (..),
 )
import Control.Concurrent.STM (STM, atomically, newEmptyTMVarIO, tryReadTMVar)
import Control.Concurrent.STM.TBMQueue (TBMQueue, newTBMQueueIO, tryReadTBMQueue)
import Control.Monad (join, replicateM)
import Data.List (elemIndex)
import Data.Maybe (catMaybes)
import Data.String.Conversion (toText)
import Data.Text (Text, isPrefixOf)
import Data.Time (diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Tracing.Instrument (
  getCounterRegistry,
  mkCounterRegistry,
 )
import Data.UUID.V4 (nextRandom)
import GHC.Conc.Sync qualified as Conc
import GHC.Environment qualified as Environment
import System.Info qualified as Info

maxQueueSize :: Int
maxQueueSize = 1000

mkTelemetryCtx :: Has (Lift IO) sig m => m TelemetryCtx
mkTelemetryCtx = sendIO $ do
  TelemetryCtx
    <$> nextRandom
    <*> newTBMQueueIO maxQueueSize
    <*> newEmptyTMVarIO
    <*> newTBMQueueIO maxQueueSize
    <*> newEmptyTMVarIO
    <*> atomically mkCounterRegistry
    <*> getCurrentTime

mkTelemetryRecord :: Has (Lift IO) sig m => Bool -> TelemetryCtx -> m TelemetryRecord
mkTelemetryRecord seenFatalException ctx = sendIO $ do
  cliCommandArgs <- getCommandArgs
  fossaConfig <- atomically . tryReadTMVar $ telFossaConfig ctx
  cliSystemInfo <- getSystemInfo
  cliTelLogs <- atomically $ getItems (telLogsQ ctx)
  cliTimedDurations <- atomically $ getItems (telTimeSpentQ ctx)
  cliUsageCounter <- atomically $ getCounterRegistry (telCounters ctx)
  finalTime <- getCurrentTime

  pure $
    TelemetryRecord
      { cliCommandArgs = cliCommandArgs
      , cliEnvironment = getCurrentCliEnvironment
      , cliExitedFatally = seenFatalException
      , cliResolvedConfig = uncurry TelemetryCmdConfig <$> fossaConfig
      , cliStartedAt = telStartUtcTime ctx
      , cliSystemInfo = cliSystemInfo
      , cliTelLogs = cliTelLogs
      , cliTelemetryId = telId ctx
      , cliTimedDurations = cliTimedDurations
      , cliTotalDurationInSec = realToFrac $ nominalDiffTimeToSeconds $ diffUTCTime finalTime (telStartUtcTime ctx)
      , cliUsageCounter = cliUsageCounter
      , cliVersion = getCurrentCliVersion
      }
  where
    getItems :: TBMQueue a -> STM [a]
    getItems q = catMaybes <$> replicateM maxQueueSize (join <$> tryReadTBMQueue q)

getCurrentCliVersion :: Text
getCurrentCliVersion = versionOrBranch

getCurrentCliEnvironment :: CliEnvironment
getCurrentCliEnvironment =
  if isDirty
    then CliDevelopmentEnvironment
    else CliProductionEnvironment

getCommandArgs :: IO [Text]
getCommandArgs = redactApiKeyFromCmdArgs <$> (map toText <$> Environment.getFullArgs)

-- | Redacts Api Key from raw command args.
redactApiKeyFromCmdArgs :: [Text] -> [Text]
redactApiKeyFromCmdArgs = redactApiKeyWhenDirectlySupplied . redactApiKeyWhenPassedAsArg
  where
    -- When provided with --fossa-api-key=someKey
    redactApiKeyWhenDirectlySupplied :: [Text] -> [Text]
    redactApiKeyWhenDirectlySupplied =
      map
        ( \arg ->
            if ("--" <> fossaApiKeyCmdText <> "=") `isPrefixOf` arg
              then ("--" <> fossaApiKeyCmdText <> "=<REDACTED>")
              else arg
        )

    -- When provided with --fossa-api-key someKey
    redactApiKeyWhenPassedAsArg :: [Text] -> [Text]
    redactApiKeyWhenPassedAsArg args =
      case elemIndex ("--" <> fossaApiKeyCmdText) args of
        Nothing -> args
        Just i -> replaceAtWith args (i + 1) "<REDACTED>"

    replaceAtWith :: [Text] -> Int -> Text -> [Text]
    replaceAtWith args replaceAt replaceWith = case splitAt replaceAt args of
      (start, _ : end) -> start ++ replaceWith : end
      _ -> args

getSystemInfo :: IO SystemInfo
getSystemInfo =
  SystemInfo
    Info.os
    Info.arch
    <$> Conc.getNumCapabilities
    <*> Conc.getNumProcessors
