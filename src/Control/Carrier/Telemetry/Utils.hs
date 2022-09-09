module Control.Carrier.Telemetry.Utils (
  getCurrentCliEnvironment,
  getCurrentCliVersion,
  getSystemInfo,
  mkTelemetryCtx,
  mkTelemetryRecord,
  -- for testing
) where

import App.Version (isDirty, versionOrBranch)
import Control.Algebra (Has)
import Control.Carrier.Lift (Lift, sendIO)
import Control.Carrier.Telemetry.Types (
  CliEnvironment (..),
  SystemInfo (SystemInfo),
  TelemetryCmdConfig (TelemetryCmdConfig),
  TelemetryCtx (TelemetryCtx, telCounters, telFossaConfig, telId, telLogsQ, telStartUtcTime, telTimeSpentQ),
  TelemetryRecord (..), CIEnvironment(..),
 )
import Control.Concurrent.STM (STM, atomically, newEmptyTMVarIO, tryReadTMVar)
import Control.Concurrent.STM.TBMQueue (TBMQueue, newTBMQueueIO, tryReadTBMQueue)
import Control.Monad (join, replicateM)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Time (diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Tracing.Instrument (
  getCounterRegistry,
  mkCounterRegistry,
 )
import Data.UUID.V4 (nextRandom)
import GHC.Conc.Sync qualified as Conc
import System.Args (getCommandArgs)
import System.Info qualified as Info
import System.Environment (lookupEnv)
import qualified Data.Text as Text
import Data.String.Conversion (toText)
import Data.Functor.Extra ((<$$>))
import Data.Foldable (asum)

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
  ciEnv <- lookupCIEnvironment

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
      , cliCIEnvironment = ciEnv
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

getSystemInfo :: IO SystemInfo
getSystemInfo =
  SystemInfo
    Info.os
    Info.arch
    <$> Conc.getNumCapabilities
    <*> Conc.getNumProcessors

lookupCIEnvironment :: IO (Maybe CIEnvironment)
lookupCIEnvironment = do
  asum
    <$> sequence
      [ isGithubAction
      , isAzurePipeline
      , isBamboo
      , isSomeCI
      ]
  where
    -- Ref: https://help.github.com/en/actions/configuring-and-managing-workflows/using-environment-variables#default-environment-variables
    isGithubAction :: IO (Maybe CIEnvironment)
    isGithubAction = do
      isGH <- isTrue "GITHUB_ACTIONS"
      pure $ if isGH then Just GithubAction else Nothing

    -- Ref: https://docs.microsoft.com/en-us/azure/devops/pipelines/build/variables?view=azure-devops&viewFallbackFrom=vsts&tabs=yaml#system-variables
    isAzurePipeline :: IO (Maybe CIEnvironment)
    isAzurePipeline = do
      isAzure <- isTrue "TF_BUILD"
      pure $ if isAzure then Just AzurePipeline else Nothing

    -- Ref: https://confluence.atlassian.com/bamboo/bamboo-variables-289277087.html#Bamboovariables-Build-specificvariables
    isBamboo :: IO (Maybe CIEnvironment)
    isBamboo = do
      isGH <- hasSomeValue "bamboo.buildKey"
      pure $ if isGH then Just Bamboo else Nothing

   -- Catch all for CI systems, typically they set either CI or BUILD_ID environment
   -- variable. Example: Jenkins, GitlabCI, etc.
    isSomeCI :: IO (Maybe CIEnvironment)
    isSomeCI = do
      hasCIEnv <- hasSomeValue "CI"
      hasBuildIdEnv <- hasSomeValue "BUILD_ID"
      pure $ if (hasCIEnv || hasBuildIdEnv) then Just UnknownCI else Nothing

    lookupName :: String -> IO (Maybe Text)
    lookupName name = toText <$$> lookupEnv name

    -- | True if environment variable has non empty value, otherwise False.
    hasSomeValue :: String -> IO Bool
    hasSomeValue name = do
      value <- lookupName name
      pure $ case value of
        Nothing -> False
        Just txt -> not $ Text.null txt

    -- | True if environment variable represents 'true' boolean value, otherwise False.
    isTrue :: String -> IO Bool
    isTrue name = do
      value <- lookupName name
      pure $ case value of
        Nothing -> False
        Just txt -> Text.toLower txt == "true"