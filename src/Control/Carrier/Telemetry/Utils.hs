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
import Control.Applicative ((<|>))
import Control.Carrier.Lift (Lift, sendIO)
import Control.Carrier.Telemetry.Types (
  CIEnvironment (..),
  CliEnvironment (..),
  LddVersionErr (LddVersionErr),
  SystemInfo (SystemInfo),
  TelemetryCmdConfig (TelemetryCmdConfig),
  TelemetryCtx (TelemetryCtx, telCounters, telFossaConfig, telId, telLogsQ, telStartUtcTime, telTimeSpentQ),
  TelemetryRecord (..),
  UnameExecErr (UnameExecErr),
 )
import Control.Concurrent.STM (STM, atomically, newEmptyTMVarIO, tryReadTMVar)
import Control.Concurrent.STM.TBMQueue (TBMQueue, newTBMQueueIO, tryReadTBMQueue)
import Control.Exception.Safe (Exception (displayException), SomeException, catchAny)
import Control.Monad (join, replicateM)
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (asum)
import Data.Functor.Extra ((<$$>))
import Data.Maybe (catMaybes)
import Data.String.Conversion (decodeUtf8, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Tracing.Instrument (
  getCounterRegistry,
  mkCounterRegistry,
 )
import Data.UUID.V4 (nextRandom)
import GHC.Conc.Sync qualified as Conc
import System.Args (getCommandArgs)
import System.Environment (lookupEnv)
import System.Info qualified as Info
import System.OsRelease (OsRelease (name, pretty_name), osRelease, parseOsRelease)
import System.Process.Typed (ExitCode (..), readProcess)

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
getSystemInfo = do
  SystemInfo
    Info.os
    Info.arch
    <$> Conc.getNumCapabilities
    <*> Conc.getNumProcessors
    -- Info.os only collects OS type, but for linux there isn't any info about distribution.
    <*> (if Info.os == "linux" then readOsRelease else pure Nothing)
    <*> (if Info.os /= "mingw32" then Just <$> readUname else pure Nothing)
    <*> if Info.os == "linux" then Just <$> findLddVersion else pure Nothing
  where
    -- read more about os-release: https://www.commandlinux.com/man-page/man5/os-release.5.html
    readOsRelease :: IO (Maybe String)
    readOsRelease = do
      releaseInfo <- fmap osRelease <$> parseOsRelease
      pure $ (pretty_name <$> releaseInfo) <|> (name <$> releaseInfo)

    readUname :: IO (Either UnameExecErr Text)
    readUname =
      first UnameExecErr
        <$> catchAny (processOutput <$> readProcess "uname -mrsv") exceptionToText

    findLddVersion :: IO (Either LddVersionErr Text)
    findLddVersion =
      first LddVersionErr
        <$> catchAny (processOutput <$> readProcess "ldd --version") exceptionToText

    processOutput :: (ExitCode, BL.ByteString, BL.ByteString) -> (Either Text Text)
    processOutput (ExitFailure code, _, stderr) =
      Left $
        "Exit code: "
          <> (toText . show $ code)
          <> " Message: "
          <> decodeUtf8 stderr
    processOutput (ExitSuccess, stdout, _) = Right $ decodeUtf8 stdout

    exceptionToText :: SomeException -> IO (Either Text a)
    exceptionToText = (pure . Left . toText . displayException)

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
      isB <- hasSomeValue "bamboo.buildKey"
      pure $ if isB then Just Bamboo else Nothing

    -- Catch all for CI systems, typically they set either CI or BUILD_ID environment
    -- variable. Example: Jenkins, GitlabCI, etc.
    isSomeCI :: IO (Maybe CIEnvironment)
    isSomeCI = do
      hasCIEnv <- hasSomeValue "CI"
      hasBuildIdEnv <- hasSomeValue "BUILD_ID"
      pure $ if (hasCIEnv || hasBuildIdEnv) then Just UnknownCI else Nothing

    lookupName :: String -> IO (Maybe Text)
    lookupName name = toText <$$> lookupEnv name

    -- True if environment variable has non empty value, otherwise False.
    hasSomeValue :: String -> IO Bool
    hasSomeValue name = do
      value <- lookupName name
      pure $ case value of
        Nothing -> False
        Just txt -> not $ Text.null txt

    -- True if environment variable represents 'true' boolean value, otherwise False.
    isTrue :: String -> IO Bool
    isTrue name = do
      value <- lookupName name
      pure $ case value of
        Nothing -> False
        Just txt -> Text.toLower txt == "true"
