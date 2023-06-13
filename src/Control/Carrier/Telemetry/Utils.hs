{-# LANGUAGE ViewPatterns #-}
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
  SystemInfo (SystemInfo),
  TelemetryCmdConfig (TelemetryCmdConfig),
  TelemetryCtx (TelemetryCtx, telCounters, telFossaConfig, telId, telLogsQ, telStartUtcTime, telTimeSpentQ),
  TelemetryRecord (..),
 )
import Control.Concurrent.STM (STM, atomically, newEmptyTMVarIO, tryReadTMVar)
import Control.Concurrent.STM.TBMQueue (TBMQueue, newTBMQueueIO, tryReadTBMQueue)
import Control.Effect.Exception (SomeException)
import Control.Exception (try)
import Control.Monad (join, replicateM)
import Data.ByteString qualified as BS
import Data.Foldable (asum)
import Data.Functor.Extra ((<$$>))
import Data.Map qualified as Map
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
    <*> if Info.os == "linux" then readOsRelease else pure Nothing
  where
    readFileStrict :: FilePath -> IO (Either SomeException Text)
    readFileStrict = try . fmap decodeUtf8 . BS.readFile

    readOsRelease :: IO (Maybe Text)
    readOsRelease = do
      -- read more about os-release: https://www.commandlinux.com/man-page/man5/os-release.5.html
      rawOsRelease <- readFileStrict "/etc/os-release" <|> readFileStrict "/usr/lib/os-release"
      pure $ do
        osReleaseMap <- parseOsRelease <$> either (const Nothing) Just rawOsRelease
        Map.lookup "PRETTY_NAME" osReleaseMap <|> Map.lookup "NAME" osReleaseMap

-- |Optimistically parse a piece of text matching the syntax of os-release:
--
-- NAME="name"
-- VERSION="version"
--
-- If the parse fails for some reason, there is no warning.
-- This is intended for telemetry, if we fail to find a value it isn't supposed to be reported or recovered from.
parseOsRelease :: Text -> Map.Map Text Text
parseOsRelease = Map.fromList . map splitOnEqual . Text.lines 
  where splitOnEqual :: Text -> (Text, Text)
        splitOnEqual (Text.span (== '=') -> (pre, post)) = (pre, Text.dropWhile (== '=') post)



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
