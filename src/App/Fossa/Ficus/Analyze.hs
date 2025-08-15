{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Ficus.Analyze (
  analyzeWithFicus,
  -- Exported for use in hubble
  analyzeWithFicusMain,
  -- Exported for testing
  singletonFicusMessage,
) where

import App.Fossa.EmbeddedBinary (BinaryPaths, toPath, withFicusBinary)
import App.Fossa.Ficus.Types (
  FicusAllFlag (..),
  FicusAnalysisFlag (..),
  FicusConfig (..),
  FicusDebug (..),
  FicusError (..),
  FicusFinding (..),
  FicusMessage (..),
  FicusMessageData (..),
  FicusMessages (..),
  FicusPerStrategyFlag (..),
  FicusSnippetScanResults (..),
 )
import App.Types (ProjectRevision (..))
import Control.Carrier.Diagnostics (Diagnostics)
import Control.Effect.Lift (Has, Lift, sendIO)
import Effect.Logger (Logger, logDebug, logInfo, logWarn)
import Prettyprinter (pretty)

import Data.Time (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import Data.Aeson (Object, decode, (.:))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (fold)
import Control.Monad (foldM)
import Data.Hashable (Hashable)
import Data.Maybe (mapMaybe)
import Data.String.Conversion (ToText (toText), toString)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Effect.Exec (AllowErr (Never), Command (..), Exec, renderCommand)
import App.Fossa.EmbeddedBinary (withFicusBinary)
import System.Process.Typed (
  createPipe,
  getStdout,
  getStderr,
  proc,
  setEnv,
  setStderr,
  setStdout,
  setWorkingDir,
  withProcessWait,
 )
import System.IO (Handle, hGetLine, hIsEOF, hReady)
import Control.Exception (try, IOException)
import Control.Concurrent.Async (async, wait)
import Effect.ReadFS (ReadFS)
import Fossa.API.Types (ApiKey (..), ApiOpts (..))

import Path (Abs, Dir, File, Path, toFilePath)
import Srclib.Types (Locator (..), renderLocator)
import Text.URI (render)
import Text.URI.Builder (PathComponent (PathComponent), TrailingSlash (TrailingSlash), setPath)
import Types (GlobFilter (..), LicenseScanPathFilters (..))
import Prelude

newtype CustomLicensePath = CustomLicensePath {unCustomLicensePath :: Text}
  deriving (Eq, Ord, Show, Hashable)
newtype CustomLicenseTitle = CustomLicenseTitle {unCustomLicenseTitle :: Text}
  deriving (Eq, Ord, Show, Hashable)

-- Helper function to log with timestamp
logDebugWithTime :: (Has Logger sig m, Has (Lift IO) sig m) => Text -> m ()
logDebugWithTime msg = do
  now <- sendIO getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%H:%M:%S.%3q" now
  logDebug $ "[" <> pretty timestamp <> "] " <> pretty msg

-- | scan rootDir with Ficus, using the given GrepOptions. This is the main entry point to this module
analyzeWithFicus ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has Logger sig m
  ) =>
  Path Abs Dir ->
  Maybe ApiOpts ->
  ProjectRevision ->
  Maybe LicenseScanPathFilters ->
  m (Maybe FicusSnippetScanResults)
analyzeWithFicus rootDir apiOpts revision filters = do
  analyzeWithFicusMain rootDir apiOpts revision filters

analyzeWithFicusMain ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has Logger sig m
  ) =>
  Path Abs Dir ->
  Maybe ApiOpts ->
  ProjectRevision ->
  Maybe LicenseScanPathFilters ->
  m (Maybe FicusSnippetScanResults)
analyzeWithFicusMain rootDir apiOpts revision filters = do
  logDebugWithTime "Preparing Ficus analysis configuration..."
  messages <- runFicus ficusConfig
  logDebugWithTime "runFicus completed, processing results..."
  let ficusResults = ficusMessagesToFicusSnippetScanResults messages
  case ficusResults of
    Just results -> do
      logInfo $ "Ficus analysis completed successfully with analysis ID: " <> pretty (ficusSnippetScanResultsAnalysisId results)
      logDebug $ "Found " <> pretty (length $ ficusMessageFindings messages) <> " findings from Ficus"
    Nothing -> logInfo "Ficus analysis completed but no fingerprint findings were found"
  pure ficusResults
  where
    ficusConfig =
      FicusConfig
        { ficusConfigRootDir = rootDir
        , ficusConfigExclude = maybe [] licenseScanPathFiltersExclude filters
        , ficusConfigEndpoint = apiOptsUri =<< apiOpts
        , ficusConfigSecret = apiOptsApiKey <$> apiOpts
        , ficusConfigRevision = revision
        , ficusConfigFlags = [All $ FicusAllFlag SkipHiddenFiles, All $ FicusAllFlag Gitignore]
        }

ficusMessagesToFicusSnippetScanResults :: FicusMessages -> Maybe FicusSnippetScanResults
ficusMessagesToFicusSnippetScanResults messages =
  let isFingerprintStrategy :: FicusFinding -> Bool
      isFingerprintStrategy (FicusFinding (FicusMessageData strategy _)) =
        Text.toLower strategy == "fingerprint"

      extractAnalysisId :: FicusFinding -> Maybe Int
      extractAnalysisId (FicusFinding (FicusMessageData _ payload)) =
        case decode (BL.fromStrict $ Text.Encoding.encodeUtf8 payload) :: Maybe Object of
          Just obj -> parseMaybe (.: "analysis_id") obj
          Nothing -> Nothing

      matchingFinding = filter isFingerprintStrategy (ficusMessageFindings messages)
      analysisId = mapMaybe extractAnalysisId matchingFinding
   in case analysisId of
        (aid : _) -> Just $ FicusSnippetScanResults{ficusSnippetScanResultsAnalysisId = aid}
        [] -> Nothing


runFicus ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  FicusConfig ->
  m FicusMessages
runFicus ficusConfig = do
  logDebugWithTime "About to extract Ficus binary..."
  withFicusBinary $ \bin -> do
    logDebugWithTime "Ficus binary extracted, building command..."
    cmd <- ficusCommand ficusConfig bin
    logDebugWithTime "Executing ficus (streaming)"
    logDebug $ "Ficus command: " <> pretty (renderCommand cmd)
    logDebug $ "Working directory: " <> pretty (toFilePath $ ficusConfigRootDir ficusConfig)

    logDebugWithTime "Creating process configuration..."
    let processConfig =
          setWorkingDir (toFilePath $ ficusConfigRootDir ficusConfig) $
          setStdout createPipe $
          setStderr createPipe $
          proc (toString $ cmdName cmd) (map toString $ cmdArgs cmd)

    logInfo $ "Running Ficus analysis on " <> pretty (toFilePath $ ficusConfigRootDir ficusConfig)
    logDebugWithTime "Starting Ficus process..."
    messages <- sendIO $ withProcessWait processConfig $ \p -> do
      getCurrentTime >>= \now -> putStrLn $ "[TIMING " ++ formatTime defaultTimeLocale "%H:%M:%S.%3q" now ++ "] Ficus process started, beginning stream processing..."
      let stdoutHandle = getStdout p
      let stderrHandle = getStderr p
      -- Start async reading of stderr to prevent blocking
      stderrAsync <- async $ consumeStderr stderrHandle
      -- Read stdout in the main thread
      result <- streamFicusOutput stdoutHandle
      -- Wait for stderr to finish
      _ <- wait stderrAsync
      pure result

    logDebug $
      "Ficus returned "
        <> pretty (length $ ficusMessageErrors messages)
        <> " errors, "
        <> pretty (length $ ficusMessageDebugs messages)
        <> " debug messages, "
        <> pretty (length $ ficusMessageFindings messages)
        <> " findings"

    pure messages
  where
    streamFicusOutput :: Handle -> IO FicusMessages
    streamFicusOutput handle = do
      let loop acc = do
            eof <- hIsEOF handle
            if eof
              then do
                putStrLn "[DEBUG] Reached end of Ficus output stream"
                pure acc
              else do
                line <- hGetLine handle
                let lineBS = BL.fromStrict $ Text.Encoding.encodeUtf8 $ Text.pack line
                case decode lineBS of
                  Just message -> do
                    -- Log messages in real-time with timestamps
                    now <- getCurrentTime
                    let timestamp = formatTime defaultTimeLocale "%H:%M:%S.%3q" now
                    case message of
                      FicusMessageError err ->
                        putStrLn $ "[" ++ timestamp ++ "] ERROR " <> Text.unpack (displayFicusError err)
                      FicusMessageDebug dbg ->
                        putStrLn $ "[" ++ timestamp ++ "] DEBUG " <> Text.unpack (displayFicusDebug dbg)
                      FicusMessageFinding finding ->
                        putStrLn $ "[" ++ timestamp ++ "] FINDING " <> Text.unpack (displayFicusFinding finding)
                    let newMessage = singletonFicusMessage message
                    loop (acc <> newMessage)
                  Nothing -> do
                    -- Skip invalid JSON silently to avoid clutter
                    loop acc
      loop mempty

    consumeStderr :: Handle -> IO ()
    consumeStderr handle = do
      let loop = do
            eof <- hIsEOF handle
            if eof
              then pure ()
              else do
                _ <- hGetLine handle  -- Consume stderr silently
                loop
      loop

    displayFicusDebug :: FicusDebug -> Text
    displayFicusDebug (FicusDebug FicusMessageData{..}) = ficusMessageDataStrategy <> ": " <> ficusMessageDataPayload
    displayFicusError :: FicusError -> Text
    displayFicusError (FicusError FicusMessageData{..}) = ficusMessageDataStrategy <> ": " <> ficusMessageDataPayload
    displayFicusFinding :: FicusFinding -> Text
    displayFicusFinding (FicusFinding FicusMessageData{..}) = ficusMessageDataStrategy <> ": " <> ficusMessageDataPayload

-- Run Ficus, passing config-based args as configuration.
-- Caveat! This hard-codes some flags currently which may later need to be set on a strategy-by-strategy basis.
ficusCommand :: Has Diagnostics sig m => FicusConfig -> BinaryPaths -> m Command
ficusCommand ficusConfig bin = do
  endpoint <- case ficusConfigEndpoint ficusConfig of
    Just baseUri -> do
      proxyUri <- setPath [PathComponent "api", PathComponent "proxy", PathComponent "analysis"] (TrailingSlash False) baseUri
      pure $ render proxyUri
    Nothing -> pure "https://app.fossa.com/api/proxy/analysis"
  pure $
    Command
      { cmdName = toText $ toPath bin
      , cmdArgs = configArgs endpoint
      , cmdAllowErr = Never
      }
  where
    configArgs endpoint = ["analyze", "--secret", secret, "--endpoint", endpoint, "--locator", locator, "--set", "all:skip-hidden-files", "--set", "all:gitignore", "--exclude", ".git", "--exclude", ".git/**" ] ++ configExcludes ++ [targetDir]
    targetDir = toText $ toFilePath $ ficusConfigRootDir ficusConfig
    secret = maybe "" (toText . unApiKey) $ ficusConfigSecret ficusConfig
    locator = renderLocator $ Locator "custom" (projectName $ ficusConfigRevision ficusConfig) (Just $ projectRevision $ ficusConfigRevision ficusConfig)
    configExcludes = unGlobFilter <$> ficusConfigExclude ficusConfig




-- add a FicusMessage to the corresponding entry of an empty FicusMessages
singletonFicusMessage :: FicusMessage -> FicusMessages
singletonFicusMessage message = case message of
  FicusMessageFinding msg -> mempty{ficusMessageFindings = [msg]}
  FicusMessageDebug msg -> mempty{ficusMessageDebugs = [msg]}
  FicusMessageError msg -> mempty{ficusMessageErrors = [msg]}
