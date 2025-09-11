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
import Effect.Logger (Logger, logDebug, logInfo)
import Prettyprinter (pretty)

import Data.Time (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

import Control.Concurrent.Async (async, wait)
import Data.Aeson (Object, decode, (.:))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy qualified as BL
import Data.Hashable (Hashable)
import Data.Maybe (mapMaybe)
import Data.String.Conversion (ToText (toText), toString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Effect.Exec (AllowErr (Never), Command (..), ExitCode (ExitSuccess), renderCommand)
import Fossa.API.Types (ApiKey (..), ApiOpts (..))
import System.IO (Handle, hGetLine, hIsEOF)
import System.Process.Typed (
  createPipe,
  getStderr,
  getStdout,
  proc,
  setStderr,
  setStdout,
  setWorkingDir,
  waitExitCode,
  withProcessWait,
 )

import Path (Abs, Dir, Path, toFilePath)
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
  , Has Logger sig m
  ) =>
  Path Abs Dir ->
  Maybe ApiOpts ->
  ProjectRevision ->
  Maybe LicenseScanPathFilters ->
  Maybe Int ->
  m (Maybe FicusSnippetScanResults)
analyzeWithFicus rootDir apiOpts revision filters snippetScanRetentionDays = do
  analyzeWithFicusMain rootDir apiOpts revision filters snippetScanRetentionDays

analyzeWithFicusMain ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  Path Abs Dir ->
  Maybe ApiOpts ->
  ProjectRevision ->
  Maybe LicenseScanPathFilters ->
  Maybe Int ->
  m (Maybe FicusSnippetScanResults)
analyzeWithFicusMain rootDir apiOpts revision filters snippetScanRetentionDays = do
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
        , ficusConfigSnippetScanRetentionDays = snippetScanRetentionDays
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
    logDebug $ "Working directory: " <> pretty (toFilePath $ ficusConfigRootDir ficusConfig)

    logDebugWithTime "Creating process configuration..."
    let processConfig =
          setWorkingDir (toFilePath $ ficusConfigRootDir ficusConfig) $
            setStdout createPipe $
              setStderr createPipe $
                proc (toString $ cmdName cmd) (map toString $ cmdArgs cmd)

    logInfo $ "Running Ficus analysis on " <> pretty (toFilePath $ ficusConfigRootDir ficusConfig)
    logDebugWithTime "Starting Ficus process..."
    (messages, exitCode, stdErrLines) <- sendIO $ withProcessWait processConfig $ \p -> do
      getCurrentTime >>= \now -> putStrLn $ "[TIMING " ++ formatTime defaultTimeLocale "%H:%M:%S.%3q" now ++ "] Ficus process started, beginning stream processing..."
      let stdoutHandle = getStdout p
      let stderrHandle = getStderr p
      -- Start async reading of stderr to prevent blocking
      stderrAsync <- async $ consumeStderr stderrHandle
      -- Read stdout in the main thread
      result <- streamFicusOutput stdoutHandle
      -- Wait for stderr to finish
      stdErrLines <- wait stderrAsync
      exitCode <- waitExitCode p
      pure (result, exitCode, stdErrLines)

    if exitCode /= ExitSuccess
      then do
        logInfo $
          "[Ficus] Ficus process returned non-zero exit code. Printing last 50 lines of stderr: " <> pretty (show exitCode)
        logInfo "\n==== BEGIN Ficus STDERR ====\n"
        logInfo $ pretty (Text.unlines stdErrLines)
        logInfo "\n==== END Ficus STDERR ====\n"
      else logInfo "[Ficus] Ficus exited successfully"
    logDebug $
      "[Ficus] Ficus returned "
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
                let lineBS = BL.fromStrict $ Text.Encoding.encodeUtf8 $ toText line
                case decode lineBS of
                  Just message -> do
                    -- Log messages as they come, with timestamps
                    now <- getCurrentTime
                    let timestamp = formatTime defaultTimeLocale "%H:%M:%S.%3q" now
                    case message of
                      FicusMessageError err ->
                        putStrLn $ "[" ++ timestamp ++ "] ERROR " <> toString (displayFicusError err)
                      FicusMessageDebug dbg ->
                        putStrLn $ "[" ++ timestamp ++ "] DEBUG " <> toString (displayFicusDebug dbg)
                      FicusMessageFinding finding ->
                        putStrLn $ "[" ++ timestamp ++ "] FINDING " <> toString (displayFicusFinding finding)
                    let newMessage = singletonFicusMessage message
                    loop (acc <> newMessage)
                  Nothing -> do
                    loop acc
      loop mempty

    consumeStderr :: Handle -> IO [Text]
    consumeStderr handle = do
      let loop acc (count :: Int) = do
            eof <- hIsEOF handle
            if eof
              then pure (reverse acc) -- Reverse at the end to get correct order
              else do
                line <- hGetLine handle -- output stderr
                now <- getCurrentTime
                let timestamp = formatTime defaultTimeLocale "%H:%M:%S.%3q" now
                let msg = "[" ++ timestamp ++ "] STDERR " <> line
                -- Keep at most the last 50 lines of stderr
                -- I came up with 50 lines by looking at a few different error traces and making
                -- sure that we captured all of the relevant error output, and then going a bit higher
                -- to make sure that we didn't miss anything. I'd rather capture a bit too much than not enough.
                -- Use cons (:) for O(1) prepending, track count explicitly for O(1) truncation
                let newAcc =
                      if count >= 50
                        then take 50 (toText msg : acc)
                        else toText msg : acc
                let newCount = min (count + 1) 50
                loop newAcc newCount
      loop [] 0

    displayFicusDebug :: FicusDebug -> Text
    displayFicusDebug (FicusDebug FicusMessageData{..}) = ficusMessageDataStrategy <> ": " <> ficusMessageDataPayload
    displayFicusError :: FicusError -> Text
    displayFicusError (FicusError FicusMessageData{..}) = ficusMessageDataStrategy <> ": " <> ficusMessageDataPayload
    displayFicusFinding :: FicusFinding -> Text
    displayFicusFinding (FicusFinding FicusMessageData{..}) = ficusMessageDataStrategy <> ": " <> ficusMessageDataPayload

-- Run Ficus, passing config-based args as configuration.
-- Caveat! This hard-codes some flags currently which may later need to be set on a strategy-by-strategy basis.
ficusCommand :: (Has Diagnostics sig m, Has Logger sig m) => FicusConfig -> BinaryPaths -> m Command
ficusCommand ficusConfig bin = do
  endpoint <- case ficusConfigEndpoint ficusConfig of
    Just baseUri -> do
      proxyUri <- setPath [PathComponent "api", PathComponent "proxy", PathComponent "analysis"] (TrailingSlash False) baseUri
      pure $ render proxyUri
    Nothing -> pure "https://app.fossa.com/api/proxy/analysis"
  let cmd =
        Command
          { cmdName = toText $ toPath bin
          , cmdArgs = configArgs endpoint
          , cmdAllowErr = Never
          }
  logDebug $ "Ficus command: " <> pretty (maskApiKeyInCommand $ renderCommand cmd)
  pure cmd
  where
    snippetScanRetentionDays = ficusConfigSnippetScanRetentionDays ficusConfig
    configArgs endpoint = ["analyze", "--secret", secret, "--endpoint", endpoint, "--locator", locator, "--set", "all:skip-hidden-files", "--set", "all:gitignore", "--exclude", ".git", "--exclude", ".git/**"] ++ configExcludes ++ maybe [] (\days -> ["--snippet-scan-retention-days", toText days]) snippetScanRetentionDays ++ [targetDir]
    targetDir = toText $ toFilePath $ ficusConfigRootDir ficusConfig
    secret = maybe "" (toText . unApiKey) $ ficusConfigSecret ficusConfig
    locator = renderLocator $ Locator "custom" (projectName $ ficusConfigRevision ficusConfig) (Just $ projectRevision $ ficusConfigRevision ficusConfig)
    configExcludes = unGlobFilter <$> ficusConfigExclude ficusConfig

    maskApiKeyInCommand :: Text -> Text
    maskApiKeyInCommand cmdText =
      case Text.splitOn " --secret " cmdText of
        [before, after] ->
          case Text.words after of
            (_ : rest) ->
              before
                <> " --secret "
                <> "******"
                <> if null rest then "" else " " <> Text.unwords rest
            [] -> cmdText
        _ -> cmdText

-- add a FicusMessage to the corresponding entry of an empty FicusMessages
singletonFicusMessage :: FicusMessage -> FicusMessages
singletonFicusMessage message = case message of
  FicusMessageFinding msg -> mempty{ficusMessageFindings = [msg]}
  FicusMessageDebug msg -> mempty{ficusMessageDebugs = [msg]}
  FicusMessageError msg -> mempty{ficusMessageErrors = [msg]}
