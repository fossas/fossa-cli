{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Ficus.Analyze (
  analyzeWithFicus,
  -- Exported for use in hubble
  analyzeWithFicusMain,
  -- Exported for testing
  singletonFicusMessage,
)
where

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
  FicusScanStats (..),
  FicusSnippetScanResults (..),
 )
import App.Types (ProjectRevision (..))
import Control.Applicative ((<|>))
import Control.Carrier.Diagnostics (Diagnostics)
import Control.Concurrent.Async (async, wait)
import Control.Effect.Lift (Has, Lift, sendIO)
import Control.Monad (when)
import Data.Aeson (decode, decodeStrictText)
import Data.ByteString.Lazy qualified as BL
import Data.Conduit ((.|))
import Data.Conduit qualified as Conduit
import Data.Conduit.Combinators qualified as CC
import Data.Conduit.List qualified as CCL
import Data.Hashable (Hashable)
import Data.Maybe (isJust)
import Data.String.Conversion (ToText (toText), toString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Time (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Effect.Exec (AllowErr (Never), Command (..), ExitCode (ExitSuccess), renderCommand)
import Effect.Logger (Logger, logDebug, logInfo)
import Fossa.API.Types (ApiKey (..), ApiOpts (..))
import Path (Abs, Dir, Path, toFilePath)
import Prettyprinter (pretty)
import Srclib.Types (Locator (..), renderLocator)
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
  ficusResults <- runFicus ficusConfig
  logDebugWithTime "runFicus completed, processing results..."
  case ficusResults of
    Just results ->
      logInfo $ pretty (formatFicusScanSummary results)
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

findingToSnippetScanResult :: FicusFinding -> Maybe FicusSnippetScanResults
findingToSnippetScanResult (FicusFinding (FicusMessageData strategy payload))
  | Text.toLower strategy == "fingerprint" =
      decode (BL.fromStrict $ Text.Encoding.encodeUtf8 payload)
findingToSnippetScanResult _ = Nothing

formatFicusScanSummary :: FicusSnippetScanResults -> Text
formatFicusScanSummary results =
  let stats = ficusSnippetScanResultsStats results
      aid = ficusSnippetScanResultsAnalysisId results
   in Text.unlines
        [ "Ficus snippet scan completed successfully!"
        , "============================================================"
        , "Snippet scan summary:"
        , "  Analysis ID: " <> Text.pack (show aid)
        , "  Bucket ID: " <> Text.pack (show $ ficusSnippetScanResultsBucketId results)
        , "  Files skipped: " <> Text.pack (show $ ficusStatsSkippedFiles stats)
        , "  Total Files processed: " <> Text.pack (show $ ficusStatsProcessedFiles stats)
        , "  Unique Files processed: " <> Text.pack (show $ ficusStatsUniqueProcessedFiles stats)
        , "  Unique Files with matches found: " <> Text.pack (show $ ficusStatsUniqueMatchedFiles stats)
        , "  Unique Files with no matches found: " <> Text.pack (show $ ficusStatsUniqueUnmatchedFiles stats)
        , "  Unique Files already in our knowledgebase: " <> Text.pack (show $ ficusStatsUniqueExistingFiles stats)
        , "  Unique Files new to our knowledgebase: " <> Text.pack (show $ ficusStatsUniqueNewFiles stats)
        , "  Processing time: " <> Text.pack (show $ ficusStatsProcessingTimeSeconds stats) <> "s"
        , "============================================================"
        ]

runFicus ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  FicusConfig ->
  m (Maybe FicusSnippetScanResults)
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
    (result, exitCode, stdErrLines) <- sendIO $ withProcessWait processConfig $ \p -> do
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
    pure result
  where
    currentTimeStamp :: IO String
    currentTimeStamp = do
      now <- getCurrentTime
      pure . formatTime defaultTimeLocale "%H:%M:%S.%3q" $ now

    streamFicusOutput :: Handle -> IO (Maybe FicusSnippetScanResults)
    streamFicusOutput handle =
      Conduit.runConduit $
        CC.sourceHandle handle
          .| CC.decodeUtf8Lenient
          .| CC.linesUnbounded
          .| CCL.mapMaybe decodeStrictText
          .| CC.foldM
            ( \acc message -> do
                -- Log messages as they come, with timestamps
                timestamp <- currentTimeStamp
                case message of
                  FicusMessageError err -> do
                    putStrLn $ "[" ++ timestamp ++ "] ERROR " <> toString (displayFicusError err)
                    pure acc
                  FicusMessageDebug dbg -> do
                    putStrLn $ "[" ++ timestamp ++ "] DEBUG " <> toString (displayFicusDebug dbg)
                    pure acc
                  FicusMessageFinding finding -> do
                    putStrLn $ "[" ++ timestamp ++ "] FINDING " <> toString (displayFicusFinding finding)
                    let analysisFinding = findingToSnippetScanResult finding
                    when (isJust acc && isJust analysisFinding) $
                      putStrLn $
                        "[" ++ timestamp ++ "] ERROR " <> "Found multiple ficus analysis responses."
                    pure $ acc <|> analysisFinding
            )
            Nothing

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
    configExcludes = concatMap (\path -> ["--exclude", unGlobFilter path]) $ ficusConfigExclude ficusConfig

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
