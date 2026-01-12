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
  FicusAnalysisResults (..),
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
  FicusStrategy (FicusStrategySnippetScan, FicusStrategyVendetta),
  FicusVendoredDependency (..),
  FicusVendoredDependencyScanResults (..),
 )
import App.Types (ProjectRevision (..))
import Control.Applicative ((<|>))
import Control.Carrier.Diagnostics (Diagnostics)
import Control.Concurrent.Async (async, wait)
import Control.Effect.Lift (Has, Lift, sendIO)
import Control.Monad (when)
import Data.Aeson (decode, decodeStrictText)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Conduit ((.|))
import Data.Conduit qualified as Conduit
import Data.Conduit.Combinators qualified as CC
import Data.Conduit.List qualified as CCL
import Data.Foldable (traverse_)
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
import Srclib.Types (Locator (..), SourceUnit (..), SourceUnitBuild (..), SourceUnitDependency (..), renderLocator, textToOriginPath)
import System.FilePath ((</>))
import System.IO (Handle, IOMode (WriteMode), hClose, hGetLine, hIsEOF, hPutStrLn, openFile, stderr)
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
import Text.Printf (printf)
import Text.URI (render)
import Text.URI.Builder (PathComponent (PathComponent), TrailingSlash (TrailingSlash), setPath)
import Types (GlobFilter (..), GraphBreadth (..), LicenseScanPathFilters (..))
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
  [FicusStrategy] ->
  Maybe LicenseScanPathFilters ->
  Maybe Int ->
  Maybe FilePath -> -- Debug directory (if enabled)
  m (Maybe FicusAnalysisResults)
analyzeWithFicus rootDir apiOpts revision strategies filters snippetScanRetentionDays maybeDebugDir = do
  Just <$> analyzeWithFicusMain rootDir apiOpts revision strategies filters snippetScanRetentionDays maybeDebugDir

analyzeWithFicusMain ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  Path Abs Dir ->
  Maybe ApiOpts ->
  ProjectRevision ->
  [FicusStrategy] ->
  Maybe LicenseScanPathFilters ->
  Maybe Int ->
  Maybe FilePath -> -- Debug directory (if enabled)
  m FicusAnalysisResults
analyzeWithFicusMain rootDir apiOpts revision strategies filters snippetScanRetentionDays maybeDebugDir = do
  logDebugWithTime "Preparing Ficus analysis configuration..."
  ficusResults <- runFicus maybeDebugDir ficusConfig
  logDebugWithTime "runFicus completed, processing results..."
  when (FicusStrategySnippetScan `elem` strategies) $
    case snippetScanResults ficusResults of
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
        , ficusConfigStrategies = strategies
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
        , ""
        , "============================================================"
        , "Snippet scan summary:"
        , "  Analysis ID: " <> toText (show aid)
        , "  Bucket ID: " <> toText (show $ ficusSnippetScanResultsBucketId results)
        , "  Files skipped: " <> toText (show $ ficusStatsSkippedFiles stats)
        , "  Total Files processed: " <> toText (show $ ficusStatsProcessedFiles stats)
        , "  Unique Files processed: " <> toText (show $ ficusStatsUniqueProcessedFiles stats)
        , "  Unique Files with matches found: " <> toText (show $ ficusStatsUniqueMatchedFiles stats)
        , "  Unique Files with no matches found: " <> toText (show $ ficusStatsUniqueUnmatchedFiles stats)
        , "  Unique Files already in our knowledge base: " <> toText (show $ ficusStatsUniqueExistingFiles stats)
        , "  Unique Files new to our knowledge base: " <> toText (show $ ficusStatsUniqueNewFiles stats)
        , "  Processing time: " <> formatProcessingTime (ficusStatsProcessingTimeSeconds stats) <> "s"
        , "============================================================"
        , "See the docs for an explanation of this summary: https://github.com/fossas/fossa-cli/blob/master/docs/features/snippet-scanning.md#the-snippet-scan-summary"
        ]
  where
    -- Format the processing time as a string with 3 decimal places
    formatProcessingTime :: Double -> Text
    formatProcessingTime seconds = toText (printf "%.3f" seconds :: String)

findingToVendoredDependency :: FicusFinding -> Maybe FicusVendoredDependency
findingToVendoredDependency (FicusFinding (FicusMessageData strategy payload))
  | Text.toLower strategy == "vendetta" =
      decode (BL.fromStrict $ Text.Encoding.encodeUtf8 payload)
findingToVendoredDependency _ = Nothing

vendoredDepsToSourceUnit :: [FicusVendoredDependency] -> SourceUnit
vendoredDepsToSourceUnit deps =
  SourceUnit
    { sourceUnitName = "ficus-vendored-dependencies"
    , sourceUnitType = "ficus-vendored"
    , sourceUnitManifest = "ficus-vendored-dependencies"
    , sourceUnitBuild =
        Just $
          SourceUnitBuild
            { buildArtifact = "default"
            , buildSucceeded = True
            , buildImports = locators
            , buildDependencies = dependencies
            }
    , sourceUnitGraphBreadth = Complete
    , sourceUnitNoticeFiles = []
    , sourceUnitOriginPaths = map (textToOriginPath . ficusVendoredDependencyPath) deps
    , sourceUnitLabels = Nothing
    , additionalData = Nothing
    }
  where
    locators :: [Locator]
    locators = map vendoredDepToLocator deps

    dependencies :: [SourceUnitDependency]
    dependencies = map vendoredDepToSourceUnitDependency deps

    vendoredDepToLocator :: FicusVendoredDependency -> Locator
    vendoredDepToLocator dep =
      Locator
        { locatorFetcher = ficusVendoredDependencyEcosystem dep
        , locatorProject = ficusVendoredDependencyName dep
        , locatorRevision = ficusVendoredDependencyVersion dep
        }

    vendoredDepToSourceUnitDependency :: FicusVendoredDependency -> SourceUnitDependency
    vendoredDepToSourceUnitDependency dep =
      SourceUnitDependency
        { sourceDepLocator = vendoredDepToLocator dep
        , sourceDepImports = []
        , sourceDepData =
            Aeson.object
              [ "vendored"
                  Aeson..= [ Aeson.object
                               [ "type" Aeson..= ("directory" :: Text)
                               , "path" Aeson..= ficusVendoredDependencyPath dep
                               ]
                           ]
              ]
        }

runFicus ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  Maybe FilePath ->
  FicusConfig ->
  m FicusAnalysisResults
runFicus maybeDebugDir ficusConfig = do
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

    -- Create files for teeing output if debug mode is enabled
    (stdoutFile, stderrFile) <- case maybeDebugDir of
      Just debugDir -> do
        sendIO $ do
          let stdoutPath = debugDir </> "fossa.ficus-stdout.log"
          let stderrPath = debugDir </> "fossa.ficus-stderr.log"
          stdoutH <- openFile stdoutPath WriteMode
          stderrH <- openFile stderrPath WriteMode
          pure (Just stdoutH, Just stderrH)
      Nothing ->
        -- No debug mode, don't tee to files
        pure (Nothing, Nothing)

    (result, exitCode, stdErrLines) <- sendIO $ withProcessWait processConfig $ \p -> do
      getCurrentTime >>= \now -> hPutStrLn stderr $ "[TIMING " ++ formatTime defaultTimeLocale "%H:%M:%S.%3q" now ++ "] Ficus process started, beginning stream processing..."
      let stdoutHandle = getStdout p
      let stderrHandle = getStderr p
      -- Start async reading of stderr to prevent blocking
      stderrAsync <- async $ consumeStderr stderrHandle stderrFile
      -- Read stdout in the main thread
      result <- streamFicusOutput stdoutHandle stdoutFile
      -- Wait for stderr to finish
      stdErrLines <- wait stderrAsync
      exitCode <- waitExitCode p
      pure (result, exitCode, stdErrLines)

    sendIO $ do
      traverse_ hClose stdoutFile
      traverse_ hClose stderrFile

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

    streamFicusOutput :: Handle -> Maybe Handle -> IO FicusAnalysisResults
    streamFicusOutput handle maybeFile = do
      accumulator <-
        Conduit.runConduit $
          CC.sourceHandle handle
            .| CC.decodeUtf8Lenient
            .| CC.linesUnbounded
            .| CC.mapM
              ( \line -> do
                  -- Tee raw line to file if debug mode
                  traverse_ (\fileH -> hPutStrLn fileH (toString line)) maybeFile
                  pure line
              )
            .| CCL.mapMaybe decodeStrictText
            .| CC.foldM
              ( \(currentSnippetResults, currentVendoredDeps) message -> do
                  -- Log messages as they come, with timestamps
                  timestamp <- currentTimeStamp
                  case message of
                    FicusMessageError err -> do
                      hPutStrLn stderr $ "[" ++ timestamp ++ "] ERROR " <> toString (displayFicusError err)
                      pure (currentSnippetResults, currentVendoredDeps)
                    FicusMessageDebug dbg -> do
                      hPutStrLn stderr $ "[" ++ timestamp ++ "] DEBUG " <> toString (displayFicusDebug dbg)
                      pure (currentSnippetResults, currentVendoredDeps)
                    FicusMessageFinding finding -> do
                      hPutStrLn stderr $ "[" ++ timestamp ++ "] FINDING " <> toString (displayFicusFinding finding)
                      let analysisFinding = findingToSnippetScanResult finding
                      let vendoredDep = findingToVendoredDependency finding
                      when (isJust currentSnippetResults && isJust analysisFinding) $
                        hPutStrLn stderr $
                          "[" ++ timestamp ++ "] ERROR " <> "Unexpected mutliple snippet scan results"
                      let newSnippetResults = currentSnippetResults <|> analysisFinding
                      let newVendoredDeps = case vendoredDep of
                            Just dep -> dep : currentVendoredDeps
                            Nothing -> currentVendoredDeps
                      pure (newSnippetResults, newVendoredDeps)
              )
              (Nothing, [])

      let (snippetResults, vendoredDeps) = accumulator
      let vendoredResults = case vendoredDeps of
            [] -> Nothing
            deps -> Just $ FicusVendoredDependencyScanResults (Just $ vendoredDepsToSourceUnit deps)

      pure $
        FicusAnalysisResults
          { snippetScanResults = snippetResults
          , vendoredDependencyScanResults = vendoredResults
          }

    consumeStderr :: Handle -> Maybe Handle -> IO [Text]
    consumeStderr handle maybeFile = do
      let loop acc (count :: Int) = do
            eof <- hIsEOF handle
            if eof
              then pure (reverse acc) -- Reverse at the end to get correct order
              else do
                line <- hGetLine handle
                -- Tee raw line to file if debug mode
                traverse_ (`hPutStrLn` line) maybeFile
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
    configArgs endpoint = ["analyze", "--secret", secret, "--endpoint", endpoint, "--locator", locator, "--set", "all:skip-hidden-files", "--set", "all:gitignore", "--exclude", ".git", "--exclude", ".git/**"] ++ configExcludes ++ configStrategies ++ maybe [] (\days -> ["--snippet-scan-retention-days", toText days]) snippetScanRetentionDays ++ [targetDir]
    targetDir = toText $ toFilePath $ ficusConfigRootDir ficusConfig
    secret = maybe "" (toText . unApiKey) $ ficusConfigSecret ficusConfig
    locator = renderLocator $ Locator "custom" (projectName $ ficusConfigRevision ficusConfig) (Just $ projectRevision $ ficusConfigRevision ficusConfig)
    configExcludes = concatMap (\path -> ["--exclude", unGlobFilter path]) $ ficusConfigExclude ficusConfig
    configStrategies = concatMap (\strategy -> ["--strategy", strategyToArg strategy]) $ ficusConfigStrategies ficusConfig
    strategyToArg = \case
      FicusStrategySnippetScan -> "snippet-scanning"
      FicusStrategyVendetta -> "vendetta"

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
