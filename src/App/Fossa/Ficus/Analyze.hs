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
import Control.Effect.Lift (Has, Lift)
import Effect.Logger (Logger, logDebug, logInfo, logWarn)
import Prettyprinter (pretty)

import Data.Aeson (Object, decode, (.:))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (fold, traverse_)
import Data.Hashable (Hashable)
import Data.Maybe (mapMaybe)
import Data.String.Conversion (ToText (toText))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Effect.Exec (AllowErr (Never), Command (..), Exec, execThrow')
import Effect.ReadFS (ReadFS)
import Fossa.API.Types (ApiKey (..), ApiOpts (..))
import Path (Abs, Dir, File, Path, toFilePath)
import Srclib.Types (Locator (..), renderLocator)
import Text.URI (render)
import Text.URI.Builder (PathComponent (..), TrailingSlash (..), setPath)
import Types (GlobFilter (..), LicenseScanPathFilters (..))
import Prelude hiding (unwords)

newtype CustomLicensePath = CustomLicensePath {unCustomLicensePath :: Text}
  deriving (Eq, Ord, Show, Hashable)
newtype CustomLicenseTitle = CustomLicenseTitle {unCustomLicenseTitle :: Text}
  deriving (Eq, Ord, Show, Hashable)

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
  logInfo $ "Running Ficus analysis on " <> pretty (toFilePath rootDir)
  messages <- runFicus ficusConfig
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
  , Has ReadFS sig m
  , Has Exec sig m
  , Has Logger sig m
  ) =>
  FicusConfig ->
  m FicusMessages
runFicus ficusConfig = do
  withFicusBinary $ \bin -> do
    cmd <- ficusCommand ficusConfig bin
    logDebug $ "Executing ficus"
    result <- execThrow' cmd
    let messages = parseFicusJson result Nothing

    -- Log summary of results
    logDebug $ "Ficus returned " <> pretty (length $ ficusMessageErrors messages) <> " errors, "
             <> pretty (length $ ficusMessageDebugs messages) <> " debug messages, "
             <> pretty (length $ ficusMessageFindings messages) <> " findings"

    -- Handle errors - log them as warnings instead of fatal errors
    -- Many ficus errors are expected (like binary file detection) and shouldn't stop analysis
    traverse_ (logWarn . pretty . displayFicusError) $ ficusMessageErrors messages

    -- Handle debug messages - log them as debug
    traverse_ (logDebug . pretty . displayFicusDebug) $ ficusMessageDebugs messages

    -- Log findings at info level for visibility since these are the actual results
    traverse_ (logInfo . pretty . displayFicusFinding) $ ficusMessageFindings messages

    pure messages
  where
    displayFicusDebug :: FicusDebug -> Text
    displayFicusDebug (FicusDebug FicusMessageData{..}) = "DEBUG " <> ficusMessageDataStrategy <> ": " <> ficusMessageDataPayload
    displayFicusError :: FicusError -> Text
    displayFicusError (FicusError FicusMessageData{..}) = "ERROR " <> ficusMessageDataStrategy <> ": " <> ficusMessageDataPayload
    displayFicusFinding :: FicusFinding -> Text
    displayFicusFinding (FicusFinding FicusMessageData{..}) = "FINDING " <> ficusMessageDataStrategy <> ": " <> ficusMessageDataPayload

-- Run Ficus, passing config-based args as configuration.
-- Caveat! This hard-codes some flags currently which may later need to be set on a strategy-by-strategy basis.
ficusCommand :: Has Diagnostics sig m => FicusConfig -> BinaryPaths -> m Command
ficusCommand ficusConfig bin = do
  endpoint <- case ficusConfigEndpoint ficusConfig of
    Just baseUri -> do
      pure $ render baseUri
    Nothing -> pure ""
  pure $ Command
    { cmdName = toText $ toPath bin
    , cmdArgs = configArgs endpoint
    , cmdAllowErr = Never
    }
  where
    configArgs endpoint = ["analyze", "--secret", secret, "--endpoint", endpoint, "--locator", locator, "--set", "all:skip-hidden-files", "--set", "all:gitignore"] ++ configExcludes ++ [targetDir]
    targetDir = toText $ toFilePath $ ficusConfigRootDir ficusConfig
    secret = maybe "" (toText . unApiKey) $ ficusConfigSecret ficusConfig
    locator = renderLocator $ Locator "custom" (projectName $ ficusConfigRevision ficusConfig) (Just $ projectRevision $ ficusConfigRevision ficusConfig)
    configExcludes = unGlobFilter <$> ficusConfigExclude ficusConfig

-- Parse Ficus's NDJson output by splitting on newlines (character 10) and
-- then decoding each line
parseFicusJson :: BL.ByteString -> Maybe (Path Abs File) -> FicusMessages
parseFicusJson out _configFile =
  fold messages
  where
    messageLines = BL.splitWith (== 10) out
    -- Once Ficus supports file filtering we'll do this by passing the config file path into Ficus
    -- and Ficus will skip scanning the config file. But for now let's just filter post-scan
    parsedLines = mapMaybe decode messageLines
    messages = map singletonFicusMessage parsedLines

-- add a FicusMessage to the corresponding entry of an empty FicusMessages
singletonFicusMessage :: FicusMessage -> FicusMessages
singletonFicusMessage message = case message of
  FicusMessageFinding msg -> mempty{ficusMessageFindings = [msg]}
  FicusMessageDebug msg -> mempty{ficusMessageDebugs = [msg]}
  FicusMessageError msg -> mempty{ficusMessageErrors = [msg]}
