{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Ficus.Analyze (
  analyzeWithFicus
) where

import App.Fossa.EmbeddedBinary (BinaryPaths, toPath, withFicusBinary)
import App.Fossa.Ficus.Types (FicusSnippetScanResults (..), FicusMessages (..), FicusMessage (..), FicusConfig (..), FicusDebug (..), FicusMessageData (..), FicusError (..), FicusFinding (..), FicusAnalysisFlag (..), FicusAllFlag (..), FicusPerStrategyFlag (..))
import App.Types (FileUpload (..), ProjectRevision (..))
import Control.Carrier.Debug (Debug)
import Control.Carrier.Diagnostics (Diagnostics, fatal, warn)
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Carrier.Telemetry.Types
import Control.Effect.FossaApiClient (FossaApiClient, getOrganization)
import Control.Effect.Lift (Has, Lift)
import Control.Effect.Telemetry (Telemetry, trackUsage)
import Control.Monad (join, unless)
import Data.Aeson (decode, Object, (.:))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (fold, traverse_)
import Data.HashMap.Internal.Strict (HashMap)
import Data.HashMap.Strict qualified as H
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.List (nub)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.String.Conversion (ToText (toText), decodeUtf8)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Effect.Exec (AllowErr (Never), Command (..), Exec, execCurrentDirStdinThrow, execThrow')
import Effect.ReadFS (ReadFS)
import Fossa.API.Types (ApiKey (..), ApiOpts (..), Organization (..), orgFileUpload)
import Path (Abs, Dir, File, Path, toFilePath)
import Prettyprinter (pretty)
import Srclib.Types
  ( LicenseScanType (..)
  , LicenseSourceUnit (..)
  , LicenseUnit (..)
  , LicenseUnitData (..)
  , LicenseUnitInfo (..)
  , LicenseUnitMatchData (..)
  , Locator (..)
  , renderLocator
  )
import Effect.Logger (Logger, logDebug, logInfo, logError)
import Text.URI (render)
import Text.URI.Builder (PathComponent (..), TrailingSlash (..), setPath)
import Types (GlobFilter (..), LicenseScanPathFilters (..), LicenseScanPathFilters)
import Prelude hiding (unwords)
import Data.Flag (Flag, fromFlag)
import App.Fossa.Config.Analyze (SnippetScan)
import qualified App.Fossa.Config.Analyze as Config
-- | scan rootDir with Ficus, using the given GrepOptions. This is the main entry point to this module
analyzeWithFicus ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has Logger sig m
  ) =>
  Flag SnippetScan ->
  Path Abs Dir ->
  Maybe ApiOpts ->
  ProjectRevision ->
  Maybe LicenseScanPathFilters ->
  m (Maybe FicusSnippetScanResults)
analyzeWithFicus snippetScan rootDir apiOpts revision filters = do
  if fromFlag Config.SnippetScan snippetScan
    then do
      logInfo $ "Running Ficus analysis on " <> pretty (toFilePath rootDir)
      messages <- runFicus ficusConfig
      let ficusResults = ficusMessagesToFicusSnippetScanResults messages
      case ficusResults of
        Just results -> do
          logInfo $ "Ficus analysis completed successfully with analysis ID: " <> pretty (ficusSnippetScanResultsAnalysisId results)
        Nothing -> logError "Ficus analysis completed but no fingerprint findings were found"
      pure ficusResults
    else do
      logInfo $ "Skipping Ficus analysis on " <> pretty (toFilePath rootDir)
      pure Nothing
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
    logDebug $ "Executing Ficus command: " <> pretty (show cmd)
    result <- execThrow' cmd
    let messages = parseFicusJson result

    -- Log findings at debug level for visibility
    traverse_ (logDebug . pretty . displayFicusFinding) $ ficusMessageFindings messages
    traverse_ (logError . pretty . displayFicusError) $ ficusMessageErrors messages
    traverse_ (logDebug . pretty . displayFicusDebug) $ ficusMessageDebugs messages

    pure messages
  where
    displayFicusDebug :: FicusDebug -> Text
    displayFicusDebug (FicusDebug FicusMessageData{..}) = "DEBUG " <> ficusMessageDataStrategy <> ": " <> ficusMessageDataPayload
    displayFicusError :: FicusError -> Text
    displayFicusError (FicusError FicusMessageData{..}) = "ERROR " <> ficusMessageDataStrategy <> ": " <> ficusMessageDataPayload
    displayFicusFinding :: FicusFinding -> Text
    displayFicusFinding (FicusFinding FicusMessageData{..}) = "FINDING " <> ficusMessageDataStrategy <> ": " <> ficusMessageDataPayload


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

-- Run Ficus, passing config-based args as configuration.
-- Caveat! This hard-codes some flags currently which may later need to be set on a strategy-by-strategy basis.
ficusCommand :: Has Diagnostics sig m => FicusConfig -> BinaryPaths -> m Command
ficusCommand ficusConfig bin = do
  endpoint <- case ficusConfigEndpoint ficusConfig of
    Just baseUri -> do
      proxyUri <- setPath [PathComponent "api", PathComponent "proxy"] (TrailingSlash False) baseUri
      pure $ render proxyUri
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
parseFicusJson :: BL.ByteString -> FicusMessages
parseFicusJson out =
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
