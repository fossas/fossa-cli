{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Ficus.Types (
  FicusConfig (..),
  FicusMessage (..),
  FicusMessages (..),
  FicusMessageData (..),
  FicusFinding (..),
  FicusDebug (..),
  FicusError (..),
  FicusAnalysisFlag (..),
  FicusStrategy (..),
  FicusAllFlag (..),
  FicusWalkFlag (..),
  FicusNoopFlag (..),
  FicusHashFlag (..),
  FicusSnippetScanFlag,
  FicusSnippetScanResults (..),
  FicusScanStats (..),
  FicusVendettaFlag,
  FicusPerStrategyFlag (..),
  FicusAnalysisResults (..),
  FicusVendoredDependency (..),
  FicusVendoredLocation (..),
  ficusVendoredLocationPath,
  FicusVendoredDependencyScanResults (..),
  WorkflowExecutable (..),
  WorkflowRunArtifact (..),
  WorkflowEvent (..),
  toWorkflowExecutable,
  findingToWorkflowEvent,
  workflowResultJson,
) where

import App.Types (ProjectRevision)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Object), decodeStrictText, object, withObject, withText, (.=))
import Data.Aeson.Types (Parser, (.:), (.:?))
import Data.String.Conversion (toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Fossa.API.Types
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path, fileExtension, toFilePath)
import Srclib.Types (SourceUnit)
import Text.URI
import Types (GlobFilter)

data FicusAnalysisResults = FicusAnalysisResults
  { snippetScanResults :: Maybe FicusSnippetScanResults
  , vendoredDependencyScanResults :: Maybe FicusVendoredDependencyScanResults
  }

newtype FicusVendoredDependencyScanResults = FicusVendoredDependencyScanResults (Maybe SourceUnit)

data FicusVendoredDependency = FicusVendoredDependency
  { ficusVendoredDependencyName :: Text
  , ficusVendoredDependencyEcosystem :: Text
  , ficusVendoredDependencyVersion :: Maybe Text
  , ficusVendoredDependencyLocations :: [FicusVendoredLocation]
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON FicusVendoredDependency where
  parseJSON = withObject "FicusVendoredDependency" $ \obj ->
    FicusVendoredDependency
      <$> obj .: "name"
      <*> obj .: "ecosystem"
      <*> obj .:? "version"
      <*> obj .: "locations"

data FicusVendoredLocation
  = FicusVendoredFile Text
  | FicusVendoredDirectory Text
  deriving (Eq, Ord, Show, Generic)

ficusVendoredLocationPath :: FicusVendoredLocation -> Text
ficusVendoredLocationPath (FicusVendoredFile path) = path
ficusVendoredLocationPath (FicusVendoredDirectory path) = path

instance FromJSON FicusVendoredLocation where
  parseJSON = withObject "FicusVendoredLocation" $ \obj -> do
    mFile <- obj .:? "file"
    mDir <- obj .:? "directory"
    case (mFile, mDir) of
      (Just path, Nothing) -> pure $ FicusVendoredFile path
      (Nothing, Just path) -> pure $ FicusVendoredDirectory path
      (Just _, Just _) -> fail "FicusVendoredLocation: both 'file' and 'directory' keys present"
      (Nothing, Nothing) -> fail "FicusVendoredLocation: expected 'file' or 'directory' key"

data FicusSnippetScanResults = FicusSnippetScanResults
  { ficusSnippetScanResultsAnalysisId :: Int
  , ficusSnippetScanResultsBucketId :: Int
  , ficusSnippetScanResultsStats :: FicusScanStats
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON FicusSnippetScanResults where
  parseJSON = withObject "FicusSnippetScanResults" $ \obj ->
    FicusSnippetScanResults
      <$> obj .: "analysis_id"
      <*> obj .: "bucket_id"
      <*> obj .: "stats"

data FicusScanStats = FicusScanStats
  { ficusStatsSkippedFiles :: Int
  , ficusStatsProcessedFiles :: Int
  , ficusStatsUniqueProcessedFiles :: Int
  , ficusStatsUniqueNewFiles :: Int
  , ficusStatsUniqueExistingFiles :: Int
  , ficusStatsUniqueMatchedFiles :: Int
  , ficusStatsUniqueUnmatchedFiles :: Int
  , ficusStatsProcessingTimeSeconds :: Double
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON FicusScanStats where
  parseJSON = withObject "FicusScanStats" $ \obj ->
    FicusScanStats
      <$> obj .: "skipped_files"
      <*> obj .: "processed_files"
      <*> obj .: "unique_processed_files"
      <*> obj .: "unique_new_files"
      <*> obj .: "unique_existing_files"
      <*> obj .: "unique_matched_files"
      <*> obj .: "unique_unmatched_files"
      <*> obj .: "processing_time_seconds"

data FicusMessages = FicusMessages
  { ficusMessageDebugs :: [FicusDebug]
  , ficusMessageErrors :: [FicusError]
  , ficusMessageFindings :: [FicusFinding]
  }
  deriving (Eq, Ord, Show, Generic)

instance Semigroup FicusMessages where
  FicusMessages w1 e1 m1 <> FicusMessages w2 e2 m2 = FicusMessages (w1 <> w2) (e1 <> e2) (m1 <> m2)

instance Monoid FicusMessages where
  mempty = FicusMessages [] [] []

data FicusMessageData = FicusMessageData
  { ficusMessageDataStrategy :: Text
  , ficusMessageDataPayload :: Text
  }
  deriving (Eq, Ord, Show, Generic)

newtype FicusDebug = FicusDebug FicusMessageData deriving (Eq, Ord, Show, Generic)

newtype FicusFinding = FicusFinding FicusMessageData deriving (Eq, Ord, Show, Generic)

newtype FicusError = FicusError FicusMessageData deriving (Eq, Ord, Show, Generic)

data FicusMessage
  = FicusMessageFinding FicusFinding
  | FicusMessageDebug FicusDebug
  | FicusMessageError FicusError
  deriving (Eq, Ord, Show, Generic)

data FicusMessageKind = FicusMessageKindFinding | FicusMessageKindDebug | FicusMessageKindError
  deriving (Eq, Ord, Show, Generic)

instance FromJSON FicusMessageKind where
  parseJSON = withText "FicusMessageKind" $ \msg -> do
    case msg of
      "finding" -> pure FicusMessageKindFinding
      "error" -> pure FicusMessageKindError
      "debug" -> pure FicusMessageKindDebug
      _ -> fail "invalid Ficus message type"

-- Ficus observations follow the pattern:
-- ```json
-- {
--   "version": 1,
--   "level": "INFO",
--   "observation": {
--     "kind": "finding",
--     "payload": "oiyOd12FYglo6gJu8gnNyytDfZDNGe83yF4rOds7YxU=",
--     "strategy": "hash"
--   }
-- }
--
-- {
--   "version": 1,
--   "level": "DEBUG",
--   "observation": {
--     "kind": "debug",
--     "payload": "A potentially useful but not finding-worthy note",
--     "strategy": "hash"
--   }
-- }
--
-- {
--   "version": 1,
--   "level": "ERROR",
--   "observation": {
--     "kind": "error",
--     "payload": "Something bad happened",
--     "strategy": "hash"
--   }
-- }
-- ```
--
-- Each ficus observation is either:
-- - A finding; at least somewhat likely to be consumed and used by the CLI.
-- - A debug; over-communicative and generally for human eyes.
-- - An error; error messages which ought to be propagated.

instance FromJSON FicusMessage where
  parseJSON (Object o) = do
    observationVersion <- o .: "version" :: Parser Int
    if observationVersion /= 1
      then
        fail "Invalid version for FicusMessage. It must be 1."
      else do
        Object observation <- o .: "observation"
        kind <- observation .: "kind"
        strategy <- observation .: "strategy"
        payload <- observation .: "payload"
        let messageData = FicusMessageData{ficusMessageDataStrategy = strategy, ficusMessageDataPayload = payload}
        case kind of
          FicusMessageKindFinding -> do
            let finding = FicusFinding messageData
            pure $ FicusMessageFinding finding
          FicusMessageKindDebug -> do
            let debug = FicusDebug messageData
            pure $ FicusMessageDebug debug
          FicusMessageKindError -> do
            let ficusError = FicusError messageData
            pure $ FicusMessageError ficusError
  parseJSON _ = fail "Invalid schema for FicusMessage. It must be an object"

data FicusConfig = FicusConfig
  { ficusConfigRootDir :: Path Abs Dir
  , ficusConfigExclude :: [GlobFilter]
  , ficusConfigEndpoint :: Maybe URI
  , ficusConfigSecret :: Maybe ApiKey
  , ficusConfigRevision :: ProjectRevision -- TODO: get this from `projectRevision AnalyzeConfig`
  , ficusConfigFlags :: [FicusPerStrategyFlag]
  , ficusConfigSnippetScanRetentionDays :: Maybe Int
  , ficusConfigStrategies :: [FicusStrategy]
  }
  deriving (Show, Eq, Generic)

data FicusStrategy
  = FicusStrategySnippetScan
  | FicusStrategyVendetta
  deriving (Show, Eq, Generic)

-- A flag for ficus paired with a proper strategy or pseudo-strategy.
-- @Walk@ and @All@ are pseudo-strategies which accept similar flags,
-- but expand into a subset of strategies in ficus.
data FicusPerStrategyFlag
  = Walk FicusWalkFlag
  | All FicusAllFlag
  | SnippetScan FicusSnippetScanFlag
  | Noop FicusNoopFlag
  | Hash FicusHashFlag
  | Vendetta FicusVendettaFlag
  deriving (Show, Eq, Generic)

data FicusAnalysisFlag
  = AllExtensions
  | SkipHiddenFiles
  | Gitignore
  deriving (Show, Eq)

newtype FicusAllFlag = FicusAllFlag FicusAnalysisFlag deriving (Show, Eq)

newtype FicusWalkFlag = FicusWalkFlag FicusAnalysisFlag deriving (Show, Eq)

newtype FicusNoopFlag = FicusNoopFlag FicusAnalysisFlag deriving (Show, Eq)

newtype FicusHashFlag = FicusHashFlag FicusAnalysisFlag deriving (Show, Eq)

data FicusSnippetScanFlag
  = SnippetScanCommonFlag FicusAnalysisFlag
  | SnippetScanBatchLen Int
  deriving (Show, Eq)

data FicusVendettaFlag
  = VendettaCommonFlag FicusAnalysisFlag
  | VendettaBatchLen Int
  deriving (Show, Eq)

-- | The program ficus should run, and the arguments that lead its command line.
-- ficus appends the target and its own @--output@ after these.
data WorkflowExecutable = WorkflowExecutable
  { workflowExecutableProgram :: Text
  , workflowExecutableArgs :: [Text]
  }
  deriving (Eq, Ord, Show, Generic)

-- | @Executable@ is the one type in the run artifact serde does /not/ rename, so
-- its keys stay lowercase while the artifact around it is camelCase.
instance ToJSON WorkflowExecutable where
  toJSON WorkflowExecutable{..} =
    object
      [ "program" .= workflowExecutableProgram
      , "args" .= workflowExecutableArgs
      ]

-- | The run artifact handed to @ficus x-workflow@ on stdin.
data WorkflowRunArtifact = WorkflowRunArtifact
  { workflowArtifactExecutable :: WorkflowExecutable
  , workflowArtifactTarget :: Path Abs Dir
  , workflowArtifactWorkingDirectory :: Path Abs Dir
  }
  deriving (Eq, Ord, Show, Generic)

-- | @version@ must be exactly 1: ficus rejects any other value rather than
-- defaulting. @idleTimeoutSeconds@ and @totalTimeoutSeconds@ carry serde
-- defaults on the ficus side and are deliberately omitted.
instance ToJSON WorkflowRunArtifact where
  toJSON WorkflowRunArtifact{..} =
    object
      [ "version" .= (1 :: Int)
      , "executable" .= workflowArtifactExecutable
      , "target" .= toFilePath workflowArtifactTarget
      , "workingDirectory" .= toFilePath workflowArtifactWorkingDirectory
      ]

-- | ficus resolves @program@ to a file and requires the executable bit, so a
-- plain JS bundle can never be the program itself. A non-JS path passes through
-- unchanged: that is the wrapper-script escape hatch and the seam for a future
-- compiled analyzer.
toWorkflowExecutable :: Path Abs File -> WorkflowExecutable
toWorkflowExecutable path
  | isJsBundle = WorkflowExecutable "node" [rendered]
  | otherwise = WorkflowExecutable rendered []
  where
    rendered :: Text
    rendered = toText $ toFilePath path

    isJsBundle :: Bool
    isJsBundle = case fileExtension path :: Maybe String of
      Just ext -> Text.toLower (toText ext) `elem` [".js", ".mjs", ".cjs"]
      Nothing -> False

-- | What ficus reports about a workflow run, carried as a JSON string in the
-- observation payload of a @workflow@-strategy finding.
data WorkflowEvent
  = WorkflowStarted
      { workflowResolvedProgram :: Text
      , workflowAnalyzerVersion :: Text
      }
  | WorkflowStepCompleted Text
  | WorkflowResult Value
  | WorkflowFailed
      { workflowFailureReason :: Text
      , workflowFailureStderrTail :: Text
      }
  deriving (Eq, Show, Generic)

-- | @exitCode@ and @timeout@ are dropped: both are optional on the wire and
-- @reason@ already renders them in prose.
instance FromJSON WorkflowEvent where
  parseJSON = withObject "WorkflowEvent" $ \obj -> do
    eventType <- obj .: "type" :: Parser Text
    case eventType of
      "workflow-started" ->
        WorkflowStarted
          <$> obj .: "resolvedProgram"
          <*> obj .: "analyzerVersion"
      "step-completed" -> WorkflowStepCompleted <$> obj .: "step"
      "workflow-result" -> WorkflowResult <$> obj .: "result"
      "workflow-failed" ->
        WorkflowFailed
          <$> obj .: "reason"
          <*> obj .: "stderrTail"
      other -> fail $ "unknown workflow event type: " <> toString other

-- | Findings on any other strategy belong to another consumer.
findingToWorkflowEvent :: FicusFinding -> Maybe WorkflowEvent
findingToWorkflowEvent (FicusFinding (FicusMessageData strategy payload))
  | Text.toLower strategy == "workflow" = decodeStrictText payload
findingToWorkflowEvent _ = Nothing

-- | Debug-bundle key the workflow result is recorded under.
workflowResultJson :: Text
workflowResultJson = "workflow.result.json"
