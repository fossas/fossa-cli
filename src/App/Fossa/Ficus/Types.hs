module App.Fossa.Ficus.Types (
  FicusConfig (..),
  FicusMessage (..),
  FicusMessages (..),
  FicusMessageData (..),
  FicusFinding (..),
  FicusDebug (..),
  FicusError (..),
  FicusAnalysisFlag (..),
  FicusAllFlag (..),
  FicusWalkFlag (..),
  FicusNoopFlag (..),
  FicusHashFlag (..),
  FicusSnippetScanFlag,
  FicusSnippetScanResults (..),
  FicusScanStats (..),
  FicusPerStrategyFlag (..),
) where

import App.Types (ProjectRevision)
import Data.Aeson (FromJSON (parseJSON), Value (Object), withObject, withText)
import Data.Aeson.Types (Parser, (.:))
import Data.Text (Text)
import Fossa.API.Types
import GHC.Generics (Generic)
import Path (Abs, Dir, Path)
import Text.URI
import Types (GlobFilter)

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
  , ficusStatsNewFiles :: Int
  , ficusStatsExistingFiles :: Int
  , ficusStatsMatchedFiles :: Int
  , ficusStatsUnmatchedFiles :: Int
  , ficusStatsProcessingTimeSeconds :: Double
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON FicusScanStats where
  parseJSON = withObject "FicusScanStats" $ \obj ->
    FicusScanStats
      <$> obj .: "skipped_files"
      <*> obj .: "processed_files"
      <*> obj .: "new_files"
      <*> obj .: "existing_files"
      <*> obj .: "matched_files"
      <*> obj .: "unmatched_files"
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
  }
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
  = CommonFlag FicusAnalysisFlag
  | BatchLen Int
  deriving (Show, Eq)
