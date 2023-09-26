{-# LANGUAGE DerivingStrategies #-}

module Control.Carrier.Telemetry.Types (
  CliEnvironment (..),
  LddVersionErr (..),
  SystemInfo (..),
  TelemetryRecord (..),
  TimedLogRecord (..),
  TelemetrySink (..),
  TelemetryCtx (..),
  TelemetryTimeSpent (..),
  TelemetryCmdConfig (..),
  UnameExecErr (..),
  CountableCliFeature (..),
  CIEnvironment (..),
) where

import Control.Concurrent.STM (TMVar)
import Control.Concurrent.STM.TBMQueue (TBMQueue)
import Data.Aeson (
  ToJSON (toEncoding, toJSON),
  ToJSONKey (toJSONKey),
  Value (String),
  defaultOptions,
  genericToEncoding,
 )
import Data.Aeson.Types (toJSONKeyText)
import Data.Map (Map)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text.Extra (showT)
import Data.Time (UTCTime)
import Data.Tracing.Instrument (CounterRegistry)
import Data.UUID (UUID)
import Effect.Logger (Severity)
import Fossa.API.Types (ApiOpts)
import GHC.Generics (Generic)

data TelemetrySink
  = TelemetrySinkToEndpoint ApiOpts
  | TelemetrySinkToFile
  deriving (Show, Eq, Ord)

data TelemetryCtx = TelemetryCtx
  { telId :: UUID
  , telLogsQ :: TBMQueue TimedLogRecord
  , telSink :: TMVar TelemetrySink
  , telTimeSpentQ :: TBMQueue TelemetryTimeSpent
  , telFossaConfig :: TMVar (Text, Value)
  , telCounters :: CounterRegistry CountableCliFeature Int
  , telStartUtcTime :: UTCTime
  }

data CountableCliFeature
  = ExperimentalGradleSingleConfigurationUsage
  | ExperimentalKeywordSearchUsage
  | CustomLicenseSearchUsage
  deriving (Show, Eq, Ord, Generic)

instance ToJSONKey CountableCliFeature where
  toJSONKey = toJSONKeyText (toText . show)

instance ToJSON CountableCliFeature where
  toEncoding = genericToEncoding defaultOptions

data TelemetryRecord = TelemetryRecord
  { cliCommandArgs :: [Text]
  , cliEnvironment :: CliEnvironment
  , cliExitedFatally :: Bool
  , cliResolvedConfig :: Maybe TelemetryCmdConfig
  , cliStartedAt :: UTCTime
  , cliSystemInfo :: SystemInfo
  , cliTelLogs :: [TimedLogRecord]
  , cliTelemetryId :: UUID
  , cliTimedDurations :: [TelemetryTimeSpent]
  , cliTotalDurationInSec :: Double
  , cliUsageCounter :: Map CountableCliFeature Int
  , cliVersion :: Text
  , cliCIEnvironment :: Maybe CIEnvironment
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON TelemetryRecord where
  toEncoding = genericToEncoding defaultOptions

data TelemetryCmdConfig = TelemetryCmdConfig
  { cmdName :: Text
  , cmdConfig :: Value
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON TelemetryCmdConfig where
  toEncoding = genericToEncoding defaultOptions

data TimedLogRecord = TimedLogRecord
  { ts :: UTCTime
  , severity :: Severity
  , message :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON TimedLogRecord where
  toEncoding = genericToEncoding defaultOptions

data CliEnvironment
  = CliProductionEnvironment
  | CliDevelopmentEnvironment
  deriving (Eq, Ord)

instance Show CliEnvironment where
  show CliProductionEnvironment = "prod"
  show CliDevelopmentEnvironment = "dev"

instance ToJSON CliEnvironment where
  toJSON = String . showT

newtype LddVersionErr = LddVersionErr Text
  deriving (Eq, Ord, Generic)
  deriving newtype (Show)

instance ToJSON LddVersionErr

newtype UnameExecErr = UnameExecErr Text
  deriving (Eq, Ord, Generic)
  deriving newtype (Show)

instance ToJSON UnameExecErr

data SystemInfo = SystemInfo
  { systemInfoOs :: String
  , systemInfoArch :: String
  , systemCapabilities :: Int
  , systemProcessors :: Int
  , systemDistroInfo :: Maybe String
  , systemUname :: Maybe (Either UnameExecErr Text)
  , systemLddVersion :: Maybe (Either LddVersionErr Text)
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON SystemInfo where
  toEncoding = genericToEncoding defaultOptions

data TelemetryTimeSpent = TelemetryTimeSpent
  { computationName :: Text
  , durationInSeconds :: Double
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON TelemetryTimeSpent where
  toEncoding = genericToEncoding defaultOptions

data CIEnvironment
  = GithubAction
  | AzurePipeline
  | Bamboo
  | UnknownCI
  deriving (Eq, Ord, Generic)

instance Show CIEnvironment where
  show GithubAction = "github_action"
  show AzurePipeline = "azure_pipeline"
  show Bamboo = "bamboo"
  show UnknownCI = "unknown_ci"

instance ToJSON CIEnvironment where
  toJSON = String . showT
