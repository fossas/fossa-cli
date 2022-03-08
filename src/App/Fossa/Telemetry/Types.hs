module App.Fossa.Telemetry.Types (
  CliEnvironment (..),
  SystemInfo (..),
  TelemetryRecord (..),
  TimedLogRecord (..),
  TelemetrySink (..),
  TelemetryCtx (..),
  TelemetryTimeSpent (..),
  TelemetryCmdConfig (..),
  CountableCliFeature (..),
) where

import Control.Concurrent.STM (TMVar)
import Control.Concurrent.STM.TBMQueue (TBMQueue)
import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)
import Data.Map (Map)
import Data.String.Conversion (toText)
import Data.Text (Text)
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
  , telTimeSpent :: TBMQueue TelemetryTimeSpent
  , telFossaConfig :: TMVar (Text, Value)
  , telCounters :: CounterRegistry CountableCliFeature Int
  , telStartUtcTime :: UTCTime
  }

data CountableCliFeature
  = ExperimentalGradleSingleConfigurationUsage
  | SomeOtherFeature
  deriving (Show, Eq, Ord, Generic)

instance ToJSONKey CountableCliFeature where
  toJSONKey = toJSONKeyText (toText . show)

instance ToJSON CountableCliFeature where
  toEncoding = genericToEncoding defaultOptions

data TelemetryRecord = TelemetryRecord
  { cliTelemetryId :: UUID
  , cliVersion :: Text
  , cliCommandArgs :: [Text]
  , cliEnvironment :: CliEnvironment
  , cliStartedAt :: UTCTime
  , cliTotalDurationInSec :: Double
  , cliExitedFatally :: Bool
  , cliResolvedConfig :: Maybe TelemetryCmdConfig
  , cliSystemInfo :: SystemInfo
  , cliUsageCounter :: Map CountableCliFeature Int
  , cliTimedDurations :: [TelemetryTimeSpent]
  , cliTelLogs :: [TimedLogRecord]
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
  toJSON CliProductionEnvironment = String . toText . show $ CliProductionEnvironment
  toJSON CliDevelopmentEnvironment = String . toText . show $ CliDevelopmentEnvironment

data SystemInfo = SystemInfo
  { systemInfoOs :: String
  , systemInfoArch :: String
  , systemCapabilities :: Int
  , systemProcessors :: Int
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON SystemInfo where
  toEncoding = genericToEncoding defaultOptions

data TelemetryTimeSpent = TelemetryTimeSpent
  { computationName :: Text
  , cpuDurationInSeconds :: Double
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON TelemetryTimeSpent where
  toEncoding = genericToEncoding defaultOptions
