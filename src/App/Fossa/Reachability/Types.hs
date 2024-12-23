module App.Fossa.Reachability.Types (
  CallGraphAnalysis (..),
  SourceUnitReachability (..),
  ParsedJar (..),
  ContentRef (..),
  ReachabilityConfig (..),
  reachabilityRawJson,
  reachabilityEndpointJson,
) where

import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.ByteString.Lazy (ByteString)
import Data.Map (Map)
import Data.String.Conversion (ConvertUtf8 (decodeUtf8))
import Data.Text (Text)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path)
import Srclib.Types (Locator, OriginPath)

-- | Reachability on JVM projects relies on analyzing the binary JAR files
-- emitted by the build process. FOSSA CLI tries to identify these from project metadata,
-- but this may not work for all projects.
--
-- This config allows users to specify custom locations for the JAR file(s)
-- built from projects in their scan.
newtype ReachabilityConfig = ReachabilityConfig
  { configReachabilityJvmOutputs :: Map (Path Abs Dir) [Path Abs File]
  }
  deriving (Eq, Ord, Show, Monoid, Semigroup, ToJSON)

data SourceUnitReachability = SourceUnitReachability
  { srcUnitType :: Text
  , srcUnitManifest :: Text
  , srcUnitName :: Text
  , srcUnitOriginPaths :: [OriginPath]
  , srcUnitDependencies :: [Locator]
  , callGraphAnalysis :: CallGraphAnalysis
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON SourceUnitReachability

data CallGraphAnalysis
  = NoCallGraphAnalysis
  | JarAnalysis [ParsedJar]
  deriving (Eq, Ord, Show, Generic)

instance ToJSON CallGraphAnalysis where
  toJSON :: CallGraphAnalysis -> Value
  toJSON (NoCallGraphAnalysis) =
    object
      [ "kind" .= ("NoCallGraphAnalysis" :: String)
      ]
  toJSON (JarAnalysis entries) =
    object
      [ "kind" .= ("JarAnalysis" :: String)
      , "value" .= entries
      ]

data ContentRef
  = ContentRaw ByteString
  | ContentStoreKey Text
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ContentRef where
  toJSON :: ContentRef -> Value
  toJSON (ContentStoreKey key) =
    object
      [ "kind" .= ("ContentStoreKey" :: String)
      , "value" .= key
      ]
  toJSON (ContentRaw bs) =
    object
      [ "kind" .= ("ContentRaw" :: String)
      , "value" .= utf8Content
      ]
    where
      utf8Content :: String
      utf8Content = decodeUtf8 bs

data ParsedJar = ParsedJar
  { parsedJarPath :: Path Abs File
  , parsedJarContent :: ContentRef
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ParsedJar

reachabilityRawJson :: Text
reachabilityRawJson = "reachability.raw.json"

reachabilityEndpointJson :: Text
reachabilityEndpointJson = "reachability.endpoint.json"
