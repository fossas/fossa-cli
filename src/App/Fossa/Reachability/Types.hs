module App.Fossa.Reachability.Types (
  CallGraphAnalysis (..),
  SourceUnitReachability (..),
  ParsedJar (..),
  ContentRef (..),
  reachabilityRawJson,
  reachabilityEndpointJson,
) where

import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.ByteString.Lazy (ByteString)
import Data.String.Conversion (ConvertUtf8 (decodeUtf8))
import Data.Text (Text)
import GHC.Generics (Generic)
import Path (Abs, File, Path)
import Srclib.Types (Locator, OriginPath)

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
