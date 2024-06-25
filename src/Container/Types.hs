{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}

module Container.Types (
  -- * Raw Image
  ContainerImageRaw (..),
  ContainerLayer (..),
  ContainerFSChangeSet (..),
  LayerPath,
  mkLayerPath,

  -- * Scanned Image
  ContainerScan (..),
  ContainerScanImage (..),
  ContainerScanImageLayer (..),

  -- * Jar Analysis Related Types
  JarObservation (..),
  DiscoveredJars (..),

  -- * helpers
  baseLayer,
  otherLayersSquashed,
  hasOtherLayers,
) where

import Codec.Archive.Tar.Index (TarEntryOffset)
import Container.Docker.Manifest (ManifestJson)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, FromJSONKey, Value, object, parseJSON, withObject, (.:))
import Data.Aeson.Types (ToJSON, toJSON, (.=))
import Data.Foldable (foldl')
import Data.List.NonEmpty as NonEmpty (NonEmpty, head, tail)
import Data.Map qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.String.Conversion (ToText, toText)
import Data.Text (Text)
import GHC.Generics (Generic)
import Srclib.Types (SourceUnit)

data ContainerImageRaw = ContainerImageRaw
  { layers :: NonEmpty.NonEmpty ContainerLayer
  , rawManifest :: ManifestJson
  }
  deriving (Show, Generic, Eq)
  deriving anyclass (NFData)

baseLayer :: ContainerImageRaw -> ContainerLayer
baseLayer img = NonEmpty.head $ layers img

hasOtherLayers :: ContainerImageRaw -> Bool
hasOtherLayers = not . null . NonEmpty.tail . layers

data ContainerLayer = ContainerLayer
  { layerChangeSets :: Seq ContainerFSChangeSet
  , lastOffset :: TarEntryOffset
  , layerDigest :: Text
  , layerPath :: Maybe LayerPath
  -- ^ When layers are squashed, a layer path may no longer be valid.
  }
  deriving (Show, Eq, Generic, NFData)

otherLayersSquashed :: ContainerImageRaw -> ContainerLayer
otherLayersSquashed containerImage = foldl' (<>) mempty restOfLayers
  where
    restOfLayers :: [ContainerLayer]
    restOfLayers = NonEmpty.tail (layers containerImage)

instance Semigroup ContainerLayer where
  ContainerLayer lhs _ _ _ <> ContainerLayer rhs offset digest _ = ContainerLayer (lhs <> rhs) offset digest Nothing

instance Monoid ContainerLayer where
  mempty =
    ContainerLayer
      { layerChangeSets = Seq.empty
      , lastOffset = 0
      , layerDigest = mempty
      , layerPath = Nothing
      }

-- | Record indicating a change fs event, with filepath and reference.
data ContainerFSChangeSet
  = -- | Record which is inserted or to be modified.
    InsertOrUpdate FilePath TarEntryOffset
  | -- | Record which is to be removed
    Whiteout FilePath
  deriving (Show, Generic, Eq, NFData)

data ContainerScan = ContainerScan
  { imageData :: ContainerScanImage
  , imageDigest :: Text
  , imageTag :: Text
  }
  deriving (Show, Eq, Ord)

instance ToJSON ContainerScan where
  toJSON scan = object ["image" .= imageData scan]

data ContainerScanImage = ContainerScanImage
  { imageOs :: Text
  , imageOsRelease :: Text
  , imageLayers :: [ContainerScanImageLayer]
  }
  deriving (Show, Eq, Ord)

instance ToJSON ContainerScanImage where
  toJSON ContainerScanImage{imageOs, imageOsRelease, imageLayers} =
    object
      [ "os" .= imageOs
      , "osRelease" .= imageOsRelease
      , "layers" .= imageLayers
      ]

data ContainerScanImageLayer = ContainerScanImageLayer
  { layerId :: Text
  , srcUnits :: [SourceUnit]
  , observations :: [JarObservation]
  }
  deriving (Show, Eq, Ord)

instance ToJSON ContainerScanImageLayer where
  toJSON ContainerScanImageLayer{layerId, srcUnits, observations} =
    object
      [ "layerId" .= layerId
      , "srcUnits" .= srcUnits
      , "observations" .= observations
      ]

-- | The Path within a container archive to the archive containing that a layer.
newtype LayerPath = LayerPath Text
  deriving (Eq, Ord, Show)
  deriving (FromJSONKey, FromJSON) via Text
  deriving newtype (NFData, ToText)

mkLayerPath :: ToText t => t -> LayerPath
mkLayerPath = LayerPath . toText

-- The CLI doesn't look at these values, so don't bother parsing them to anything specific.
-- This lets millhone modify individual observations more-or-less freely.
newtype JarObservation = JarObservation
  {inner :: Value}
  deriving (Eq, Ord, Show, Generic)
  deriving (ToJSON, FromJSON) via Value

-- | Output parse type for millhone.
newtype DiscoveredJars = DiscoveredJars
  { discoveredJars :: Map.Map LayerPath [JarObservation]
  }
  deriving (Eq, Ord, Show)

instance FromJSON DiscoveredJars where
  parseJSON = withObject "JarInput" $ \o -> DiscoveredJars <$> o .: "discovered_jars"
