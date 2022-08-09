{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module Container.Types (
  ContainerImageRaw (..),
  ContainerLayer (..),
  ContainerFSChangeSet (..),
  baseLayer,
  otherLayersSquashed,
  ContainerScan (..),
  ContainerScanImage (..),
  ContainerScanImageLayer (..),
) where

import Codec.Archive.Tar.Index (TarEntryOffset)
import Container.Docker.Manifest (ManifestJson)
import Control.DeepSeq (NFData)
import Data.Aeson (object)
import Data.Aeson.Types (ToJSON, toJSON, (.=))
import Data.Foldable (foldl')
import Data.List.NonEmpty as NonEmpty (NonEmpty, head, tail)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
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

data ContainerLayer = ContainerLayer
  { layerChangeSets :: Seq ContainerFSChangeSet
  , lastOffset :: TarEntryOffset
  , layerDigest :: Text
  }
  deriving (Show, Eq, Generic, NFData)

otherLayersSquashed :: ContainerImageRaw -> ContainerLayer
otherLayersSquashed containerImage = foldl' (<>) mempty restOfLayers
  where
    restOfLayers :: [ContainerLayer]
    restOfLayers = NonEmpty.tail (layers containerImage)

instance Semigroup ContainerLayer where
  ContainerLayer lhs _ _ <> ContainerLayer rhs offset digest = ContainerLayer (lhs <> rhs) offset digest

instance Monoid ContainerLayer where
  mempty = ContainerLayer Seq.empty 0 mempty

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
  toJSON ContainerScanImage{..} =
    object
      [ "os" .= imageOs
      , "osRelease" .= imageOsRelease
      , "layers" .= imageLayers
      ]

data ContainerScanImageLayer = ContainerScanImageLayer
  { layerId :: Text
  , srcUnits :: [SourceUnit]
  }
  deriving (Show, Eq, Ord)

instance ToJSON ContainerScanImageLayer where
  toJSON ContainerScanImageLayer{..} =
    object
      [ "layerId" .= layerId
      , "srcUnits" .= srcUnits
      ]
