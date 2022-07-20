{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Container.Types (
    ContainerImageRaw(..),
  ContainerLayerImage (..),
  ContainerFSChangeSet (..),
  isWhiteOut,
  removeWhiteOut,
) where

import Codec.Archive.Tar.Index (TarEntryOffset)
import Control.DeepSeq (NFData)

import Data.Text qualified as Text
import GHC.Generics (Generic)
import Container.Docker.Manifest (ManifestJson)

data ContainerImageRaw = ContainerImageRaw
    { manifest :: ManifestJson
    , layers :: [ContainerLayerImage]
    }
    deriving (Show, Generic, NFData)

data ContainerLayerImage = ContainerLayerImage
  { layerChangeSets :: [ContainerFSChangeSet]
  , lastOffset :: TarEntryOffset
  }
  deriving (Show, Generic, NFData)

-- | Record indicating a change fs event, with filepath and reference.
data ContainerFSChangeSet
  = InsertOrUpdate !FilePath !TarEntryOffset
  | Whiteout !FilePath
  deriving (Show, Generic, NFData)

isWhiteOut :: FilePath -> Bool
isWhiteOut path = Text.isPrefixOf ".wh." (snd $ Text.breakOnEnd "/" $ Text.pack path)

removeWhiteOut :: FilePath -> Maybe FilePath
removeWhiteOut path =
  if Text.isPrefixOf ".wh." (snd $ Text.breakOnEnd "/" $ Text.pack path)
    then Just $ Text.unpack $ Text.replace ".wh." "" (Text.pack path)
    else Nothing
