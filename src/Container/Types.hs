{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Container.Types (
  ContainerImageRaw (..),
  ContainerLayer (..),
  ContainerFSChangeSet (..),
  isWhiteOut,
  removeWhiteOut,
  baseLayer,
) where

import Codec.Archive.Tar.Index (TarEntryOffset)
import Control.DeepSeq (NFData)

import Container.Docker.Manifest (ManifestJson)
import Data.List.NonEmpty as NonEmpty
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.String.Conversion (ToString (toString), toText)
import Data.Text qualified as Text
import GHC.Generics (Generic)

data ContainerImageRaw = ContainerImageRaw
  { manifest :: ManifestJson
  , layers :: NonEmpty.NonEmpty ContainerLayer
  }
  deriving (Show, Generic, NFData)

baseLayer :: ContainerImageRaw -> ContainerLayer
baseLayer img = NonEmpty.head $ layers img

data ContainerLayer = ContainerLayer
  { layerChangeSets :: !(Seq ContainerFSChangeSet)
  , lastOffset :: !TarEntryOffset
  }
  deriving (Show, Generic, NFData)

instance Semigroup ContainerLayer where
  ContainerLayer lhs _ <> ContainerLayer rhs offset = ContainerLayer (lhs <> rhs) offset

instance Monoid ContainerLayer where
  mempty = ContainerLayer Seq.empty 0

-- | Record indicating a change fs event, with filepath and reference.
data ContainerFSChangeSet
  = -- | Record which is inserted or to be modified.
    InsertOrUpdate !FilePath !TarEntryOffset
  | -- | Record which is to be removed
    Whiteout !FilePath
  deriving (Show, Generic, NFData)

-- | Returns true if the provided filepath has whiteout prefix. Otherwise returns False.
--
-- >> isWhiteOut "" = False
-- >> isWhiteOut "" = True
isWhiteOut :: FilePath -> Bool
isWhiteOut path = Text.isPrefixOf ".wh." (snd $ Text.breakOnEnd "/" $ toText path)

-- | Removes whiteout prefix from the filepath. If no whiteout prefix is detected returns Nothing.
--
-- >> removeWhiteOut "etc/hello.txt" = Nothing
-- >> removeWhiteOut "etc/.wh.hello.txt" = Just "etc/hello.txt"
-- >> removeWhiteOut "etc/w.h.os" = Just "etc/os"
removeWhiteOut :: FilePath -> Maybe FilePath
removeWhiteOut path =
  if Text.isPrefixOf ".wh." (snd $ Text.breakOnEnd "/" $ toText path)
    then Just $ toString $ Text.replace ".wh." "" (toText path)
    else Nothing
