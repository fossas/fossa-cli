{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Container.Types (
  ContainerImageRaw (..),
  ContainerLayer (..),
  ContainerFSChangeSet (..),
  baseLayer,
) where

import Codec.Archive.Tar.Index (TarEntryOffset)
import Control.DeepSeq (NFData)
import Data.List.NonEmpty as NonEmpty (NonEmpty, head)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import GHC.Generics (Generic)

newtype ContainerImageRaw = ContainerImageRaw {layers :: NonEmpty.NonEmpty ContainerLayer}
  deriving (Show, Generic, Eq)
  deriving anyclass (NFData)

baseLayer :: ContainerImageRaw -> ContainerLayer
baseLayer img = NonEmpty.head $ layers img

data ContainerLayer = ContainerLayer
  { layerChangeSets :: !(Seq ContainerFSChangeSet)
  , lastOffset :: !TarEntryOffset
  }
  deriving (Show, Eq, Generic, NFData)

instance Semigroup ContainerLayer where
  ContainerLayer lhs _ <> ContainerLayer rhs offset =
    ContainerLayer (lhs <> rhs) offset

instance Monoid ContainerLayer where
  mempty = ContainerLayer Seq.empty 0

-- | Record indicating a change fs event, with filepath and reference.
data ContainerFSChangeSet
  = -- | Record which is inserted or to be modified.
    InsertOrUpdate !FilePath !TarEntryOffset
  | -- | Record which is to be removed
    Whiteout !FilePath
  deriving (Show, Generic, Eq, NFData)
