{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Container.Docker.ImageJson (
  ImageJson (..),
  ImageJsonRootFs (..),
  decodeImageJson,
  getLayerIds,
) where

import Control.DeepSeq (NFData)
import Control.Monad (when)
import Data.Aeson (
  FromJSON,
  eitherDecode,
  parseJSON,
  withObject,
  (.:),
 )
import Data.ByteString.Lazy qualified as ByteStringLazy
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Image Description Json.
newtype ImageJson = ImageJson ImageJsonRootFs
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (NFData) -- explicitly use DeriveAnyClass strategy to avoid deriving-defaults warning.

instance FromJSON ImageJson where
  parseJSON = withObject "" $ \o ->
    ImageJson <$> o .: "rootfs"

newtype ImageJsonRootFs = ImageJsonRootFs {diffIds :: NonEmpty.NonEmpty Text}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (NFData) -- explicitly use DeriveAnyClass strategy to avoid deriving-defaults warning.

instance FromJSON ImageJsonRootFs where
  parseJSON = withObject "" $ \o -> do
    diffIds <- o .: "diff_ids"
    diffType <- o .: "type"
    when (diffType /= "layers") $
      fail $
        "expected to have diff_ids of type: layer, but got: " <> diffType

    pure $ ImageJsonRootFs diffIds

decodeImageJson :: ByteStringLazy.ByteString -> Either String ImageJson
decodeImageJson = eitherDecode

getLayerIds :: ImageJson -> [Text]
getLayerIds (ImageJson (ImageJsonRootFs diffIds)) = NonEmpty.toList diffIds
