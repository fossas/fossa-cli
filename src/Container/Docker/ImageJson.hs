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
-- Refer to: https://github.com/moby/moby/blob/master/image/spec/v1.2.md#image-json-description
-- Refer to: https://github.com/moby/moby/blob/master/image/spec/v1.1.md#image-json-description
newtype ImageJson = ImageJson ImageJsonRootFs
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (NFData) -- explicitly use DeriveAnyClass strategy to avoid deriving-defaults warning.

instance FromJSON ImageJson where
  parseJSON = withObject "image json" $ \o ->
    ImageJson <$> o .: "rootfs"

newtype ImageJsonRootFs = ImageJsonRootFs {diffIds :: NonEmpty.NonEmpty Text}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (NFData) -- explicitly use DeriveAnyClass strategy to avoid deriving-defaults warning.

instance FromJSON ImageJsonRootFs where
  parseJSON = withObject "root fs" $ \o -> do
    diffIds <- o .: "diff_ids"
    diffType <- o .: "type"
    when (diffType /= "layers") $
      fail $
        "expected to have diff_ids of type: layer, but got: " <> diffType

    pure $ ImageJsonRootFs diffIds

decodeImageJson :: ByteStringLazy.ByteString -> Either String ImageJson
decodeImageJson = eitherDecode

-- | Gets all layer Ids.
getLayerIds :: ImageJson -> [Text]
getLayerIds (ImageJson (ImageJsonRootFs diffIds)) = NonEmpty.toList diffIds
