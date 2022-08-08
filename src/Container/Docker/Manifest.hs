{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Container.Docker.Manifest (
  ManifestJson (..),
  ManifestJsonImageEntry (..),
  decodeManifestJson,
  getLayerPaths,
  manifestFilename,
  getImageJsonConfigFilePath,
  getImageDigest,
) where

import Control.DeepSeq (NFData)
import Data.Aeson (
  FromJSON,
  ToJSON,
  defaultOptions,
  eitherDecode,
  genericToEncoding,
  parseJSON,
  toEncoding,
  withObject,
  (.:),
 )
import Data.ByteString.Lazy qualified as ByteStringLazy
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)

manifestFilename :: Text
manifestFilename = "manifest.json"

-- | The manifest of container image for the top-level image, and optionally for parent images
-- that this image was derived from. It consists of an array of metadata entries.
--
-- Reference:
-- * https://github.com/moby/moby/blob/master/image/spec/v1.2.md
-- * https://github.com/moby/moby/blob/master/image/spec/v1.1.md
newtype ManifestJson = ManifestJson (NonEmpty.NonEmpty ManifestJsonImageEntry)
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (NFData) -- explicitly use DeriveAnyClass strategy to avoid deriving-defaults warning.

instance FromJSON ManifestJson
instance ToJSON ManifestJson where
  toEncoding = genericToEncoding defaultOptions

data ManifestJsonImageEntry = ManifestJsonImageEntry
  { config :: Text -- references another file in the tar which includes the image JSON for this image.
  , repoTags :: [Text] -- references pointing to this image.
  , layers :: NonEmpty.NonEmpty FilePath -- points to the filesystem changeset tars
  }
  deriving (Show, Eq, Ord, Generic, NFData)

instance ToJSON ManifestJsonImageEntry where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ManifestJsonImageEntry where
  parseJSON = withObject "" $ \o ->
    ManifestJsonImageEntry
      <$> o .: "Config"
      <*> o .: "RepoTags"
      <*> o .: "Layers"

getLayerPaths :: ManifestJson -> NonEmpty.NonEmpty FilePath
getLayerPaths (ManifestJson mjEntries) = layers $ NonEmpty.head mjEntries

decodeManifestJson :: ByteStringLazy.ByteString -> Either String ManifestJson
decodeManifestJson = eitherDecode

getImageJsonConfigFilePath :: ManifestJson -> Text
getImageJsonConfigFilePath (ManifestJson mjEntries) = config $ NonEmpty.head mjEntries

-- | Gets the image digest.
-- Exported docker ball's config filename is digest of the image.
getImageDigest :: ManifestJson -> Text
getImageDigest mj = "sha256:" <> Text.replace ".json" "" (getImageJsonConfigFilePath mj)
