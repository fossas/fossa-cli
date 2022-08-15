module Container.Docker.OciManifestIndex (
  OciManifestIndex (..),
  OciManifestIndexItem (..),
  supportedManifestIndexKinds,
  digestOf,
) where

import Container.Docker.OciManifest (supportedManifestKinds)
import Container.Docker.SourceParser (RepoDigest (RepoDigest), RepoReference (RepoReferenceDigest))
import Control.Monad (unless)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Foldable (find)
import Data.List.NonEmpty qualified as NonEmpty
import Data.String.Conversion (toString)
import Data.Text (Text)

supportedManifestIndexKinds :: [Text]
supportedManifestIndexKinds =
  [ "application/vnd.docker.distribution.manifest.list.v2+json"
  , "application/vnd.oci.image.index.v1+json"
  ]

-- | Oci Image Manifests.
--
-- Container Registries produce manifest listing when image supports
-- multi-platforms. Docker refers to as manifest list, where as OCI
-- refers to as index. They are both of same shape.
--
-- Refer to:
--  https://github.com/opencontainers/image-spec/blob/main/image-index.md
--  https://docs.docker.com/registry/spec/manifest-v2-2/#example-manifest-list
newtype OciManifestIndex
  = OciManifestIndex (NonEmpty.NonEmpty OciManifestIndexItem)
  deriving (Show, Eq, Ord)

instance FromJSON OciManifestIndex where
  parseJSON = withObject "OciManifestIndex" $ \o -> do
    mediaType :: Text <- o .: "mediaType"
    unless (mediaType `elem` supportedManifestIndexKinds) $
      fail $
        "MediaType of: " <> (toString mediaType) <> " is not supported."

    OciManifestIndex <$> o .: "manifests"

data OciManifestIndexItem = OciManifestIndexItem
  { ociManifestItemDigest :: RepoDigest
  , ociManifestItemPlatformOs :: Text
  , ociManifestItemPlatformArch :: Text
  }
  deriving (Show, Eq, Ord)

instance FromJSON OciManifestIndexItem where
  parseJSON = withObject "OciManifestIndexItem" $ \o -> do
    digest <- RepoDigest <$> o .: "digest"

    mediaType :: Text <- o .: "mediaType"
    unless (mediaType `elem` supportedManifestKinds) $
      fail $
        "MediaType of: " <> (toString mediaType) <> " is not supported!"

    platform <- o .: "platform"
    os <- platform .: "os"
    arch <- platform .: "architecture"
    pure $ OciManifestIndexItem digest os arch

digestOf :: OciManifestIndex -> Text -> Maybe RepoReference
digestOf (OciManifestIndex entries) arch =
  RepoReferenceDigest
    . ociManifestItemDigest
    <$> find (\e -> ociManifestItemPlatformArch e == arch) entries
