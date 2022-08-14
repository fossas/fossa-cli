module Container.Docker.OciManifest (
  blobEntries,
  OciManifestIndex (..),
  OciManifestIndexItem (..),
  OciManifestV2 (..),
  LayerKind (..),
  OciManifestConfig (..),
  OciManifestLayer (..),
  toDockerManifest,
  supportedManifestKinds,
  supportedManifestListKinds,
  digestOf,
) where

import Container.Docker.Manifest (ManifestJson (ManifestJson), ManifestJsonImageEntry (ManifestJsonImageEntry))
import Control.Monad (unless)
import Data.Aeson (FromJSON (parseJSON), withObject, withText, (.:))
import Data.Foldable (find)
import Data.List.NonEmpty ((<|))
import Data.List.NonEmpty qualified as NonEmpty
import Data.String.Conversion (toString)
import Data.Text (Text)
import Data.Text qualified as Text

supportedManifestListKinds :: [Text]
supportedManifestListKinds =
  [ "application/vnd.docker.distribution.manifest.list.v2+json"
  , "application/vnd.oci.image.index.v1+json"
  ]

newtype OciManifestIndex = OciManifestIndex (NonEmpty.NonEmpty OciManifestIndexItem) deriving (Show, Eq, Ord)
instance FromJSON OciManifestIndex where
  parseJSON = withObject "OciManifestIndex" $ \o -> do
    mediaType :: Text <- o .: "mediaType"
    unless (mediaType `elem` supportedManifestListKinds) $
      fail $
        "MediaType of: " <> (toString mediaType) <> " is not supported."

    OciManifestIndex <$> o .: "manifests"

data OciManifestIndexItem = OciManifestIndexItem
  { ociManifestItemDigest :: Text
  , ociManifestItemPlatformOs :: Text
  , ociManifestItemPlatformArch :: Text
  }
  deriving (Show, Eq, Ord)

instance FromJSON OciManifestIndexItem where
  parseJSON = withObject "OciManifestIndexItem" $ \o -> do
    digest <- o .: "digest"

    mediaType :: Text <- o .: "mediaType"
    unless (mediaType `elem` supportedManifestKinds) $
      fail $
        "MediaType of: " <> (toString mediaType) <> " is not supported."

    platform <- o .: "platform"
    os <- platform .: "os"
    arch <- platform .: "architecture"
    pure $ OciManifestIndexItem digest os arch

digestOf :: OciManifestIndex -> Text -> Maybe Text
digestOf (OciManifestIndex entries) arch = ociManifestItemDigest <$> find (\e -> ociManifestItemPlatformArch e == arch) entries

supportedManifestKinds :: [Text]
supportedManifestKinds =
  [ "application/vnd.oci.image.manifest.v1+json"
  , "application/vnd.docker.distribution.manifest.v2+json"
  ]

data OciManifestV2 = OciManifestV2
  { ociConfig :: OciManifestConfig
  , ociLayers :: NonEmpty.NonEmpty OciManifestLayer
  }
  deriving (Show, Eq, Ord)

instance FromJSON OciManifestV2 where
  parseJSON = withObject "Manifest" $ \o ->
    OciManifestV2 <$> o .: "config" <*> o .: "layers"

newtype OciManifestConfig = OciManifestConfig {configDigest :: Text} deriving (Show, Eq, Ord)
instance FromJSON OciManifestConfig where
  parseJSON = withObject "ConfigEntry" $ \o -> OciManifestConfig <$> o .: "digest"

data OciManifestLayer = OciManifestLayer
  { layerDigest :: Text
  , layerKind :: LayerKind
  }
  deriving (Show, Eq, Ord)

instance FromJSON OciManifestLayer where
  parseJSON = withObject "LayerEntry" $ \o ->
    OciManifestLayer
      <$> o .: "digest"
      <*> o .: "mediaType"

data LayerKind
  = LayerDockerRootFsDiffTarGz
  | LayerDockerRootForeignFsDiffTarGz
  | LayerOCITar
  | LayerOCITarGz
  | LayerOCINonDistributableTar
  | LayerOCINonDistributableTarGz
  deriving (Eq, Ord)

instance Show LayerKind where
  show LayerDockerRootFsDiffTarGz = "application/vnd.docker.image.rootfs.diff.tar.gzip"
  show LayerDockerRootForeignFsDiffTarGz = "vnd.docker.image.rootfs.foreign.diff.tar.gzip"
  show LayerOCITar = "application/vnd.oci.image.layer.v1.tar"
  show LayerOCITarGz = "application/vnd.oci.image.layer.v1.tar+gzip"
  show LayerOCINonDistributableTar = "application/vnd.oci.image.layer.nondistributable.v1.tar"
  show LayerOCINonDistributableTarGz = "application/vnd.oci.image.layer.nondistributable.v1.tar+gzip"

isGzipKind :: LayerKind -> Bool
isGzipKind LayerOCITarGz = True
isGzipKind LayerOCINonDistributableTarGz = True
isGzipKind LayerDockerRootFsDiffTarGz = True
isGzipKind LayerDockerRootForeignFsDiffTarGz = True
isGzipKind _ = False

mediaTypeToLayerKind :: String -> Maybe LayerKind
mediaTypeToLayerKind mediaType | show LayerDockerRootFsDiffTarGz == mediaType = Just LayerDockerRootFsDiffTarGz
mediaTypeToLayerKind mediaType | show LayerDockerRootForeignFsDiffTarGz == mediaType = Just LayerDockerRootForeignFsDiffTarGz
mediaTypeToLayerKind mediaType | show LayerOCITar == mediaType = Just LayerOCITar
mediaTypeToLayerKind mediaType | show LayerOCITarGz == mediaType = Just LayerOCITarGz
mediaTypeToLayerKind mediaType | show LayerOCINonDistributableTar == mediaType = Just LayerOCINonDistributableTar
mediaTypeToLayerKind mediaType | show LayerOCINonDistributableTarGz == mediaType = Just LayerOCINonDistributableTarGz
mediaTypeToLayerKind _ = Nothing

instance FromJSON LayerKind where
  parseJSON = withText "ErrorCode" $ \t -> case mediaTypeToLayerKind (toString t) of
    Nothing -> fail $ "Provided Layer Kind is not supported: " <> (toString t)
    Just layerKind -> pure layerKind

toDockerManifest :: OciManifestV2 -> Text -> Text -> ManifestJson
toDockerManifest (OciManifestV2 config layers) repo tag =
  ManifestJson $
    NonEmpty.singleton $
      ManifestJsonImageEntry
        (mkConfigFileName config)
        [repoTag]
        (NonEmpty.map (toString . mkLayerTarFileName) layers)
  where
    repoTag :: Text
    repoTag = repo <> ":" <> tag

removeDigest :: Text -> Text
removeDigest = Text.replace "sha256:" ""

mkConfigFileName :: OciManifestConfig -> Text
mkConfigFileName (OciManifestConfig digest) = removeDigest digest <> ".json"

mkLayerTarFileName :: OciManifestLayer -> Text
mkLayerTarFileName (OciManifestLayer digest _) = removeDigest digest <> ".tar"

blobEntries :: OciManifestV2 -> NonEmpty.NonEmpty (Text, Bool, Text)
blobEntries (OciManifestV2 config layers) = configBlobEntry <| (NonEmpty.map layerToBlobEntry layers)
  where
    configBlobEntry :: (Text, Bool, Text)
    configBlobEntry =
      ( configDigest config
      , False
      , mkConfigFileName config
      )

    layerToBlobEntry :: OciManifestLayer -> (Text, Bool, Text)
    layerToBlobEntry layer =
      ( layerDigest layer
      , isGzipKind . layerKind $ layer
      , mkLayerTarFileName layer
      )
