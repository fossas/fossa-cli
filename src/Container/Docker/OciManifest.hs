module Container.Docker.OciManifest (
  OciManifestV2 (..),
  LayerKind (..),
  OciManifestConfig (..),
  OciManifestLayer (..),
  NotSupportedManifestFmt (..),
  toDockerManifest,
  supportedManifestKinds,
  blobEntries,
) where

import Container.Docker.Manifest (
  ManifestJson (ManifestJson),
  ManifestJsonImageEntry (ManifestJsonImageEntry),
 )
import Container.Docker.SourceParser (
  RegistryImageSource (RegistryImageSource),
  RepoDigest (RepoDigest),
  dockerHubRegistry,
  showReferenceWithSep,
  suggestDockerExport,
 )
import Control.Effect.Diagnostics (ToDiagnostic, renderDiagnostic)
import Data.Aeson (FromJSON (parseJSON), withObject, withText, (.:))
import Data.List.NonEmpty ((<|))
import Data.List.NonEmpty qualified as NonEmpty
import Data.String.Conversion (toString)
import Data.Text (Text)
import Data.Text qualified as Text
import Effect.Logger (vsep)
import Prettyprinter (indent, line, pretty)

supportedManifestKinds :: [Text]
supportedManifestKinds =
  [ "application/vnd.oci.image.manifest.v1+json"
  , "application/vnd.docker.distribution.manifest.v2+json"
  ]

-- | Oci Manifest.
--
-- Image manifest retrieved from container registry.
--
-- Refer to:
--  https://github.com/opencontainers/image-spec/blob/main/manifest.md#oci-image-manifest-specification
--  https://docs.docker.com/registry/spec/manifest-v2-2/
data OciManifestV2 = OciManifestV2
  { ociConfig :: OciManifestConfig
  , ociLayers :: NonEmpty.NonEmpty OciManifestLayer
  }
  deriving (Show, Eq, Ord)

instance FromJSON OciManifestV2 where
  parseJSON = withObject "Manifest" $ \o ->
    OciManifestV2 <$> o .: "config" <*> o .: "layers"

newtype OciManifestConfig = OciManifestConfig {configDigest :: RepoDigest} deriving (Show, Eq, Ord)
instance FromJSON OciManifestConfig where
  parseJSON = withObject "ConfigEntry" $ \o -> OciManifestConfig . RepoDigest <$> o .: "digest"

data OciManifestLayer = OciManifestLayer
  { layerDigest :: RepoDigest
  , layerKind :: LayerKind
  }
  deriving (Show, Eq, Ord)

instance FromJSON OciManifestLayer where
  parseJSON = withObject "LayerEntry" $ \o ->
    OciManifestLayer
      <$> (RepoDigest <$> o .: "digest")
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

instance FromJSON LayerKind where
  parseJSON = withText "ErrorCode" $ \t -> case mediaTypeToLayerKind (toString t) of
    Nothing -> fail $ "Provided Layer Kind is not supported: " <> (toString t)
    Just layerKind -> pure layerKind

mediaTypeToLayerKind :: String -> Maybe LayerKind
mediaTypeToLayerKind mediaType | show LayerDockerRootFsDiffTarGz == mediaType = Just LayerDockerRootFsDiffTarGz
mediaTypeToLayerKind mediaType | show LayerDockerRootForeignFsDiffTarGz == mediaType = Just LayerDockerRootForeignFsDiffTarGz
mediaTypeToLayerKind mediaType | show LayerOCITar == mediaType = Just LayerOCITar
mediaTypeToLayerKind mediaType | show LayerOCITarGz == mediaType = Just LayerOCITarGz
mediaTypeToLayerKind mediaType | show LayerOCINonDistributableTar == mediaType = Just LayerOCINonDistributableTar
mediaTypeToLayerKind mediaType | show LayerOCINonDistributableTarGz == mediaType = Just LayerOCINonDistributableTarGz
mediaTypeToLayerKind _ = Nothing

-- | Is Layer GZip Compressed? True is Gzip Compressed, otherwise False.
isGzipKind :: LayerKind -> Bool
isGzipKind LayerOCITarGz = True
isGzipKind LayerOCINonDistributableTarGz = True
isGzipKind LayerDockerRootFsDiffTarGz = True
isGzipKind LayerDockerRootForeignFsDiffTarGz = True
isGzipKind _ = False

-- | Converts Oci Manifest To Docker Manifest Json.
toDockerManifest :: OciManifestV2 -> RegistryImageSource -> ManifestJson
toDockerManifest (OciManifestV2 config layers) (RegistryImageSource host _ _ repo ref _) =
  ManifestJson $
    NonEmpty.singleton $
      ManifestJsonImageEntry
        (mkConfigFileName config)
        [repoTag]
        (NonEmpty.map (toString . mkLayerTarFileName) layers)
  where
    repoTag :: Text
    repoTag = sanitizeRepoName <> showReferenceWithSep ref

    sanitizeRepoName :: Text
    sanitizeRepoName =
      if ( Text.isPrefixOf "library/" repo
            && host == dockerHubRegistry
         )
        then Text.replace "library/" "" repo
        else repo

-- | Gets image artifacts blobs.
blobEntries :: OciManifestV2 -> NonEmpty.NonEmpty (RepoDigest, Bool, Text)
blobEntries (OciManifestV2 config layers) = configBlobEntry <| (NonEmpty.map layerToBlobEntry layers)
  where
    configBlobEntry :: (RepoDigest, Bool, Text)
    configBlobEntry =
      ( configDigest config
      , False
      , mkConfigFileName config
      )

    layerToBlobEntry :: OciManifestLayer -> (RepoDigest, Bool, Text)
    layerToBlobEntry layer =
      ( layerDigest layer
      , isGzipKind . layerKind $ layer
      , mkLayerTarFileName layer
      )

mkConfigFileName :: OciManifestConfig -> Text
mkConfigFileName (OciManifestConfig (RepoDigest digest)) = removeDigestAlgorithm digest <> ".json"

mkLayerTarFileName :: OciManifestLayer -> Text
mkLayerTarFileName (OciManifestLayer (RepoDigest digest) _) = removeDigestAlgorithm digest <> ".tar"

-- | Removes digest algorithm (if any)
--
-- >> removeDigestAlgorithm "sha256:digest" == "digest"
-- >> removeDigestAlgorithm "sha512:digest" == "digest"
-- >> removeDigestAlgorithm "sha256+b64u:digest" = "digest"
-- >> removeDigestAlgorithm "digest" = "digest"
--
-- Refer to:
--  https://github.com/opencontainers/image-spec/blob/main/descriptor.md#digests
--
-- Note:
--  In practice, all public and private repositories use sha256.
removeDigestAlgorithm :: Text -> Text
removeDigestAlgorithm = snd . Text.breakOnEnd ":"

data NotSupportedManifestFmt = NotSupportedManifestFmt Text RegistryImageSource

instance ToDiagnostic NotSupportedManifestFmt where
  renderDiagnostic (NotSupportedManifestFmt fmt imgSrc) =
    vsep
      [ pretty $ "We cannot process, manifest in format of: " <> fmt
      , line <> "Workaround:" <> line
      , indent 2 $
          vsep
            [ "You can use exported tarball to perform analysis:"
            , line
            , suggestDockerExport imgSrc
            ]
      ]
