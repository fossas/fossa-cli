module Container.Docker.OciManifestSpec (spec) where

import Container.Docker.Manifest (
  ManifestJson (ManifestJson),
  ManifestJsonImageEntry (ManifestJsonImageEntry),
 )
import Container.Docker.OciManifest (
  LayerKind (LayerDockerRootFsDiffTarGz),
  OciManifestConfig (OciManifestConfig),
  OciManifestLayer (OciManifestLayer),
  OciManifestV2 (OciManifestV2),
  toDockerManifest,
 )
import Container.Docker.SourceParser (RegistryImageSource (..), RepoDigest (RepoDigest), RepoReference (RepoReferenceTag), RepoTag (RepoTag), defaultHttpScheme, dockerHubRegistry)
import Data.Aeson (decodeFileStrict')
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Oci Manifest" $ do
    it "should parse oci manifest" $ do
      ociManifest <- decodeFileStrict' "test/Container/Docker/testdata/ociManifest.json"
      ociManifest `shouldBe` Just expectedOciManifest

  describe "toDockerManifest" $ do
    it "should convert to docker manifest" $ do
      toDockerManifest expectedOciManifest (defaultRegistrySrc "org/img" latestTag)
        `shouldBe` ManifestJson
          ( NonEmpty.fromList
              [ ManifestJsonImageEntry
                  "config.json"
                  ["org/img:latest"]
                  $ NonEmpty.fromList ["layer.tar"]
              ]
          )

  it "should remove library/ for official docker images" $ do
    toDockerManifest expectedOciManifest (defaultRegistrySrc "library/redis" alpineTag)
      `shouldBe` ManifestJson
        ( NonEmpty.fromList
            [ ManifestJsonImageEntry
                "config.json"
                ["redis:alpine"]
                $ NonEmpty.fromList ["layer.tar"]
            ]
        )

  it "should not remove library/ from private registries" $ do
    toDockerManifest expectedOciManifest (mkRegistrySrc "quay.io" "library/redis" alpineTag)
      `shouldBe` ManifestJson
        ( NonEmpty.fromList
            [ ManifestJsonImageEntry
                "config.json"
                ["library/redis:alpine"]
                $ NonEmpty.fromList ["layer.tar"]
            ]
        )
mkRegistrySrc :: Text -> Text -> RepoReference -> RegistryImageSource
mkRegistrySrc host repo ref =
  RegistryImageSource
    host
    defaultHttpScheme
    Nothing
    repo
    ref
    mempty

defaultRegistrySrc :: Text -> RepoReference -> RegistryImageSource
defaultRegistrySrc = mkRegistrySrc dockerHubRegistry

alpineTag :: RepoReference
alpineTag = (RepoReferenceTag . RepoTag $ "alpine")

latestTag :: RepoReference
latestTag = (RepoReferenceTag . RepoTag $ "latest")

expectedOciManifest :: OciManifestV2
expectedOciManifest =
  OciManifestV2
    ( OciManifestConfig $
        RepoDigest "sha256:config"
    )
    ( NonEmpty.fromList
        [ OciManifestLayer
            (RepoDigest "sha256:layer")
            LayerDockerRootFsDiffTarGz
        ]
    )
