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
import Container.Docker.SourceParser (RegistryImageSource (..), RepoDigest (RepoDigest), RepoReference (RepoReferenceTag), RepoTag (RepoTag), defaultHttpScheme, defaultRegistry)
import Data.Aeson (Value, decodeFileStrict', eitherDecode, encode, object, (.=))
import Data.Either (isLeft, isRight)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
  describe "Oci Manifest" $ do
    it "should parse oci manifest" $ do
      ociManifest <- decodeFileStrict' "test/Container/Docker/testdata/ociManifest.json"
      ociManifest `shouldBe` Just expectedOciManifest

  describe "content digests" $ do
    -- The encoded part of each digest becomes the file name the blob is written to,
    -- so a registry must not be able to put a path separator or `..` in it.
    it "should reject a layer digest that traverses out of the export directory" $
      decodeManifest "sha256:config" "sha256:../../../../tmp/escaped" `shouldSatisfy` isLeft

    it "should reject a config digest containing a path separator" $
      decodeManifest "sha256:etc/escaped" "sha256:layer" `shouldSatisfy` isLeft

    it "should reject a digest without an algorithm" $
      decodeManifest "config" "sha256:layer" `shouldSatisfy` isLeft

    it "should accept well formed digests" $ do
      decodeManifest "sha256:9f2c0f4e2ab5b4aa6c8ad0d1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1" "sha256:layer" `shouldSatisfy` isRight
      decodeManifest "sha256+b64u:LCa0a2j_xo_5m0U8HTBBNBNCLXBkg7-g-YpeiGJm564=" "sha512:layer" `shouldSatisfy` isRight

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
                ["quay.io/library/redis:alpine"]
                $ NonEmpty.fromList ["layer.tar"]
            ]
        )

-- | A manifest in the registry's own format, with the given config and layer digests.
decodeManifest :: Text -> Text -> Either String OciManifestV2
decodeManifest configDigest layerDigest =
  eitherDecode . encode $
    object
      [ "schemaVersion" .= (2 :: Int)
      , "config" .= object ["mediaType" .= ("application/vnd.docker.container.image.v1+json" :: Text), "digest" .= configDigest]
      , "layers" .= [object ["mediaType" .= ("application/vnd.docker.image.rootfs.diff.tar.gzip" :: Text), "digest" .= layerDigest] :: Value]
      ]

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
defaultRegistrySrc = mkRegistrySrc defaultRegistry

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
