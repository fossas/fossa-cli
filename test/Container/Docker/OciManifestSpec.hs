module Container.Docker.OciManifestSpec (spec) where

import Container.Docker.OciManifest (
  LayerKind (LayerDockerRootFsDiffTarGz),
  OciManifestConfig (OciManifestConfig),
  OciManifestIndex (..),
  OciManifestIndexItem (OciManifestIndexItem),
  OciManifestLayer (OciManifestLayer),
  OciManifestV2 (OciManifestV2),
 )
import Data.Aeson (decodeFileStrict')
import Data.List.NonEmpty qualified as NonEmpty
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Oci Manifest" $ do
    it "should parse oci manifest" $ do
      ociManifest <- decodeFileStrict' "test/Container/Docker/testdata/ociManifest.json"
      ociManifest `shouldBe` Just expectedOciManifest

    it "should parse oci manifest list" $ do
      manifests <- decodeFileStrict' "test/Container/Docker/testdata/ociManifestIndex.json"
      manifests `shouldBe` Just expectedManifestList

expectedOciManifest :: OciManifestV2
expectedOciManifest =
  OciManifestV2
    (OciManifestConfig "sha256:09743aaf7c328c517bc0e9a7f0008802a1e85a73bb7772aaff2fb9370e3e0367")
    ( NonEmpty.fromList
        [ OciManifestLayer
            "sha256:88ecf269dec31566a8e6b05147732fe34d32bc608de0d636dffaba659230a515"
            LayerDockerRootFsDiffTarGz
        ]
    )

expectedManifestList :: OciManifestIndex
expectedManifestList =
  OciManifestIndex $
    NonEmpty.fromList
      [ OciManifestIndexItem
          "sha256:ea8176d5185642c6e34063eac2d1c0edb5dcdff8f678028db14b0dad960ee0b5"
          "linux"
          "arm64"
      ]
