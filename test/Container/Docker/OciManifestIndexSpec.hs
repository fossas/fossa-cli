module Container.Docker.OciManifestIndexSpec (spec) where

import Container.Docker.OciManifestIndex (
  OciManifestIndex (..),
  OciManifestIndexItem (OciManifestIndexItem),
 )
import Container.Docker.SourceParser (RepoDigest (RepoDigest))
import Data.Aeson (decodeFileStrict')
import Data.List.NonEmpty qualified as NonEmpty
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Oci Manifest Index" $ do
    it "should parse oci manifest list" $ do
      manifestIndex <- decodeFileStrict' "test/Container/Docker/testdata/ociManifestIndex.json"
      manifestIndex `shouldBe` Just expectedManifestIndex

expectedManifestIndex :: OciManifestIndex
expectedManifestIndex =
  OciManifestIndex $
    NonEmpty.fromList
      [ OciManifestIndexItem
          (RepoDigest "sha256:ea8176d5185642c6e34063eac2d1c0edb5dcdff8f678028db14b0dad960ee0b5")
          "linux"
          "arm64"
      ]
