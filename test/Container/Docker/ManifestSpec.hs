module Container.Docker.ManifestSpec (spec) where

import Container.Docker.Manifest (ManifestJson (..), ManifestJsonImageEntry (..))
import Data.Aeson (decodeFileStrict')
import Data.List.NonEmpty qualified as NonEmpty
import Test.Hspec (Spec, describe, it, shouldBe)

expectedManifestJson :: ManifestJson
expectedManifestJson =
  ManifestJson $
    NonEmpty.fromList
      [ ManifestJsonImageEntry
          "0ac33e5f5afa79e084075e8698a22d574816eea8d7b7d480586835657c3e1c8b.json"
          ["alpine:latest"]
          $ NonEmpty.fromList ["0e29dcfcf3a535424b4d3e373a006d31521eb2630d340f842d530b0b28582bb0/layer.tar"]
      , ManifestJsonImageEntry 
          "e04c818066afe78a0c9379f62ec65aece28566024fd348242de92760293454b8.json" 
          ["alpine:3.14"] 
          $ NonEmpty.fromList ["8bda15652bf72885f009c3cc3d623a11a25bec6a362905b23576bad0e8b74f33/layer.tar"]
      , ManifestJsonImageEntry 
          "43773d1dba76c4d537b494a8454558a41729b92aa2ad0feb23521c3e58cd0440.json" 
          ["alpine:3.6"] 
          $ NonEmpty.fromList ["eea126ee9bd5ff1ae7944088f4d6b32abb9874a6b00935646b5526e1b57ceb86/layer.tar"]
      ]

spec :: Spec
spec = do
  describe "Docker Manifest" $
    it "should parse manifest file with multiple entries" $ do
      manifest <- decodeFileStrict' "test/Container/Docker/testdata/manifest.json"
      manifest `shouldBe` Just expectedManifestJson
