module Container.Docker.ImageJsonSpec (spec) where

import Container.Docker.ImageJson (ImageJson (..), ImageJsonRootFs (..))
import Data.Aeson (decodeFileStrict')
import Data.List.NonEmpty qualified as NonEmpty
import Test.Hspec (Spec, describe, it, shouldBe)

expectedImageJson :: ImageJson
expectedImageJson =
  ImageJson . ImageJsonRootFs $
    NonEmpty.fromList
      [ "sha256:c6f988f4874bb0add23a778f753c65efe992244e148a1d2ec2a8b664fb66bbd1"
      , "sha256:5f70bf18a086007016e948b04aed3b82103a36bea41755b6cddfaf10ace3c6ef"
      ]

spec :: Spec
spec = do
  describe "Docker Image Description Json" $
    it "should parse image description json" $ do
      imgJson <- decodeFileStrict' "test/Container/Docker/testdata/config.json"
      imgJson `shouldBe` Just expectedImageJson
