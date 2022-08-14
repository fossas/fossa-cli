module Container.RegistryApiSpec (spec) where

import Container.Docker.OciManifest (OciManifestConfig (configDigest), OciManifestV2 (ociConfig))
import Container.Docker.SourceParser (RegistryImageSource, parseImageUrl)
import Control.Effect.ContainerRegistryApi (getImageManifest)
import Control.Effect.Diagnostics (fromEitherShow)
import Data.Text (Text)
import Data.Void (Void)
import Test.Effect (it', shouldBe')
import Test.Hspec (Spec, describe)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (ParseErrorBundle)

decodeStrict :: Text -> Text -> Either (ParseErrorBundle Text Void) RegistryImageSource
decodeStrict arch = parse (parseImageUrl arch) mempty

spec :: Spec
spec =
  describe "Container Registry API" $ do
    describe "Public Registry APIs" $ do
      describe "Custom Registry" $ do
        it' "should get manifest with tag" $ do
          res <- (getImageManifest =<< fromEitherShow (decodeStrict amd64 githubImage))
          (configDigest . ociConfig $ res) `shouldBe'` githubImageConfigDigest

        it' "should get manifest with digest" $ do
          res <- (getImageManifest =<< fromEitherShow (decodeStrict amd64 githubImageWithDigest))
          (configDigest . ociConfig $ res) `shouldBe'` githubImageConfigDigest

        it' "should get manifest for multi-platform image (chooses target platform)" $ do
          res <- (getImageManifest =<< fromEitherShow (decodeStrict amd64 githubMultiArchImage))
          (configDigest . ociConfig $ res) `shouldBe'` githubMultiArchImageConfigDigest

      describe "Default Index (docker)" $ do
        it' "should get manifest with tag" $ do
          res <- (getImageManifest =<< fromEitherShow (decodeStrict amd64 dhImage))
          (configDigest . ociConfig $ res) `shouldBe'` dhImageDigest

        it' "should get manifest with digest" $ do
          res <- (getImageManifest =<< fromEitherShow (decodeStrict amd64 dhImageWithDigest))
          (configDigest . ociConfig $ res) `shouldBe'` dhImageDigest

        it' "should get manifest for multi-platform image (chooses target platform)" $ do
          res <- (getImageManifest =<< fromEitherShow (decodeStrict arm dhMultiArchImage))
          (configDigest . ociConfig $ res) `shouldBe'` dhMultiArchImageDigest

amd64 :: Text
amd64 = "amd64"

arm :: Text
arm = "arm"

githubImage :: Text
githubImage = "ghcr.io/fossas/haskell-dev-tools:8.10.4"

githubImageConfigDigest :: Text
githubImageConfigDigest = "sha256:e83e5c9e32de2454ef584b4582f7ea171c9c71b5f23c8f13e297a9153e02f4af"

githubImageWithDigest :: Text
githubImageWithDigest = "ghcr.io/fossas/haskell-dev-tools@sha256:35643079905dfdd597fa2cff3b46aadf5378d325a68f00bdb2665c49fa6ca6a2"

githubMultiArchImage :: Text
githubMultiArchImage = "ghcr.io/graalvm/graalvm-ce:ol7-java11-21.3.3"

githubMultiArchImageConfigDigest :: Text
githubMultiArchImageConfigDigest = "sha256:bdcba07acb11053fea0026b807ecf94550ace7df27b10596ca4c765165243cef"

dhImage :: Text
dhImage = "amazon/aws-cli:2.0.6"

dhImageDigest :: Text
dhImageDigest = "sha256:af825d93886a9ce8af4b7b9d36f3becbadef7420f79d2b05a3178ec0ad9e1a21"

dhImageWithDigest :: Text
dhImageWithDigest = "amazon/aws-cli@sha256:7a27c26c2937a3d0b84171675709df1dc09aa331e86cad90f74ada6df7b59c89"

dhMultiArchImage :: Text
dhMultiArchImage = "grafana/grafana:8.1.7-ubuntu"

dhMultiArchImageDigest :: Text
dhMultiArchImageDigest = "sha256:86618e1e78e4962b5abec6cc7fabe89010ebfbbf0885cbba1aada7287457c263"
