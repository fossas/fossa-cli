{-# LANGUAGE QuasiQuotes #-}

module Container.Docker.CredentialsSpec (spec) where

import Container.Docker.Credentials (
  DockerConfig (..),
  DockerConfigRawAuth (..),
  DockerCredentialHelperGetResponse (..),
  getRawCred,
 )
import Data.Aeson (decodeStrict)
import Data.ByteString (ByteString)
import Data.Map qualified as Map
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Text.RawString.QQ (r)

dockerConfig :: ByteString
dockerConfig =
  [r|{
  "currentContext": "default",
  "credsStore": "desktop",
  "auths": {
    "quay.io": {},
    "https://index.docker.io/v1/": {}
  }
}|]

dockerConfigRawAuth :: ByteString
dockerConfigRawAuth =
  [r|{
  "auths": {
    "gcr.io": {
      "auth": "c29tZVVzZXI6c29tZVBhc3M="
    }
  }
}|]

expectedDockerConfigRawAuth :: DockerConfig
expectedDockerConfigRawAuth =
  DockerConfig
    Nothing
    ( Map.fromList
        [ ("gcr.io", DockerConfigRawAuth (Just "c29tZVVzZXI6c29tZVBhc3M="))
        ]
    )
    mempty

dockerCredHelperResponse :: ByteString
dockerCredHelperResponse = [r|{"ServerURL":"index.docker.io","Username":"user","Secret":"ohno!"}|]

spec :: Spec
spec = do
  describe "Docker Credentials" $ do
    it "should parse docker config file" $ do
      case decodeStrict dockerConfig of
        Nothing -> expectationFailure "Failed to parse docker config"
        Just config ->
          config
            `shouldBe` DockerConfig
              (Just "desktop")
              ( Map.fromList
                  [ ("quay.io", DockerConfigRawAuth Nothing)
                  , ("https://index.docker.io/v1/", DockerConfigRawAuth Nothing)
                  ]
              )
              mempty

      case decodeStrict dockerConfigRawAuth of
        Nothing -> expectationFailure "Failed to parse docker config"
        Just config -> config `shouldBe` expectedDockerConfigRawAuth

    it "should should decode raw authentication values" $ do
      getRawCred "gcr.io" expectedDockerConfigRawAuth `shouldBe` Right ("someUser", "somePass")

    it "should parse docker credential helper's response" $ do
      case decodeStrict dockerCredHelperResponse of
        Nothing -> expectationFailure "Failed to parse docker-cred helper response"
        Just config -> config `shouldBe` DockerCredentialHelperGetResponse "user" "ohno!"
