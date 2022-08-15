{-# LANGUAGE QuasiQuotes #-}

module Container.Docker.CredentialsSpec (spec) where

import Container.Docker.Credentials (
  DockerConfig (..),
  DockerCredentialHelperGetResponse (..),
 )
import Data.Aeson (decodeStrict)
import Data.ByteString (ByteString)
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

dockerCredHelperResponse :: ByteString
dockerCredHelperResponse = [r|{"ServerURL":"index.docker.io","Username":"user","Secret":"ohno!"}|]

spec :: Spec
spec = do
  describe "Docker Credentials" $ do
    it "should parse docker config file" $
      case decodeStrict dockerConfig of
        Nothing -> expectationFailure "Failed to parse docker config"
        Just config -> config `shouldBe` DockerConfig "desktop" mempty

    it "should parse docker credential helper's response" $ do
      case decodeStrict dockerCredHelperResponse of
        Nothing -> expectationFailure "Failed to parse docker-cred helper response"
        Just config -> config `shouldBe` DockerCredentialHelperGetResponse "user" "ohno!"
