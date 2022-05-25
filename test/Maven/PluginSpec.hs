{-# LANGUAGE QuasiQuotes #-}

module Maven.PluginSpec (spec) where

import Data.Aeson (decode)
import Data.String.Conversion (encodeUtf8)
import Data.Text (Text)
import Strategy.Maven.Plugin (Artifact (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)

jsonArtifact :: Text
jsonArtifact =
  [r|{
    "id" : "org.clojure:clojure:jar",
    "numericId" : 1,
    "groupId" : "org.clojure",
    "artifactId" : "clojure",
    "version" : "1.12.0-master-SNAPSHOT",
    "optional" : false,
    "scopes" : [ "compile" ],
    "types" : [ "jar" ]
  }|]

spec :: Spec
spec = do
  parseJsonArtifactSpec

parseJsonArtifactSpec :: Spec
parseJsonArtifactSpec = do
  describe "json Artifact parsing" $ do
    it "Parses an artifact from JSON" $
      (decode . encodeUtf8 $ jsonArtifact) `shouldBe` Just simpleArtifact

simpleArtifact :: Artifact
simpleArtifact =
  Artifact
    { artifactNumericId = 0
    , artifactGroupId = "org.clojure"
    , artifactArtifactId = "clojure"
    , artifactVersion = "1.12.0-master-SNAPSHOT"
    , artifactOptional = False
    , artifactScopes = ["compile"]
    }
