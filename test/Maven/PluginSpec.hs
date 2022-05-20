{-# LANGUAGE QuasiQuotes #-}

module Maven.PluginSpec (spec) where

import Data.Aeson (decode)
import Data.String.Conversion (encodeUtf8)
import Data.Text (Text)
import Strategy.Maven.Plugin (Artifact (..), parseArtifact)
import Test.Hspec (Spec, describe, fcontext, it, shouldBe)
import Text.Megaparsec (ParseErrorBundle, Parsec, runParser)
import Text.RawString.QQ (r)

shouldParse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
shouldParse parser = runParser parser ""

to :: (Show a1, Show a2, Eq a2) => Either a1 a2 -> a2 -> IO ()
to parsed expected = either (fail . show) (`shouldBe` expected) parsed

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
  parseTextArtifactSpec

parseJsonArtifactSpec :: Spec
parseJsonArtifactSpec = do
  describe "json Artifact parsing" $ do
    it "Parses an artifact from JSON" $
      (decode . encodeUtf8 $ jsonArtifact) `shouldBe` Just simpleArtifact

simpleArtifactString :: Text
simpleArtifactString = "org.clojure:clojure:1.12.0-master-SNAPSHOT:compile "

optionalArtifactString :: Text
optionalArtifactString = "jakarta.mail:jakarta.mail-api:2.0.1:compile (optional)"

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

optionalArtifact :: Artifact
optionalArtifact =
  Artifact
    { artifactNumericId = 0
    , artifactGroupId = "jakarta.mail"
    , artifactArtifactId = "jakarta.mail-api"
    , artifactVersion = "2.0.1"
    , artifactScopes = ["compile"]
    , artifactOptional = True
    }

parseTextArtifactSpec :: Spec
parseTextArtifactSpec = do
  it "Parses an artifact from a string" $
    parseArtifact 0 `shouldParse` simpleArtifactString `to` simpleArtifact
  it "Parses an optional artifact from a string" $
    parseArtifact 0 `shouldParse` optionalArtifactString `to` optionalArtifact
