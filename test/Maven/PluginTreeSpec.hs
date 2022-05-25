module Maven.PluginTreeSpec (spec) where

import Data.Text (Text)
import Strategy.Maven.PluginTree (Artifact (..), parseArtifact)
import Test.Hspec (Spec, shouldBe, it)
import Text.Megaparsec (ParseErrorBundle, Parsec, runParser)

-- TODO: test edgecase where a dep is optional in one part of the
-- maven-depgraph-plugin output but required in another

shouldParse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
shouldParse parser = runParser parser ""

to :: (Show a1, Show a2, Eq a2) => Either a1 a2 -> a2 -> IO ()
to parsed expected = either (fail . show) (`shouldBe` expected) parsed

spec :: Spec
spec = parseTextArtifactSpec

simpleArtifactString :: Text
simpleArtifactString = "org.clojure:clojure:1.12.0-master-SNAPSHOT:compile "

multiScopeArtifactString :: Text
multiScopeArtifactString = "org.clojure:clojure:1.12.0-master-SNAPSHOT:compile/test "

optionalArtifactString :: Text
optionalArtifactString = "jakarta.mail:jakarta.mail-api:2.0.1:compile (optional)"

simpleArtifact :: Artifact
simpleArtifact =
  Artifact
    { artifactGroupId = "org.clojure"
    , artifactArtifactId = "clojure"
    , artifactVersion = "1.12.0-master-SNAPSHOT"
    , artifactOptional = False
    , artifactScopes = ["compile"]
    }

multiScopeArtifact :: Artifact
multiScopeArtifact =
  Artifact
    { artifactGroupId = "org.clojure"
    , artifactArtifactId = "clojure"
    , artifactVersion = "1.12.0-master-SNAPSHOT"
    , artifactOptional = False
    , artifactScopes = ["compile", "test"]
    }

optionalArtifact :: Artifact
optionalArtifact =
  Artifact
    { artifactGroupId = "jakarta.mail"
    , artifactArtifactId = "jakarta.mail-api"
    , artifactVersion = "2.0.1"
    , artifactScopes = ["compile"]
    , artifactOptional = True
    }

parseTextArtifactSpec :: Spec
parseTextArtifactSpec = do
  it "Parses an artifact from a string" $
    parseArtifact `shouldParse` simpleArtifactString `to` simpleArtifact
  it "Parses an artifact with multiple scopes a string" $
    parseArtifact `shouldParse` multiScopeArtifactString `to` multiScopeArtifact
  it "Parses an optional artifact from a string" $
    parseArtifact `shouldParse` optionalArtifactString `to` optionalArtifact
