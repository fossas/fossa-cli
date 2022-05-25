{-# LANGUAGE QuasiQuotes #-}

module Maven.PluginTreeSpec (spec) where

import Data.Text (Text)
import Strategy.Maven.PluginTree (Artifact (..), TextArtifact (..), parseArtifact, parseTextArtifact)
import Test.Hspec (Spec, fcontext, fit, it, shouldBe)
import Text.Megaparsec (ParseErrorBundle, Parsec, runParser)
import Text.RawString.QQ (r)

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

multiScopeTextArtifact :: TextArtifact
multiScopeTextArtifact =
  TextArtifact
    { artifactText = "org.clojure:clojure:1.12.0-master-SNAPSHOT"
    , scopes = ["compile", "test"]
    , children = []
    -- , isOptional = False
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
parseTextArtifactSpec = fcontext "" $ do
  it "Parses an Artifact from a string" $
    parseArtifact `shouldParse` simpleArtifactString `to` simpleArtifact
  it "Parses an Artifact with multiple scopes a string" $
    parseArtifact `shouldParse` multiScopeArtifactString `to` multiScopeArtifact
  it "Parses an optional Artifact from a string" $
    parseArtifact `shouldParse` optionalArtifactString `to` optionalArtifact
  it "Parses a TextArtifact from a string" $
    parseTextArtifact `shouldParse` multiScopeArtifactString `to` multiScopeTextArtifact
  it "Parses a TextArtifact with children" $
    parseTextArtifact `shouldParse` artifactTextWithChildren `to` artifactWithChildren

artifactWithChildren :: TextArtifact
artifactWithChildren =
  TextArtifact {
  artifactText = "org.clojure:tools.namespace:1.0.0"
  , scopes = ["test"]
  , children = [ TextArtifact {
                   artifactText = "org.clojure:java.classpath:1.0.0"
                   , scopes = ["test"]
                   , children = []}
               , TextArtifact {
                   artifactText = "org.clojure:tools.reader:1.3.2"
                   , scopes = ["test"]
                   , children = []
                   }]
               }

artifactTextWithChildren :: Text
artifactTextWithChildren =
  [r|org.clojure:tools.namespace:1.0.0:test
+- org.clojure:java.classpath:1.0.0:test
\- org.clojure:tools.reader:1.3.2:test |]
