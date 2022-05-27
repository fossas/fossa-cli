{-# LANGUAGE QuasiQuotes #-}

module Maven.PluginTreeSpec (spec) where

import Data.Text (Text)
import Strategy.Maven.PluginTree (TextArtifact (..), parseTextArtifact)
import Test.Hspec (Spec, it, shouldBe)
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

mockArtifactString :: Text
mockArtifactString = "org.clojure:clojure:1.12.0-master-SNAPSHOT:test"

multiScopeArtifactString :: Text
multiScopeArtifactString = "org.clojure:clojure:1.12.0-master-SNAPSHOT:compile/test"

optionalArtifactString :: Text
optionalArtifactString = "jakarta.mail:jakarta.mail-api:2.0.1:compile (optional)"

mockArtifact :: TextArtifact
mockArtifact =
  TextArtifact
    { artifactText = "org.clojure:clojure:1.12.0-master-SNAPSHOT"
    , scopes = ["test"]
    , isOptional = False
    , children = []
    }

multiScopeTextArtifact :: TextArtifact
multiScopeTextArtifact =
  TextArtifact
    { artifactText = "org.clojure:clojure:1.12.0-master-SNAPSHOT"
    , scopes = ["compile", "test"]
    , children = []
    , isOptional = False
    }

optionalTextArtifact :: TextArtifact
optionalTextArtifact =
  TextArtifact
    { artifactText = "jakarta.mail:jakarta.mail-api:2.0.1"
    , scopes = ["compile"]
    , isOptional = True
    , children = []
    }

parseTextArtifactSpec :: Spec
parseTextArtifactSpec = do
  it "Parses a TextArtifact from a string" $
    parseTextArtifact `shouldParse` mockArtifactString `to` mockArtifact
  it "Parses an TextArtifact with multiple scopes from a string" $
    parseTextArtifact `shouldParse` multiScopeArtifactString `to` multiScopeTextArtifact
  it "Parses an optional TextArtifact from a string" $
    parseTextArtifact `shouldParse` optionalArtifactString `to` optionalTextArtifact
  it "Parses a TextArtifact with nested children" $
    parseTextArtifact `shouldParse` artifactTextWithChildren `to` artifactWithChildren

artifactWithChildren :: TextArtifact
artifactWithChildren =
  TextArtifact
    { artifactText = "org.clojure:test.generative:1.0.0"
    , scopes = ["test"]
    , isOptional = False
    , children =
        [ TextArtifact
            { artifactText = "org.clojure:tools.namespace:1.0.0"
            , scopes = ["test"]
            , isOptional = False
            , children =
                [ TextArtifact
                    { artifactText = "org.clojure:java.classpath:1.0.0"
                    , scopes = ["test"]
                    , children = []
                    , isOptional = False
                    }
                , TextArtifact
                    { artifactText = "org.fake:fake-pkg:1.0.0"
                    , scopes = ["compile"]
                    , children = []
                    , isOptional = True
                    }
                , TextArtifact
                    { artifactText = "org.clojure:tools.reader:1.3.2"
                    , scopes = ["test"]
                    , children = []
                    , isOptional = False
                    }
                ]
            }
        , TextArtifact
            { artifactText = "org.foo:bar:1.0.0"
            , scopes = ["compile"]
            , isOptional = False
            , children =
                [ TextArtifact
                    { artifactText = "org.baz:buzz:1.0.0"
                    , scopes = ["test"]
                    , children = []
                    , isOptional = False
                    }
                ]
            }
        ]
    }

artifactTextWithChildren :: Text
artifactTextWithChildren =
  [r|org.clojure:test.generative:1.0.0:test
+- org.clojure:tools.namespace:1.0.0:test
|  +- org.clojure:java.classpath:1.0.0:test
|  +- org.fake:fake-pkg:1.0.0:compile (optional)
|  \- org.clojure:tools.reader:1.3.2:test
\- org.foo:bar:1.0.0:compile
   \- org.baz:buzz:1.0.0:test|]
