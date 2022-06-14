{-# LANGUAGE QuasiQuotes #-}

module Maven.PluginTreeSpec (spec) where

import Data.Text (Text)
import Data.Tree (Tree (..))
import Strategy.Maven.PluginTree (TextArtifact (..), parseTextArtifact)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (ParseErrorBundle, Parsec, runParser)
import Text.RawString.QQ (r)

shouldParse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
shouldParse parser = runParser parser ""

to :: (Show a1, Show a2, Eq a2) => Either a1 a2 -> a2 -> IO ()
to parsed expected = either (fail . show) (`shouldBe` expected) parsed

spec :: Spec
spec = do parseTextArtifactSpec

mockArtifactString :: Text
mockArtifactString = "org.clojure:clojure:1.12.0-master-SNAPSHOT:test"

multiScopeArtifactString :: Text
multiScopeArtifactString = "org.clojure:clojure:1.12.0-master-SNAPSHOT:compile/test"

optionalArtifactString :: Text
optionalArtifactString = "jakarta.mail:jakarta.mail-api:2.0.1:compile (optional)"

mockArtifact :: Tree TextArtifact
mockArtifact =
  Node
    TextArtifact
      { artifactText = "org.clojure:clojure:1.12.0-master-SNAPSHOT"
      , groupId = "org.clojure"
      , artifactId = "clojure"
      , textArtifactVersion = "1.12.0-master-SNAPSHOT"
      , scopes = ["test"]
      , isDirect = True
      , isOptional = False
      }
    []

multiScopeTextArtifact :: Tree TextArtifact
multiScopeTextArtifact =
  Node
    TextArtifact
      { artifactText = "org.clojure:clojure:1.12.0-master-SNAPSHOT"
      , groupId = "org.clojure"
      , artifactId = "clojure"
      , textArtifactVersion = "1.12.0-master-SNAPSHOT"
      , scopes = ["compile", "test"]
      , isDirect = True
      , isOptional = False
      }
    []

optionalTextArtifact :: Tree TextArtifact
optionalTextArtifact =
  Node
    TextArtifact
      { artifactText = "jakarta.mail:jakarta.mail-api:2.0.1"
      , groupId = "jakarta.mail"
      , artifactId = "jakarta.mail-api"
      , textArtifactVersion = "2.0.1"
      , scopes = ["compile"]
      , isDirect = True
      , isOptional = True
      }
    []

parseTextArtifactSpec :: Spec
parseTextArtifactSpec = describe "Parsing maven artifact text" $ do
  it "Parses a TextArtifact from a string" $
    parseTextArtifact `shouldParse` mockArtifactString `to` mockArtifact
  it "Parses an TextArtifact with multiple scopes from a string" $
    parseTextArtifact `shouldParse` multiScopeArtifactString `to` multiScopeTextArtifact
  it "Parses an optional TextArtifact from a string" $
    parseTextArtifact `shouldParse` optionalArtifactString `to` optionalTextArtifact
  it "Parses a TextArtifact with nested children" $
    parseTextArtifact `shouldParse` artifactTextWithChildren `to` artifactWithChildren

artifactWithChildren :: Tree TextArtifact
artifactWithChildren =
  Node
    TextArtifact
      { artifactText = "org.clojure:test.generative:1.0.0"
      , groupId = "org.clojure"
      , artifactId = "test.generative"
      , textArtifactVersion = "1.0.0"
      , scopes = ["test"]
      , isDirect = True
      , isOptional = False
      }
    [ Node
        TextArtifact
          { artifactText = "org.clojure:tools.namespace:1.0.0"
          , groupId = "org.clojure"
          , artifactId = "tools.namespace"
          , textArtifactVersion = "1.0.0"
          , scopes = ["test"]
          , isOptional = False
          , isDirect = False
          }
        [ Node
            TextArtifact
              { artifactText = "org.clojure:java.classpath:1.0.0"
              , groupId = "org.clojure"
              , artifactId = "java.classpath"
              , textArtifactVersion = "1.0.0"
              , scopes = ["test"]
              , isDirect = False
              , isOptional = False
              }
            []
        , Node
            TextArtifact
              { artifactText = "org.fake:fake-pkg:1.0.0"
              , groupId = "org.fake"
              , artifactId = "fake-pkg"
              , textArtifactVersion = "1.0.0"
              , isDirect = False
              , scopes = ["compile"]
              , isOptional = True
              }
            []
        , Node
            TextArtifact
              { artifactText = "org.clojure:tools.reader:1.3.2"
              , groupId = "org.clojure"
              , artifactId = "tools.reader"
              , textArtifactVersion = "1.3.2"
              , isDirect = False
              , scopes = ["test"]
              , isOptional = False
              }
            []
        ]
    , Node
        TextArtifact
          { artifactText = "org.foo:bar:1.0.0"
          , groupId = "org.foo"
          , artifactId = "bar"
          , textArtifactVersion = "1.0.0"
          , isDirect = False
          , scopes = ["compile"]
          , isOptional = False
          }
        [ Node
            TextArtifact
              { artifactText = "org.baz:buzz:1.0.0"
              , groupId = "org.baz"
              , artifactId = "buzz"
              , textArtifactVersion = "1.0.0"
              , isDirect = False
              , scopes = ["test"]
              , isOptional = False
              }
            []
        ]
    ]

artifactTextWithChildren :: Text
artifactTextWithChildren =
  [r|org.clojure:test.generative:1.0.0:test
+- org.clojure:tools.namespace:1.0.0:test
|  +- org.clojure:java.classpath:1.0.0:test
|  +- org.fake:fake-pkg:1.0.0:compile (optional)
|  \- org.clojure:tools.reader:1.3.2:test
\- org.foo:bar:1.0.0:compile
   \- org.baz:buzz:1.0.0:test|]
