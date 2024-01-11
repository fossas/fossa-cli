{-# LANGUAGE QuasiQuotes #-}

module Maven.PluginSpec (spec) where

import Data.Text (Text)
import Data.Tree (Tree (..))
import Strategy.Maven.Plugin (Artifact (..), Edge (..), PluginOutput (..), textArtifactToPluginOutput)
import Strategy.Maven.PluginTree (TextArtifact (..), parseTextArtifact)
import Test.Effect (expectationFailure', it', shouldBe', shouldContain', shouldMatchList', shouldSatisfy')
import Test.Hspec (Spec, describe, shouldSatisfy)
import Text.Megaparsec (parseMaybe)
import Text.RawString.QQ (r)
import Control.Effect.Lift (sendIO)
import Data.String.Conversion (toText)
import Effect.ReadFS (readContentsParser)
import Path (parseAbsFile)

spec :: Spec
spec = do
  textArtifactConversionSpec

singleTextArtifact :: TextArtifact
singleTextArtifact =
  TextArtifact
    { artifactText = "org.clojure:clojure:1.12.0-master-SNAPSHOT"
    , groupId = "org.clojure"
    , artifactId = "clojure"
    , textArtifactVersion = "1.12.0-master-SNAPSHOT"
    , scopes = ["test"]
    , isDirect = True
    , isOptional = False
    }

complexTextArtifact :: Tree TextArtifact
complexTextArtifact =
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
          { artifactText = "org.fake:fake-pkg:1.0.0"
          , groupId = "org.fake"
          , artifactId = "fake-pkg"
          , textArtifactVersion = "1.0.0"
          , scopes = ["compile"]
          , isDirect = False
          , isOptional = True
          }
        []
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
    , Node
        TextArtifact
          { artifactText = "org.clojure:data.generators:1.0.0"
          , groupId = "org.clojure"
          , artifactId = "data.generators"
          , textArtifactVersion = "1.0.0"
          , isDirect = False
          , scopes = ["test"]
          , isOptional = False
          }
        []
    ]

complexPluginOutputArtifacts :: PluginOutput
complexPluginOutputArtifacts =
  PluginOutput
    { outArtifacts =
        [ Artifact
            { artifactNumericId = 0
            , artifactGroupId = "org.clojure"
            , artifactArtifactId = "data.generators"
            , artifactVersion = "1.0.0"
            , artifactScopes = ["test"]
            , artifactOptional = False
            , artifactIsDirect = False
            }
        , Artifact
            { artifactNumericId = 1
            , artifactGroupId = "org.baz"
            , artifactArtifactId = "buzz"
            , artifactVersion = "1.0.0"
            , artifactScopes = ["test"]
            , artifactOptional = False
            , artifactIsDirect = False
            }
        , Artifact
            { artifactNumericId = 2
            , artifactGroupId = "org.foo"
            , artifactArtifactId = "bar"
            , artifactVersion = "1.0.0"
            , artifactScopes = ["compile"]
            , artifactOptional = False
            , artifactIsDirect = False
            }
        , Artifact
            { artifactNumericId = 3
            , artifactGroupId = "org.fake"
            , artifactArtifactId = "fake-pkg"
            , artifactVersion = "1.0.0"
            , artifactScopes = ["compile"]
            , artifactOptional = True
            , artifactIsDirect = False
            }
        , Artifact
            { artifactNumericId = 4
            , artifactGroupId = "org.clojure"
            , artifactArtifactId = "test.generative"
            , artifactVersion = "1.0.0"
            , artifactScopes = ["test"]
            , artifactOptional = False
            , artifactIsDirect = True
            }
        ]
    , outEdges =
        [ Edge 2 1
        , Edge 4 3
        , Edge 4 2
        , Edge 4 0
        ]
    }

textArtifactConversionSpec :: Spec
textArtifactConversionSpec =
  describe "Maven text artifact -> PluginOutput conversion" $ do
    it' "Converts a single TextArtifact correctly" $ do
      pluginOutput <- textArtifactToPluginOutput (Node singleTextArtifact [])
      pluginOutput
        `shouldBe'` PluginOutput
          { outArtifacts = [simpleArtifact]
          , outEdges = []
          }

    it' "Converts a more complex TextArtifact correctly" $ do
      PluginOutput{outArtifacts = resArts, outEdges = resEdges} <- textArtifactToPluginOutput complexTextArtifact
      resArts `shouldMatchList'` (outArtifacts complexPluginOutputArtifacts)
      resEdges `shouldMatchList'` (outEdges complexPluginOutputArtifacts)

    it' "should correctly include dependency with multiple scopes" $ do
      let maybeArtifactTree = mkTreeTextArtifact depWithMultipleScopes
      case maybeArtifactTree of
        Nothing -> expectationFailure' "could not parse raw tree output!"
        Just tree' -> do
          PluginOutput{outArtifacts = resArts} <- textArtifactToPluginOutput tree'
          resArts `shouldContain'` [kafkaClientCompile]
          resArts `shouldContain'` [kafkaClientTest]

    it' "TEST TEST TEST" $ do
      input <- sendIO $ parseAbsFile "/home/leo/tmp/zd-7531/dependency-graph.txt"
      parsed <- readContentsParser parseTextArtifact input
      output <- textArtifactToPluginOutput parsed
      output `shouldSatisfy'` (const True)

simpleArtifact :: Artifact
simpleArtifact =
  Artifact
    { artifactNumericId = 0
    , artifactGroupId = "org.clojure"
    , artifactArtifactId = "clojure"
    , artifactVersion = "1.12.0-master-SNAPSHOT"
    , artifactOptional = False
    , artifactScopes = ["test"]
    , artifactIsDirect = True
    }

mkTreeTextArtifact :: Text -> Maybe (Tree TextArtifact)
mkTreeTextArtifact = parseMaybe parseTextArtifact

depWithMultipleScopes :: Text
depWithMultipleScopes =
  [r|com.mycompany.app:my-app:1.0-SNAPSHOT:compile
+- junit:junit:4.11:test
|  \- org.hamcrest:hamcrest-core:1.3:test
+- org.apache.kafka:kafka-clients:3.0.2:compile
|  +- com.github.luben:zstd-jni:1.5.0-2:runtime
|  +- org.lz4:lz4-java:1.7.1:runtime
|  +- org.xerial.snappy:snappy-java:1.1.8.1:runtime
|  \- org.slf4j:slf4j-api:1.7.30:runtime
+- org.apache.kafka:kafka-clients:3.0.2:test
\- joda-time:joda-time:2.9.2:compile|]

kafkaClientCompile :: Artifact
kafkaClientCompile = Artifact 6 "org.apache.kafka" "kafka-clients" "3.0.2" ["compile"] False False

kafkaClientTest :: Artifact
kafkaClientTest = kafkaClientCompile{artifactNumericId = 1, artifactScopes = ["test"]}
