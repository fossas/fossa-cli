{-# LANGUAGE QuasiQuotes #-}

module Maven.PluginSpec (spec) where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Text (Text)
import Data.Tree (Tree (..))
import Strategy.Maven.Plugin (
  Artifact (..),
  Edge (..),
  PluginOutput (..),
  VerboseArtifact (..),
  VerboseEdge (..),
  VerboseGraph (..),
  augmentWithDuplicateEdges,
  textArtifactToPluginOutput,
 )
import Strategy.Maven.PluginTree (TextArtifact (..), parseTextArtifact)
import Test.Effect (expectationFailure', it', shouldBe', shouldContain', shouldMatchList')
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parseMaybe)
import Text.RawString.QQ (r)

spec :: Spec
spec = do
  textArtifactConversionSpec
  verboseGraphParsingSpec
  augmentWithDuplicateEdgesSpec

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

-- | depgraph JSON with @showDuplicates=true@. The numeric ids deliberately do
-- not line up: depgraph numbers artifacts and edge endpoints independently, so
-- edges must join to artifacts via the string ids.
verboseGraphJson :: BS.ByteString
verboseGraphJson =
  BS.pack
    [r|{
  "graphName": "example",
  "artifacts": [
    { "id": "org.example:app:jar", "numericId": 7, "groupId": "org.example", "artifactId": "app", "version": "1.0.0", "scopes": [], "types": ["jar"] },
    { "id": "org.apache.poi:poi-ooxml:jar", "numericId": 3, "groupId": "org.apache.poi", "artifactId": "poi-ooxml", "version": "5.2.5", "scopes": ["compile"], "types": ["jar"] },
    { "id": "org.apache.logging.log4j:log4j-api:jar", "numericId": 9, "groupId": "org.apache.logging.log4j", "artifactId": "log4j-api", "version": "2.21.1", "scopes": ["compile"], "types": ["jar"] },
    { "id": "org.apache.logging.log4j:log4j-core:jar", "numericId": 1, "groupId": "org.apache.logging.log4j", "artifactId": "log4j-core", "version": "2.21.1", "scopes": ["compile"], "types": ["jar"] }
  ],
  "dependencies": [
    { "from": "org.example:app:jar", "to": "org.apache.poi:poi-ooxml:jar", "numericFrom": 1, "numericTo": 2, "resolution": "INCLUDED" },
    { "from": "org.example:app:jar", "to": "org.apache.logging.log4j:log4j-core:jar", "numericFrom": 1, "numericTo": 4, "resolution": "INCLUDED" },
    { "from": "org.apache.poi:poi-ooxml:jar", "to": "org.apache.logging.log4j:log4j-api:jar", "numericFrom": 2, "numericTo": 3, "resolution": "INCLUDED" },
    { "from": "org.apache.logging.log4j:log4j-core:jar", "to": "org.apache.logging.log4j:log4j-api:jar", "numericFrom": 4, "numericTo": 3, "resolution": "OMITTED_FOR_DUPLICATE" }
  ]
}|]

expectedVerboseGraph :: VerboseGraph
expectedVerboseGraph =
  VerboseGraph
    { verboseArtifacts =
        [ VerboseArtifact "org.example:app:jar" "org.example" "app" "1.0.0"
        , VerboseArtifact "org.apache.poi:poi-ooxml:jar" "org.apache.poi" "poi-ooxml" "5.2.5"
        , VerboseArtifact "org.apache.logging.log4j:log4j-api:jar" "org.apache.logging.log4j" "log4j-api" "2.21.1"
        , VerboseArtifact "org.apache.logging.log4j:log4j-core:jar" "org.apache.logging.log4j" "log4j-core" "2.21.1"
        ]
    , verboseEdges =
        [ VerboseEdge "org.example:app:jar" "org.apache.poi:poi-ooxml:jar" "INCLUDED"
        , VerboseEdge "org.example:app:jar" "org.apache.logging.log4j:log4j-core:jar" "INCLUDED"
        , VerboseEdge "org.apache.poi:poi-ooxml:jar" "org.apache.logging.log4j:log4j-api:jar" "INCLUDED"
        , VerboseEdge "org.apache.logging.log4j:log4j-core:jar" "org.apache.logging.log4j:log4j-api:jar" "OMITTED_FOR_DUPLICATE"
        ]
    }

verboseGraphParsingSpec :: Spec
verboseGraphParsingSpec =
  describe "verbose graph parsing" $
    it "should parse the depgraph plugin's json format" $
      eitherDecode verboseGraphJson `shouldBe` Right expectedVerboseGraph

-- Aggregate output for the same build, with its own unrelated numeric ids.
mkAggregateArtifact :: Int -> Text -> Text -> Text -> Bool -> Artifact
mkAggregateArtifact numericId groupId artifactId version isDirect =
  Artifact
    { artifactNumericId = numericId
    , artifactGroupId = groupId
    , artifactArtifactId = artifactId
    , artifactVersion = version
    , artifactScopes = ["compile"]
    , artifactOptional = False
    , artifactIsDirect = isDirect
    }

aggregateOutput :: PluginOutput
aggregateOutput =
  PluginOutput
    { outArtifacts =
        [ mkAggregateArtifact 10 "org.apache.poi" "poi-ooxml" "5.2.5" True
        , mkAggregateArtifact 11 "org.apache.logging.log4j" "log4j-api" "2.21.1" False
        , mkAggregateArtifact 12 "org.apache.logging.log4j" "log4j-core" "2.21.1" True
        ]
    , outEdges = [Edge 10 11]
    }

augmentWithDuplicateEdgesSpec :: Spec
augmentWithDuplicateEdgesSpec =
  describe "augmentWithDuplicateEdges" $ do
    it "should add duplicate-resolved edges between existing artifacts" $
      outEdges (augmentWithDuplicateEdges aggregateOutput [expectedVerboseGraph])
        `shouldBe` [Edge 10 11, Edge 12 11]

    it "should not modify artifacts" $
      outArtifacts (augmentWithDuplicateEdges aggregateOutput [expectedVerboseGraph])
        `shouldBe` outArtifacts aggregateOutput

    it "should ignore included edges and artifacts absent from the aggregate output" $ do
      let onlyIncluded =
            expectedVerboseGraph
              { verboseEdges =
                  [ VerboseEdge "org.example:app:jar" "org.apache.poi:poi-ooxml:jar" "INCLUDED"
                  , VerboseEdge "org.apache.poi:poi-ooxml:jar" "org.apache.logging.log4j:log4j-api:jar" "INCLUDED"
                  ]
              }
      outEdges (augmentWithDuplicateEdges aggregateOutput [onlyIncluded])
        `shouldBe` outEdges aggregateOutput

    it "should not duplicate an edge that already exists" $ do
      let alreadyPresent = aggregateOutput{outEdges = [Edge 10 11, Edge 12 11]}
      outEdges (augmentWithDuplicateEdges alreadyPresent [expectedVerboseGraph])
        `shouldBe` [Edge 10 11, Edge 12 11]

    it "should do nothing without verbose graphs" $
      augmentWithDuplicateEdges aggregateOutput [] `shouldBe` aggregateOutput
