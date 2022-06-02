{-# LANGUAGE RecordWildCards #-}

module Maven.PluginSpec (spec) where

import Data.Tree (Tree (..))
import Strategy.Maven.Plugin (Artifact (..), Edge (..), PluginOutput (..), textArtifactToPluginOutput)
import Strategy.Maven.PluginTree (TextArtifact (..))
import Test.Effect (it', shouldBe', shouldMatchList')
import Test.Hspec (Spec, describe, fdescribe)

spec :: Spec
spec = do
  -- TODO: Consider moving this to PluginStrategySpec.hs
  textArtifactConversionSpec

singleTextArtifact :: TextArtifact
singleTextArtifact =
  TextArtifact
    { artifactText = "org.clojure:clojure:1.12.0-master-SNAPSHOT"
    , scopes = ["test"]
    , isDirect = True
    , isOptional = False
    }

complexTextArtifact :: Tree TextArtifact
complexTextArtifact =
  Node
    TextArtifact
      { artifactText = "org.clojure:test.generative:1.0.0"
      , scopes = ["test"]
      , isDirect = True
      , isOptional = False
      }
    [ Node
        TextArtifact
          { artifactText = "org.fake:fake-pkg:1.0.0"
          , scopes = ["compile"]
          , isDirect = False
          , isOptional = True
          }
        []
    , Node
        TextArtifact
          { artifactText = "org.foo:bar:1.0.0"
          , isDirect = False
          , scopes = ["compile"]
          , isOptional = False
          }
        [ Node
            TextArtifact
              { artifactText = "org.baz:buzz:1.0.0"
              , isDirect = False
              , scopes = ["test"]
              , isOptional = False
              }
            []
        ]
    , Node
        TextArtifact
          { artifactText = "org.clojure:data.generators:1.0.0"
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
