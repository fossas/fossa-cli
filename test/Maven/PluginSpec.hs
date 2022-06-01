module Maven.PluginSpec (spec) where

import Strategy.Maven.Plugin (Artifact (..), Edge (..), PluginOutput (..), textArtifactToPluginOutput)
import Strategy.Maven.PluginTree (TextArtifact (..))
import Test.Effect (it', shouldBe')
import Test.Hspec (Spec, fdescribe, describe)

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
    , children = []
    }

complexTextArtifact :: TextArtifact
complexTextArtifact =
  TextArtifact
    { artifactText = "org.clojure:test.generative:1.0.0"
    , scopes = ["test"]
    , isDirect = False
    , isOptional = False
    , children =
        [ TextArtifact
            { artifactText = "org.fake:fake-pkg:1.0.0"
            , scopes = ["compile"]
            , isDirect = False
            , children = []
            , isOptional = True
            }
        , TextArtifact
            { artifactText = "org.foo:bar:1.0.0"
            , isDirect = False
            , scopes = ["compile"]
            , isOptional = False
            , children =
                [ TextArtifact
                    { artifactText = "org.baz:buzz:1.0.0"
                    , isDirect = False
                    , scopes = ["test"]
                    , children = []
                    , isOptional = False
                    }
                ]
            }
        , TextArtifact
            { artifactText = "org.clojure:data.generators:1.0.0"
            , isDirect = False
            , scopes = ["test"]
            , isOptional = False
            , children = []
            }
        ]
    }

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
            , artifactIsDirect = True
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
            , artifactIsDirect = False
            }
        ]
    , outEdges =
        [ Edge 2 1
        , Edge 4 3
        , Edge 4 2
        , Edge 4 0
        ]
    }

-- TODO: because we use sets, there is no set internal order here so matching the numeric id is difficult without
-- some kind of sort

textArtifactConversionSpec :: Spec
textArtifactConversionSpec =
  describe "Maven text artifact -> PluginOutput conversion" $ do
    it' "Converts a single TextArtifact correctly" $ do
      pluginOutput <- textArtifactToPluginOutput singleTextArtifact
      pluginOutput
        `shouldBe'` PluginOutput
          { outArtifacts = [simpleArtifact]
          , outEdges = []
          }

    it' "Converts a more complext TextArtifact correctly" $ do
      pluginOutput <- textArtifactToPluginOutput complexTextArtifact
      pluginOutput `shouldBe'` complexPluginOutputArtifacts

simpleArtifact :: Artifact
simpleArtifact =
  Artifact
    { artifactNumericId = 0
    , artifactGroupId = "org.clojure"
    , artifactArtifactId = "clojure"
    , artifactVersion = "1.12.0-master-SNAPSHOT"
    , artifactOptional = False
    , artifactScopes = ["test"]
    , artifactIsDirect = False
    }
