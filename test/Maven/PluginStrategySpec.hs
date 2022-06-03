module Maven.PluginStrategySpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import DepTypes
import GraphUtil
import Strategy.Maven.Plugin
import Strategy.Maven.PluginStrategy
import Test.Hspec

packageOne :: Dependency
packageOne =
  Dependency
    { dependencyType = MavenType
    , dependencyName = "mygroup:packageOne"
    , dependencyVersion = Just (CEq "1.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvTesting
    , dependencyTags = Map.fromList [("scopes", ["compile", "test"])]
    }

packageTwo :: Dependency
packageTwo =
  Dependency
    { dependencyType = MavenType
    , dependencyName = "mygroup:packageTwo"
    , dependencyVersion = Just (CEq "2.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.fromList [("scopes", ["compile"]), ("optional", ["true"])]
    }

packageFour :: Dependency
packageFour =
  Dependency
    { dependencyType = MavenType
    , dependencyName = "mygroup2:packageFour"
    , dependencyVersion = Just (CEq "4.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.fromList [("scopes", ["compile"])]
    }

mavenOutput :: PluginOutput
mavenOutput =
  PluginOutput
    { outArtifacts =
        [ Artifact
            { artifactNumericId = 0
            , artifactGroupId = "mygroup"
            , artifactArtifactId = "packageOne"
            , artifactVersion = "1.0.0"
            , artifactOptional = False
            , artifactScopes = ["compile", "test"]
            , artifactIsDirect = False
            }
        , Artifact
            { artifactNumericId = 1
            , artifactGroupId = "mygroup"
            , artifactArtifactId = "packageTwo"
            , artifactVersion = "2.0.0"
            , artifactOptional = True
            , artifactScopes = ["compile"]
            , artifactIsDirect = False
            }
        ]
    , outEdges =
        [ Edge
            { edgeFrom = 0
            , edgeTo = 1
            }
        ]
    }

mavenOutputWithDirects :: PluginOutput
mavenOutputWithDirects =
  PluginOutput
    { outArtifacts =
        [ Artifact
            { artifactNumericId = 0
            , artifactGroupId = "mygroup"
            , artifactArtifactId = "packageOne"
            , artifactVersion = "1.0.0"
            , artifactOptional = True
            , artifactScopes = ["compile", "test"]
            , artifactIsDirect = True
            }
        , Artifact
            { artifactNumericId = 1
            , artifactGroupId = "mygroup"
            , artifactArtifactId = "packageTwo"
            , artifactVersion = "2.0.0"
            , artifactOptional = True
            , artifactScopes = ["compile"]
            , artifactIsDirect = False
            }
        ]
    , outEdges =
        [ Edge
            { edgeFrom = 0
            , edgeTo = 1
            }
        ]
    }

mavenMultimoduleOutputWithDirects :: PluginOutput
mavenMultimoduleOutputWithDirects =
  PluginOutput
    { outArtifacts =
        [ Artifact
            { artifactNumericId = 0
            , artifactGroupId = "mygroup"
            , artifactArtifactId = "packageOne"
            , artifactVersion = "1.0.0"
            , artifactOptional = False
            , artifactScopes = ["compile", "test"]
            , artifactIsDirect = True
            }
        , Artifact
            { artifactNumericId = 1
            , artifactGroupId = "mygroup"
            , artifactArtifactId = "packageTwo"
            , artifactVersion = "2.0.0"
            , artifactOptional = True
            , artifactScopes = ["compile"]
            , artifactIsDirect = False
            }
        , Artifact
            { artifactNumericId = 2
            , artifactGroupId = "mygroup"
            , artifactArtifactId = "packageThree"
            , artifactVersion = "3.0.0"
            , artifactOptional = False
            , artifactScopes = ["compile"]
            , artifactIsDirect = True
            }
        , Artifact
            { artifactNumericId = 3
            , artifactGroupId = "mygroup2"
            , artifactArtifactId = "packageFour"
            , artifactVersion = "4.0.0"
            , artifactOptional = False
            , artifactScopes = ["compile"]
            , artifactIsDirect = False
            }
        ]
    , outEdges =
        [ Edge 0 1
        , Edge 2 3
        ]
    }

spec :: Spec
spec = do
  fdescribe "buildGraph" $ do
    it "should produce expected output" $ do
      let graph = buildGraph mavenOutput

      expectDeps [packageOne, packageTwo] graph
      expectDirect [] graph
      expectEdges [(packageOne, packageTwo)] graph

    it "Should promote children of root dep(s) to direct" $ do
      let graph = buildGraph mavenOutputWithDirects
      expectDeps [packageTwo] graph
      expectDirect [packageTwo] graph
      expectEdges [] graph

    it "Should promote children of root dep(s) to direct in multimodule projects" $ do
      let graph = buildGraph mavenMultimoduleOutputWithDirects
      expectDeps [packageTwo, packageFour] graph
      expectDirect [packageTwo, packageFour] graph
      expectEdges [] graph
