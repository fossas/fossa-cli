module Maven.PluginStrategySpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import DepTypes (
  DepEnvironment (..),
  DepType (MavenType),
  Dependency (..),
  VerConstraint (CEq),
 )
import GraphUtil (expectDeps, expectDirect, expectEdges)
import Strategy.Maven.Plugin (
  Artifact (
    Artifact,
    artifactArtifactId,
    artifactGroupId,
    artifactIsDirect,
    artifactNumericId,
    artifactOptional,
    artifactScopes,
    artifactVersion
  ),
  Edge (Edge, edgeFrom, edgeTo),
  PluginOutput (..),
  ReactorArtifact (..),
  ReactorOutput (..),
 )
import Strategy.Maven.PluginStrategy (buildGraph)
import Test.Hspec (Spec, describe, it)

packageOne :: Dependency
packageOne =
  Dependency
    { dependencyType = MavenType
    , dependencyName = "mygroup:packageOne"
    , dependencyVersion = Just (CEq "1.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = Set.fromList [EnvProduction, EnvTesting]
    , dependencyTags = Map.fromList [("scopes", ["compile", "test"])]
    }

packageTwo :: Dependency
packageTwo =
  Dependency
    { dependencyType = MavenType
    , dependencyName = "mygroup:packageTwo"
    , dependencyVersion = Just (CEq "2.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.fromList [("scopes", ["compile"]), ("optional", ["true"])]
    }

packageFour :: Dependency
packageFour =
  Dependency
    { dependencyType = MavenType
    , dependencyName = "mygroup2:packageFour"
    , dependencyVersion = Just (CEq "4.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.fromList [("scopes", ["compile"])]
    }

packageMultiScope :: Dependency
packageMultiScope =
  Dependency
    { dependencyType = MavenType
    , dependencyName = "multiscope:pie"
    , dependencyVersion = Just (CEq "1.0.0")
    , dependencyLocations = mempty
    , dependencyEnvironments = Set.fromList [EnvProduction, EnvTesting, EnvOther "other"]
    , dependencyTags = Map.singleton "scopes" ["compile", "test", "other"]
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

mavenMultiScopeOutput :: PluginOutput
mavenMultiScopeOutput =
  PluginOutput
    { outArtifacts =
        [ Artifact
            { artifactNumericId = 0
            , artifactGroupId = "multiscope"
            , artifactArtifactId = "pie"
            , artifactVersion = "1.0.0"
            , artifactOptional = False
            , artifactScopes = ["compile", "test", "other"]
            , artifactIsDirect = True
            }
        ]
    , outEdges =
        []
    }

mavenCrossDependentSubModules :: PluginOutput
mavenCrossDependentSubModules =
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
            , artifactIsDirect = False
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
        , Edge 3 0
        ]
    }

spec :: Spec
spec = do
  describe "buildGraph" $ do
    it "should produce expected output" $ do
      let graph = buildGraph (ReactorOutput []) mavenOutput

      expectDeps [packageOne, packageTwo] graph
      expectDirect [] graph
      expectEdges [(packageOne, packageTwo)] graph

    it "Should promote children of root dep(s) to direct" $ do
      let graph = buildGraph (ReactorOutput []) mavenOutputWithDirects
      expectDeps [packageTwo] graph
      expectDirect [packageTwo] graph
      expectEdges [] graph

    it "Should promote children of root dep(s) to direct in multimodule projects" $ do
      let graph = buildGraph (ReactorOutput []) mavenMultimoduleOutputWithDirects
      expectDeps [packageTwo, packageFour] graph
      expectDirect [packageTwo, packageFour] graph
      expectEdges [] graph

    it "Should parse all scopes" $ do
      let graph = buildGraph (ReactorOutput []) mavenMultiScopeOutput
      expectDeps [packageMultiScope] graph

    let graph = buildGraph (ReactorOutput [ReactorArtifact "packageThree"]) mavenCrossDependentSubModules
    it "Should mark top-level graph artifacts and known submodules as direct, then shrinkRoots" $ do
      expectDirect [packageTwo, packageFour] graph

    it "Should remove submodules in projects where submodules depend on eachother" $ do
      expectDeps [packageTwo, packageFour] graph
      expectEdges [(packageFour, packageTwo)] graph
