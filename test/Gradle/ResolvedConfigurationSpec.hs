module Gradle.ResolvedConfigurationSpec (
  spec,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import DepTypes
import GraphUtil (expectDeps, expectDirect, expectEdges)
import Graphing (Graphing, empty)
import Strategy.Gradle.Common (
  ConfigName (ConfigName),
  PackageName (PackageName),
 )
import Strategy.Gradle.ResolvedConfiguration (
  JsonDep (..),
  buildGraph,
 )
import Test.Hspec (Spec, describe, it, shouldBe)

projectOne :: Dependency
projectOne =
  Dependency
    { dependencyType = SubprojectType
    , dependencyName = ":projectOne"
    , dependencyVersion = Nothing
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton $ EnvOther "config"
    , dependencyTags = Map.empty
    }

projectTwo :: Dependency
projectTwo =
  Dependency
    { dependencyType = SubprojectType
    , dependencyName = ":projectTwo"
    , dependencyVersion = Nothing
    , dependencyLocations = []
    , dependencyEnvironments = Set.fromList [EnvDevelopment, EnvOther "config"]
    , dependencyTags = Map.empty
    }

projectThree :: Dependency
projectThree =
  Dependency
    { dependencyType = SubprojectType
    , dependencyName = ":projectThree"
    , dependencyVersion = Nothing
    , dependencyLocations = []
    , dependencyEnvironments = Set.fromList [EnvDevelopment, EnvTesting]
    , dependencyTags = Map.empty
    }

packageOne :: Dependency
packageOne =
  Dependency
    { dependencyType = MavenType
    , dependencyName = "mygroup:packageOne"
    , dependencyVersion = Just (CEq "1.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvDevelopment
    , dependencyTags = Map.empty
    }

packageTwo :: Dependency
packageTwo =
  Dependency
    { dependencyType = MavenType
    , dependencyName = "mygroup:packageTwo"
    , dependencyVersion = Just (CEq "2.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvTesting
    , dependencyTags = Map.empty
    }

mkProject :: Text -> DepEnvironment -> Dependency
mkProject name env =
  Dependency
    { dependencyType = SubprojectType
    , dependencyName = name
    , dependencyVersion = Nothing
    , dependencyLocations = []
    , dependencyEnvironments = Set.fromList [env, EnvOther "config"]
    , dependencyTags = Map.empty
    }

projectFour :: Dependency
projectFour = mkProject ":projectFour" EnvTesting

projectFive :: Dependency
projectFive = mkProject ":projectFive" EnvTesting

mkMavenDep :: Text -> DepEnvironment -> Dependency
mkMavenDep name env =
  Dependency
    { dependencyType = MavenType
    , dependencyName = name
    , dependencyVersion = Just (CEq "2.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton env
    , dependencyTags = Map.empty
    }

packageFour :: Dependency
packageFour = mkMavenDep "mygroup:packageFour" EnvTesting

packageFive :: Dependency
packageFive = mkMavenDep "mygroup:packageFive" EnvTesting

gradleOutput :: Map (Text, Text) [JsonDep]
gradleOutput =
  Map.fromList
    [ ((":projectOne", "config"), [ProjectDep ":projectTwo", ProjectDep ":projectFour", ProjectDep ":projectFive"])
    , ((":projectTwo", "compileOnly"), [ProjectDep ":projectThree", PackageDep "mygroup:packageOne" "1.0.0" []])
    , ((":projectThree", "testCompileOnly"), [PackageDep "mygroup:packageTwo" "2.0.0" []])
    , ((":projectFour", "testCompileClasspath"), [PackageDep "mygroup:packageFour" "2.0.0" []])
    , ((":projectFive", "testRuntimeClasspath"), [PackageDep "mygroup:packageFive" "2.0.0" []])
    ]

wrapKeys :: (Text, Text) -> (PackageName, ConfigName)
wrapKeys (a, b) = (PackageName a, ConfigName b)

spec :: Spec
spec = do
  describe "buildGraph" $ do
    it "should produce an empty graph for empty input" $ do
      let graph = buildGraph Map.empty (Set.fromList [])
      graph `shouldBe` (empty :: Graphing Dependency)

    it "should produce expected output" $ do
      let graph = buildGraph (Map.mapKeys wrapKeys gradleOutput) (Set.fromList [])
      expectDeps [projectOne, projectTwo, projectThree, packageOne, packageTwo, projectFour, projectFive, packageFour, packageFive] graph
      expectDirect [projectOne, projectTwo, projectThree, projectFour, projectFive] graph
      expectEdges
        [ (projectOne, projectTwo)
        , (projectTwo, projectThree)
        , (projectTwo, packageOne)
        , (projectThree, packageTwo)
        , (projectOne, projectFour)
        , (projectOne, projectFive)
        , (projectFour, packageFour)
        , (projectFive, packageFive)
        ]
        graph

    it "should filter configurations when provided" $ do
      let filteredConfig = "testRuntimeClasspath"
      let graph = buildGraph (Map.mapKeys wrapKeys gradleOutput) (Set.fromList [ConfigName filteredConfig])
      let projectFiveCustomEnv = projectFive{dependencyEnvironments = Set.singleton $ EnvOther filteredConfig}
      let packageFiveCustomEnv = packageFive{dependencyEnvironments = Set.singleton $ EnvOther filteredConfig}

      expectDeps [projectFiveCustomEnv, packageFiveCustomEnv] graph
      expectDirect [projectFiveCustomEnv] graph
      expectEdges
        [ (projectFiveCustomEnv, packageFiveCustomEnv)
        ]
        graph
