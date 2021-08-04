module Gradle.GradleSpec (
  spec,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import DepTypes
import GraphUtil
import Graphing (Graphing, empty)
import Strategy.Gradle (ConfigName (..), JsonDep (..), PackageName (..), buildGraph)
import Test.Hspec

projectOne :: Dependency
projectOne =
  Dependency
    { dependencyType = SubprojectType
    , dependencyName = ":projectOne"
    , dependencyVersion = Nothing
    , dependencyLocations = []
    , dependencyEnvironments = [EnvOther "config"]
    , dependencyTags = Map.empty
    }

projectTwo :: Dependency
projectTwo =
  Dependency
    { dependencyType = SubprojectType
    , dependencyName = ":projectTwo"
    , dependencyVersion = Nothing
    , dependencyLocations = []
    , dependencyEnvironments = [EnvDevelopment, EnvOther "config"]
    , dependencyTags = Map.empty
    }

projectThree :: Dependency
projectThree =
  Dependency
    { dependencyType = SubprojectType
    , dependencyName = ":projectThree"
    , dependencyVersion = Nothing
    , dependencyLocations = []
    , dependencyEnvironments = [EnvDevelopment, EnvTesting]
    , dependencyTags = Map.empty
    }

packageOne :: Dependency
packageOne =
  Dependency
    { dependencyType = MavenType
    , dependencyName = "mygroup:packageOne"
    , dependencyVersion = Just (CEq "1.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = [EnvDevelopment]
    , dependencyTags = Map.empty
    }

packageTwo :: Dependency
packageTwo =
  Dependency
    { dependencyType = MavenType
    , dependencyName = "mygroup:packageTwo"
    , dependencyVersion = Just (CEq "2.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = [EnvTesting]
    , dependencyTags = Map.empty
    }

gradleOutput :: Map (Text, Text) [JsonDep]
gradleOutput =
  Map.fromList
    [ ((":projectOne", "config"), [ProjectDep ":projectTwo"])
    , ((":projectTwo", "compileOnly"), [ProjectDep ":projectThree", PackageDep "mygroup:packageOne" "1.0.0" []])
    , ((":projectThree", "testCompileOnly"), [PackageDep "mygroup:packageTwo" "2.0.0" []])
    ]

wrapKeys :: (Text, Text) -> (PackageName, ConfigName)
wrapKeys (a, b) = (PackageName a, ConfigName b)

spec :: Spec
spec = do
  describe "buildGraph" $ do
    it "should produce an empty graph for empty input" $ do
      let graph = buildGraph Map.empty
      graph `shouldBe` (empty :: Graphing Dependency)

    it "should produce expected output" $ do
      let graph = buildGraph $ Map.mapKeys wrapKeys gradleOutput
      expectDeps [projectOne, projectTwo, projectThree, packageOne, packageTwo] graph
      expectDirect [projectOne, projectTwo, projectThree] graph
      expectEdges
        [ (projectOne, projectTwo)
        , (projectTwo, projectThree)
        , (projectTwo, packageOne)
        , (projectThree, packageTwo)
        ]
        graph
