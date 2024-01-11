module Gradle.ResolutionApiSpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.String.Conversion (toText)
import Data.Text (Text)
import DepTypes
import GraphUtil (expectDep, expectDeps, expectDirect, expectEdges)
import Graphing (Graphing, empty)
import Strategy.Gradle.Common (
  ConfigName (..),
  ProjectName (..),
 )
import Strategy.Gradle.ResolutionApi (
  ResolvedComponent (..),
  ResolvedConfiguration (..),
  ResolvedDependency (..),
  ResolvedProject (..),
  buildGraph,
  parseResolutionApiJsonDeps,
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

gradleOutput :: [ResolvedProject]
gradleOutput =
  [ ResolvedProject
      (ProjectName ":projectOne")
      [ ResolvedConfiguration
          (ConfigName "config")
          [ProjectDependency ":projectOne"]
          [ResolvedComponent (ProjectDependency ":projectOne") [ProjectDependency ":projectTwo", ProjectDependency ":projectFour", ProjectDependency ":projectFive"]]
      ]
  , ResolvedProject
      (ProjectName ":projectTwo")
      [ ResolvedConfiguration
          (ConfigName "compileOnly")
          [ProjectDependency ":projectTwo"]
          [ResolvedComponent (ProjectDependency ":projectTwo") [ProjectDependency ":projectThree", PackageDependency "mygroup:packageOne" "1.0.0"]]
      ]
  , ResolvedProject
      (ProjectName ":projectThree")
      [ ResolvedConfiguration
          (ConfigName "testCompileOnly")
          [ProjectDependency ":projectThree"]
          [ResolvedComponent (ProjectDependency ":projectThree") [PackageDependency "mygroup:packageTwo" "2.0.0"]]
      ]
  , ResolvedProject
      (ProjectName ":projectFour")
      [ ResolvedConfiguration
          (ConfigName "testCompileClasspath")
          [ProjectDependency ":projectFour"]
          [ResolvedComponent (ProjectDependency ":projectFour") [PackageDependency "mygroup:packageFour" "2.0.0"]]
      ]
  , ResolvedProject
      (ProjectName ":projectFive")
      [ ResolvedConfiguration
          (ConfigName "testRuntimeClasspath")
          [ProjectDependency ":projectFive"]
          [ResolvedComponent (ProjectDependency ":projectFive") [PackageDependency "mygroup:packageFive" "2.0.0"]]
      ]
  ]

spec :: Spec
spec = do
  describe "buildGraph" $ do
    it "should produce an empty graph for empty input" $ do
      let graph = buildGraph [] Set.empty
      graph `shouldBe` (empty :: Graphing Dependency)

    it "should produce expected output" $ do
      let graph = buildGraph gradleOutput Set.empty
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
      let graph = buildGraph gradleOutput (Set.fromList [ConfigName filteredConfig])
      let projectFiveCustomEnv = projectFive{dependencyEnvironments = Set.singleton $ EnvOther filteredConfig}
      let packageFiveCustomEnv = packageFive{dependencyEnvironments = Set.singleton $ EnvOther filteredConfig}

      expectDirect [projectFiveCustomEnv] graph
      expectDeps [projectFiveCustomEnv, packageFiveCustomEnv] graph
      expectEdges
        [ (projectFiveCustomEnv, packageFiveCustomEnv)
        ]
        graph

    it "TEST TEST TEST" $ do
      exampleInputSingle <- toText <$> readFile "/home/leo/tmp/zd-7543/yubico-search.txt"
      let parsed = parseResolutionApiJsonDeps exampleInputSingle
      let graph = buildGraph parsed (Set.fromList ["compileOnly", "nativeLibsInputZips", "runtimeClasspath", "runtimeOnly"])
      let expected =
            Dependency
              { dependencyType = MavenType
              , dependencyName = "com.yubico:yubihsm"
              , dependencyVersion = Just (CEq "2.3.0.0")
              , dependencyLocations = []
              , dependencyEnvironments = Set.singleton $ EnvOther "nativeLibsInputZips"
              , dependencyTags = Map.empty
              }
      expectDep expected graph
