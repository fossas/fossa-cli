module Gradle.GradleTest
  ( spec_buildGraph
  ) where

import Prologue

import qualified Data.Map.Strict as M
import           Polysemy
import           Polysemy.Error

import           Diagnostics
import           Effect.GraphBuilder
import qualified Graph as G
import           Strategy.Gradle (JsonDep(..), buildGraph)

import GraphUtil
import Test.Tasty.Hspec

projectOne :: G.Dependency
projectOne = G.Dependency
  { dependencyType = G.SubprojectType
  , dependencyName = ":projectOne"
  , dependencyVersion = Nothing
  , dependencyLocations = []
  , dependencyTags = M.empty
  }

projectTwo :: G.Dependency
projectTwo = G.Dependency
  { dependencyType = G.SubprojectType
  , dependencyName = ":projectTwo"
  , dependencyVersion = Nothing
  , dependencyLocations = []
  , dependencyTags = M.empty
  }

projectThree :: G.Dependency
projectThree = G.Dependency
  { dependencyType = G.SubprojectType
  , dependencyName = ":projectThree"
  , dependencyVersion = Nothing
  , dependencyLocations = []
  , dependencyTags = M.empty
  }

packageOne :: G.Dependency
packageOne = G.Dependency
  { dependencyType = G.MavenType
  , dependencyName = "mygroup:packageOne"
  , dependencyVersion = Just (G.CEq "1.0.0")
  , dependencyLocations = []
  , dependencyTags = M.empty
  }

packageTwo :: G.Dependency
packageTwo = G.Dependency
  { dependencyType = G.MavenType
  , dependencyName = "mygroup:packageTwo"
  , dependencyVersion = Just (G.CEq "2.0.0")
  , dependencyLocations = []
  , dependencyTags = M.empty
  }

gradleOutput :: Map Text [JsonDep]
gradleOutput = M.fromList
  [ (":projectOne", [ProjectDep ":projectTwo"])
  , (":projectTwo", [ProjectDep ":projectThree", PackageDep "mygroup:packageOne" "1.0.0" []])
  , (":projectThree", [PackageDep "mygroup:packageTwo" "2.0.0" []])
  ]

spec_buildGraph :: Spec
spec_buildGraph = do
  let runIt = run . runError @CLIErr . evalGraphBuilder G.empty

  describe "buildGraph" $ do
    it "should produce an empty graph for empty input" $ do
      let result = runIt $ buildGraph M.empty
      case result of
        Left err -> expectationFailure ("buildGraph failed: " <> show err)
        Right graph -> graph `shouldBe` G.empty

    it "should fail when there are unresolved projects" $ do
      let result = runIt $ buildGraph (M.fromList [(":myproject", [ProjectDep ":missingproject"])])
      case result of
        Left _ -> pure ()
        Right _ -> expectationFailure "did not fail"

    it "should produce expected output" $ do
      let result = runIt $ buildGraph gradleOutput
      case result of
        Left _ -> expectationFailure "buildGraph failed"
        Right graph -> do
          expectDeps [projectOne, projectTwo, projectThree, packageOne, packageTwo] graph
          expectDirect [projectOne, projectTwo, projectThree] graph
          expectEdges [ (projectOne, projectTwo)
                      , (projectTwo, projectThree)
                      , (projectTwo, packageOne)
                      , (projectThree, packageTwo)
                      ] graph
