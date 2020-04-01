module Node.PackageJsonTest
  ( spec_packageJsonBuildGraph
  ) where

import Prologue

import qualified Data.Map.Strict as M

import DepTypes
import Strategy.Node.PackageJson

import GraphUtil
import Test.Tasty.Hspec

mockInput :: PackageJson
mockInput = PackageJson
  { packageDeps = M.fromList [("packageOne", "^1.0.0")]
  , packageDevDeps = M.fromList [("packageTwo", "^2.0.0")]
  }

packageOne :: Dependency
packageOne = Dependency
  { dependencyType = NodeJSType
  , dependencyName = "packageOne"
  , dependencyVersion = Just (CCompatible "^1.0.0")
  , dependencyLocations = []
  , dependencyEnvironments = [EnvProduction]
  , dependencyTags = M.empty
  }

packageTwo :: Dependency
packageTwo = Dependency
  { dependencyType = NodeJSType
  , dependencyName = "packageTwo"
  , dependencyVersion = Just (CCompatible "^2.0.0")
  , dependencyLocations = []
  , dependencyEnvironments = [EnvDevelopment]
  , dependencyTags = M.empty
  }

spec_packageJsonBuildGraph :: Spec
spec_packageJsonBuildGraph = do
  describe "buildGraph" $ do
    it "should produce expected output" $ do
      let graph = buildGraph mockInput
      expectDeps [packageOne, packageTwo] graph
      expectDirect [packageOne, packageTwo] graph
      expectEdges [] graph
