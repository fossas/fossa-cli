module Node.PackageJsonSpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import DepTypes
import GraphUtil
import Strategy.Node.PackageJson
import Test.Hspec

mockInput :: PackageJson
mockInput =
  PackageJson
    { packageDeps = Map.fromList [("packageOne", "^1.0.0")]
    , packageDevDeps = Map.fromList [("packageTwo", "^2.0.0")]
    }

packageOne :: Dependency
packageOne =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageOne"
    , dependencyVersion = Just (CCompatible "^1.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = [EnvProduction]
    , dependencyTags = Map.empty
    }

packageTwo :: Dependency
packageTwo =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageTwo"
    , dependencyVersion = Just (CCompatible "^2.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = [EnvDevelopment]
    , dependencyTags = Map.empty
    }

spec :: Spec
spec = do
  describe "buildGraph" $ do
    it "should produce expected output" $ do
      let graph = buildGraph mockInput
      expectDeps [packageOne, packageTwo] graph
      expectDirect [packageOne, packageTwo] graph
      expectEdges [] graph
