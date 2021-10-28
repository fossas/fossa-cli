module Node.PackageJsonSpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (NodeJSType),
  Dependency (..),
  VerConstraint (CCompatible),
 )
import GraphUtil (expectDeps, expectDirect, expectEdges)
import Strategy.Node.PackageJson (
  PackageJson (..),
  PkgJsonWorkspaces (PkgJsonWorkspaces),
  buildGraph,
 )
import Test.Hspec (Spec, describe, it)

mockInput :: PackageJson
mockInput =
  PackageJson
    { packageName = Nothing
    , packageVersion = Nothing
    , packageWorkspaces = PkgJsonWorkspaces []
    , packageDeps = Map.fromList [("packageOne", "^1.0.0")]
    , packageDevDeps = Map.fromList [("packageTwo", "^2.0.0")]
    }

packageOne :: Dependency
packageOne =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageOne"
    , dependencyVersion = Just (CCompatible "^1.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.empty
    }

packageTwo :: Dependency
packageTwo =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageTwo"
    , dependencyVersion = Just (CCompatible "^2.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvDevelopment
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
