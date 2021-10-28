module Node.NpmLockSpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import DepTypes
import GraphUtil
import Strategy.Node.Npm.PackageLock
import Test.Hspec

mockInput :: NpmPackageJson
mockInput =
  NpmPackageJson
    { packageName = "example"
    , packageVersion = "1.0.0"
    , packageDependencies =
        Map.fromList
          [
            ( "packageOne"
            , NpmDep
                { depVersion = "1.0.0"
                , depDev = False
                , depResolved = Just "https://example.com/one.tgz"
                , depRequires = Map.fromList [("packageTwo", "2.0.0")]
                , depDependencies =
                    Map.fromList
                      [
                        ( "packageTwo"
                        , NpmDep
                            { depVersion = "2.0.0"
                            , depDev = True
                            , depResolved = Just "https://example.com/two.tgz"
                            , depRequires = Map.fromList [("packageThree", "3.0.0")]
                            , depDependencies = mempty
                            }
                        )
                      ]
                }
            )
          ,
            ( "packageThree"
            , NpmDep
                { depVersion = "3.0.0"
                , depDev = True
                , depResolved = Just "https://example.com/three.tgz"
                , depRequires = Map.fromList [("packageOne", "1.0.0")]
                , depDependencies = mempty
                }
            )
          ,
            ( "packageFour"
            , NpmDep
                { depVersion = "file:abc/def"
                , depDev = False
                , depResolved = Nothing
                , depRequires = mempty
                , depDependencies = mempty
                }
            )
          ]
    }

packageOne :: Dependency
packageOne =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageOne"
    , dependencyVersion = Just (CEq "1.0.0")
    , dependencyLocations = ["https://example.com/one.tgz"]
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.empty
    }

packageTwo :: Dependency
packageTwo =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageTwo"
    , dependencyVersion = Just (CEq "2.0.0")
    , dependencyLocations = ["https://example.com/two.tgz"]
    , dependencyEnvironments = Set.singleton EnvDevelopment
    , dependencyTags = Map.empty
    }

packageThree :: Dependency
packageThree =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageThree"
    , dependencyVersion = Just (CEq "3.0.0")
    , dependencyLocations = ["https://example.com/three.tgz"]
    , dependencyEnvironments = Set.singleton EnvDevelopment
    , dependencyTags = Map.empty
    }

spec :: Spec
spec = do
  describe "buildGraph" $ do
    it "should produce expected output" $ do
      let graph = buildGraph mockInput $ Set.fromList ["packageOne", "packageThree"]
      expectDeps [packageOne, packageTwo, packageThree] graph
      expectDirect [packageOne, packageThree] graph
      expectEdges
        [ (packageOne, packageTwo)
        , (packageTwo, packageThree)
        , (packageThree, packageOne)
        ]
        graph
