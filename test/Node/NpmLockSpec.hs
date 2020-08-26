module Node.NpmLockSpec
  ( spec
  ) where

import qualified Data.Map.Strict as M
import DepTypes
import GraphUtil
import Strategy.Node.NpmLock
import Test.Hspec

mockInput :: NpmPackageJson
mockInput = NpmPackageJson
  { packageName = "example"
  , packageVersion = "1.0.0"
  , packageDependencies = M.fromList
    [ ("packageOne", NpmDep
        { depVersion = "1.0.0"
        , depDev = Nothing
        , depResolved = Just "https://example.com/one.tgz"
        , depRequires = Just (M.fromList [("packageTwo", "2.0.0")])
        , depDependencies = Just (M.fromList
            [ ("packageTwo", NpmDep
                { depVersion = "2.0.0"
                , depDev = Just True
                , depResolved = Just "https://example.com/two.tgz"
                , depRequires = Just (M.fromList [("packageThree", "3.0.0")])
                , depDependencies = Nothing
                })
            ])
        })
    , ("packageThree", NpmDep
        { depVersion = "3.0.0"
        , depDev = Just True
        , depResolved = Nothing
        , depRequires = Just (M.fromList [("packageOne", "1.0.0")])
        , depDependencies = Nothing
        })
    ]
  }

packageOne :: Dependency
packageOne = Dependency
  { dependencyType = NodeJSType
  , dependencyName = "packageOne"
  , dependencyVersion = Just (CEq "1.0.0")
  , dependencyLocations = ["https://example.com/one.tgz"]
  , dependencyEnvironments = [EnvProduction]
  , dependencyTags = M.empty
  }

packageTwo :: Dependency
packageTwo = Dependency
  { dependencyType = NodeJSType
  , dependencyName = "packageTwo"
  , dependencyVersion = Just (CEq "2.0.0")
  , dependencyLocations = ["https://example.com/two.tgz"]
  , dependencyEnvironments = [EnvDevelopment]
  , dependencyTags = M.empty
  }

packageThree :: Dependency
packageThree = Dependency
  { dependencyType = NodeJSType
  , dependencyName = "packageThree"
  , dependencyVersion = Just (CEq "3.0.0")
  , dependencyLocations = []
  , dependencyEnvironments = [EnvDevelopment]
  , dependencyTags = M.empty
  }

spec :: Spec
spec = do
  describe "buildGraph" $ do
    it "should produce expected output" $ do
      let graph = buildGraph mockInput
      expectDeps [packageOne, packageTwo, packageThree] graph
      expectDirect [packageOne, packageThree] graph
      expectEdges [ (packageOne, packageTwo)
                  , (packageTwo, packageThree)
                  , (packageThree, packageOne)
                  ] graph
