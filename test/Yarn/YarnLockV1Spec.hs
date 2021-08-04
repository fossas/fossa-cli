module Yarn.YarnLockV1Spec (
  spec,
) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text.Encoding
import DepTypes
import GraphUtil
import Strategy.Yarn.V1.YarnLock
import Test.Hspec
import Yarn.Lock qualified as YL

packageOne :: Dependency
packageOne =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageOne"
    , dependencyVersion = Just (CEq "1.0.0")
    , dependencyLocations = ["https://registry.npmjs.org/packageOne"]
    , dependencyEnvironments = []
    , dependencyTags = Map.empty
    }

packageTwo :: Dependency
packageTwo =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageTwo"
    , dependencyVersion = Just (CEq "2.0.0")
    , dependencyLocations = ["https://registry.npmjs.org/packageTwo"]
    , dependencyEnvironments = []
    , dependencyTags = Map.empty
    }

packageThree :: Dependency
packageThree =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageThree"
    , dependencyVersion = Just (CEq "3.0.0")
    , dependencyLocations = ["https://registry.npmjs.org/packageThree"]
    , dependencyEnvironments = []
    , dependencyTags = Map.empty
    }

spec :: Spec
spec = do
  testFile <- runIO (BS.readFile "test/Yarn/testdata/yarn.lock")
  describe "buildGraph" $ do
    it "should produce expected output" $ do
      case YL.parse "test/Yarn/testdata/yarn.lock" (decodeUtf8 testFile) of
        Left _ -> expectationFailure "failed to parse"
        Right lockfile -> do
          let graph = buildGraph lockfile
          expectDeps [packageOne, packageTwo, packageThree] graph
          expectDirect [] graph
          expectEdges
            [ (packageOne, packageTwo)
            , (packageTwo, packageThree)
            ]
            graph
