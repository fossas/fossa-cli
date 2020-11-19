module Node.YarnLockSpec
  ( spec
  ) where

import qualified Data.Map.Strict as M
import Data.Text.Encoding
import DepTypes
import GraphUtil
import Strategy.Node.YarnLock
import qualified Data.ByteString as BS
import Test.Hspec
import qualified Yarn.Lock as YL

packageOne :: Dependency
packageOne = Dependency
  { dependencyType = NodeJSType
  , dependencyName = "packageOne"
  , dependencyVersion = Just (CEq "1.0.0")
  , dependencyLocations = ["https://registry.npmjs.org/packageOne"]
  , dependencyEnvironments = []
  , dependencyTags = M.empty
  }

packageTwo :: Dependency
packageTwo = Dependency
  { dependencyType = NodeJSType
  , dependencyName = "packageTwo"
  , dependencyVersion = Just (CEq "2.0.0")
  , dependencyLocations = ["https://registry.npmjs.org/packageTwo"]
  , dependencyEnvironments = []
  , dependencyTags = M.empty
  }

packageThree :: Dependency
packageThree = Dependency
  { dependencyType = NodeJSType
  , dependencyName = "packageThree"
  , dependencyVersion = Just (CEq "3.0.0")
  , dependencyLocations = ["https://registry.npmjs.org/packageThree"]
  , dependencyEnvironments = []
  , dependencyTags = M.empty
  }

spec :: Spec
spec = do
  testFile <- runIO (BS.readFile "test/Node/testdata/yarn.lock")
  describe "buildGraph" $ do
    it "should produce expected output" $ do
      case YL.parse "test/Node/testdata/yarn.lock" (decodeUtf8 testFile) of
        Left _ -> expectationFailure "failed to parse"
        Right lockfile -> do
          let graph = buildGraph lockfile
          expectDeps [packageOne, packageTwo, packageThree] graph
          expectDirect [] graph
          expectEdges [ (packageOne, packageTwo)
                      , (packageTwo, packageThree)
                      ] graph
