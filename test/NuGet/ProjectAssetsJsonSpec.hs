module NuGet.ProjectAssetsJsonSpec (
  spec,
) where

import Data.Aeson
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as M
import DepTypes
import GraphUtil
import Strategy.NuGet.ProjectAssetsJson
import Test.Hspec

dependencyOne :: Dependency
dependencyOne =
  Dependency
    { dependencyType = NuGetType
    , dependencyName = "one"
    , dependencyVersion = Just (CEq "1.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = []
    , dependencyTags = M.empty
    }

dependencyTwo :: Dependency
dependencyTwo =
  Dependency
    { dependencyType = NuGetType
    , dependencyName = "two"
    , dependencyVersion = Just (CEq "2.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = []
    , dependencyTags = M.empty
    }

dependencyThree :: Dependency
dependencyThree =
  Dependency
    { dependencyType = NuGetType
    , dependencyName = "three"
    , dependencyVersion = Just (CEq "3.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = []
    , dependencyTags = M.empty
    }

dependencyFour :: Dependency
dependencyFour =
  Dependency
    { dependencyType = NuGetType
    , dependencyName = "four"
    , dependencyVersion = Just (CEq "4.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = []
    , dependencyTags = M.empty
    }

spec :: Spec
spec = do
  testFile <- runIO (BS.readFile "test/NuGet/testdata/project.assets.json")

  describe "project.assets.json analyzer" $ do
    it "reads a file and constructs an accurate graph" $ do
      case eitherDecodeStrict testFile of
        Right res -> do
          let graph = buildGraph res
          expectDeps [dependencyOne, dependencyTwo, dependencyThree, dependencyFour] graph
          expectDirect [dependencyOne, dependencyTwo, dependencyFour] graph
          expectEdges [(dependencyOne, dependencyThree)] graph
        Left _ -> expectationFailure "failed to parse"
