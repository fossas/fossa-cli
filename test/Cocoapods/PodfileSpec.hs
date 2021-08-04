module Cocoapods.PodfileSpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as TIO
import DepTypes
import GraphUtil
import Strategy.Cocoapods.Podfile
import Test.Hspec qualified as T
import Text.Megaparsec

dependencyOne :: Dependency
dependencyOne =
  Dependency
    { dependencyType = PodType
    , dependencyName = "one"
    , dependencyVersion = Just (CEq "1.0.0")
    , dependencyLocations = ["test.repo"]
    , dependencyEnvironments = []
    , dependencyTags = Map.empty
    }

dependencyTwo :: Dependency
dependencyTwo =
  Dependency
    { dependencyType = PodType
    , dependencyName = "two"
    , dependencyVersion = Just (CEq "2.0.0")
    , dependencyLocations = ["custom.repo"]
    , dependencyEnvironments = []
    , dependencyTags = Map.empty
    }

dependencyThree :: Dependency
dependencyThree =
  Dependency
    { dependencyType = PodType
    , dependencyName = "three"
    , dependencyVersion = Just (CEq "3.0.0")
    , dependencyLocations = ["test.repo"]
    , dependencyEnvironments = []
    , dependencyTags = Map.empty
    }

dependencyFour :: Dependency
dependencyFour =
  Dependency
    { dependencyType = PodType
    , dependencyName = "four"
    , dependencyVersion = Nothing
    , dependencyLocations = ["test.repo"]
    , dependencyEnvironments = []
    , dependencyTags = Map.empty
    }

podOne :: Pod
podOne = Pod "one" (Just "1.0.0") Map.empty

podTwo :: Pod
podTwo = Pod "two" (Just "2.0.0") (Map.fromList [(SourceProperty, "custom.repo")])

podThree :: Pod
podThree = Pod "three" (Just "3.0.0") (Map.fromList [(PathProperty, "internal/path")])

podFour :: Pod
podFour = Pod "four" Nothing (Map.fromList [(GitProperty, "fossa/spectrometer"), (CommitProperty, "12345")])

testPods :: [Pod]
testPods = [podOne, podTwo, podThree, podFour]

testPodfile :: Podfile
testPodfile = Podfile testPods "test.repo"

spec :: T.Spec
spec = do
  T.describe "podfile analyzer" $
    T.it "produces the expected output" $ do
      let graph = buildGraph testPodfile

      expectDeps [dependencyOne, dependencyTwo, dependencyThree, dependencyFour] graph
      expectDirect [dependencyOne, dependencyTwo, dependencyThree, dependencyFour] graph
      expectEdges [] graph

  podLockFile <- T.runIO (TIO.readFile "test/Cocoapods/testdata/Podfile")
  T.describe "podfile parser" $ do
    T.it "correctly parses a file" $ do
      case runParser parsePodfile "" podLockFile of
        Left _ -> T.expectationFailure "failed to parse"
        Right result -> do
          pods result `T.shouldMatchList` testPods
          source result `T.shouldBe` "test.repo"
