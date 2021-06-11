module Cocoapods.PodfileLockSpec (
  spec,
) where

import Data.Map.Strict qualified as M
import Data.Text.IO qualified as TIO
import DepTypes
import GraphUtil
import Strategy.Cocoapods.PodfileLock
import Test.Hspec qualified as T
import Text.Megaparsec

dependencyOne :: Dependency
dependencyOne =
  Dependency
    { dependencyType = PodType
    , dependencyName = "one"
    , dependencyVersion = Just (CEq "1.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = []
    , dependencyTags = M.empty
    }

dependencyTwo :: Dependency
dependencyTwo =
  Dependency
    { dependencyType = PodType
    , dependencyName = "two"
    , dependencyVersion = Just (CEq "2.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = []
    , dependencyTags = M.empty
    }

dependencyThree :: Dependency
dependencyThree =
  Dependency
    { dependencyType = PodType
    , dependencyName = "three"
    , dependencyVersion = Just (CEq "3.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = []
    , dependencyTags = M.empty
    }

dependencyFour :: Dependency
dependencyFour =
  Dependency
    { dependencyType = PodType
    , dependencyName = "four"
    , dependencyVersion = Just (CEq "4.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = []
    , dependencyTags = M.empty
    }

podSection :: Section
podSection = PodSection [Pod "one" "1.0.0" [Dep "two", Dep "three"], Pod "two" "2.0.0" [], Pod "three" "3.0.0" [Dep "four"], Pod "four" "4.0.0" []]

dependencySection :: Section
dependencySection = DependencySection [Dep "one", Dep "three"]

spec :: T.Spec
spec = do
  T.describe "podfile lock analyzer" $
    T.it "produces the expected output" $ do
      let graph = buildGraph [podSection, dependencySection]

      expectDeps [dependencyOne, dependencyTwo, dependencyThree, dependencyFour] graph
      expectDirect [dependencyOne, dependencyThree] graph
      expectEdges
        [ (dependencyOne, dependencyTwo)
        , (dependencyOne, dependencyThree)
        , (dependencyThree, dependencyFour)
        ]
        graph

  podLockFile <- T.runIO (TIO.readFile "test/Cocoapods/testdata/Podfile.lock")
  T.describe "podfile lock parser" $
    T.it "parses error messages into an empty list" $
      case runParser findSections "" podLockFile of
        Left err -> T.expectationFailure ("failed to parse: " <> errorBundlePretty err)
        Right result -> do
          result `T.shouldContain` [podSection]
          result `T.shouldContain` [dependencySection]
