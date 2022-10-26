module Conda.CondaListSpec (
  spec,
) where

import Control.Carrier.Diagnostics
import Data.Aeson
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import DepTypes
import Effect.Grapher
import Graphing (Graphing)
import Strategy.Conda.CondaList (buildGraph)
import Test.Hspec
import Prelude

expected :: Graphing Dependency
expected = run . evalGrapher $ do
  direct $
    Dependency
      { dependencyType = CondaType
      , dependencyName = "'pkgs/main':osx-64:biopython"
      , dependencyVersion = Just (CEq "1.78")
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }
  direct $
    Dependency
      { dependencyType = CondaType
      , dependencyName = "'pkgs/main':osx-64:blas"
      , dependencyVersion = Just (CEq "1.0")
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }
  direct $
    Dependency
      { dependencyType = CondaType
      , dependencyName = "'pkgs/main':osx-64:ca-certificates"
      , dependencyVersion = Just (CEq "2021.1.19")
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }
  -- Missing platform, should default to the v1 package spec format
  direct $
    Dependency
      { dependencyType = CondaType
      , dependencyName = "absl-py"
      , dependencyVersion = Just (CEq "1.3.0")
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }
  -- Missing channel field, should default to v1 package spec format
  direct $
    Dependency
      { dependencyType = CondaType
      , dependencyName = "aenum"
      , dependencyVersion = Just (CEq "3.1.5")
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }

spec :: Spec
spec = do
  condaOutputFile <- runIO (BS.readFile "test/Conda/testdata/conda-list-output.txt")
  describe "build graph from conda list output" $
    it "can build graph" $
      case eitherDecodeStrict condaOutputFile of
        Right deps -> buildGraph deps `shouldBe` expected
        Left err -> expectationFailure $ "Failed to parse: " ++ err
