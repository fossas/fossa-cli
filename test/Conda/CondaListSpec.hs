module Conda.CondaListSpec
  ( spec,
  )
where

import Control.Carrier.Diagnostics
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import DepTypes
import Effect.Grapher
import Graphing (Graphing)
import Strategy.Conda.CondaList (CondaEnvDep (..), parseCondaEnvDep)
import Test.Hspec
import Text.Megaparsec (parse)
import Prelude

condaForgeEnvOut :: Text
condaForgeEnvOut = "conda-forge/osx-64::bzip2==1.0.8=h0d85af4_4"

envDepParseSpec :: Spec
envDepParseSpec =
  fdescribe "Parsing dep specs from 'conda env create'" $ do
    it "Parses without slashes" $
      parse parseCondaEnvDep "" condaForgeEnvOut
        `shouldBe` Right
          CondaEnvDep
            { channel = "conda-forge",
              platform = "osx-64",
              name = "bzip2",
              version = "1.0.8"
            }

expected :: Graphing Dependency
expected = run . evalGrapher $ do
  direct $
    Dependency
      { dependencyType = CondaType,
        dependencyName = "biopython",
        dependencyVersion = Just (CEq "1.78"),
        dependencyLocations = [],
        dependencyEnvironments = mempty,
        dependencyTags = Map.empty
      }
  direct $
    Dependency
      { dependencyType = CondaType,
        dependencyName = "blas",
        dependencyVersion = Just (CEq "1.0"),
        dependencyLocations = [],
        dependencyEnvironments = mempty,
        dependencyTags = Map.empty
      }
  direct $
    Dependency
      { dependencyType = CondaType,
        dependencyName = "ca-certificates",
        dependencyVersion = Just (CEq "2021.1.19"),
        dependencyLocations = [],
        dependencyEnvironments = mempty,
        dependencyTags = Map.empty
      }

spec :: Spec
spec = do
  envDepParseSpec
