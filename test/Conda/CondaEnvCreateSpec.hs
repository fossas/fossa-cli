module Conda.CondaEnvCreateSpec
  ( spec,
  )
where

import Control.Carrier.Diagnostics (run)
import Data.Aeson (eitherDecodeStrict)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import DepTypes
  ( DepType (CondaType),
    Dependency (..),
    VerConstraint (CEq),
  )
import Effect.Grapher (direct, evalGrapher)
import Graphing (Graphing)
import Strategy.Conda.CondaEnvCreate (CondaEnvDep (..), buildGraph, parseCondaEnvDep, parseEnvCreateDeps)
import Test.Effect (expectationFailure', it', shouldBe')
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Text.Megaparsec (parse)
import Prelude

condaForgeEnvSpec :: Text
condaForgeEnvSpec = "conda-forge/osx-64::bzip2==1.0.8=h0d85af4_4"

pkgsMainEnvSpec :: Text
pkgsMainEnvSpec = "pkgs/main/osx-64::libgfortran5==11.3.0=h082f757_25"

envDepParseSpec :: Spec
envDepParseSpec =
  describe "Parsing dep specs from 'conda env create'" $ do
    it "Parses without slashes" $
      parse parseCondaEnvDep "" condaForgeEnvSpec
        `shouldBe` Right
          CondaEnvDep
            { channel = "conda-forge",
              platform = "osx-64",
              name = "bzip2",
              version = "1.0.8"
            }
    it "Parses with slashes in the channel" $ do
      parse parseCondaEnvDep "" pkgsMainEnvSpec
        `shouldBe` Right
          CondaEnvDep
            { channel = "pkgs/main",
              platform = "osx-64",
              name = "libgfortran5",
              version = "11.3.0"
            }

expected :: Graphing Dependency
expected = run . evalGrapher $ do
  direct $
    Dependency
      { dependencyType = CondaType,
        dependencyName = "'conda-forge':osx-64:bzip2",
        dependencyVersion = Just (CEq "1.0.8"),
        dependencyLocations = [],
        dependencyEnvironments = mempty,
        dependencyTags = Map.empty
      }
  direct $
    Dependency
      { dependencyType = CondaType,
        dependencyName = "'defaults':osx-64:ca-certificates",
        dependencyVersion = Just (CEq "2022.07.19"),
        dependencyLocations = [],
        dependencyEnvironments = mempty,
        dependencyTags = Map.empty
      }
  direct $
    Dependency
      { dependencyType = CondaType,
        dependencyName = "'pkgs/main':noarch:tzdata",
        dependencyVersion = Just (CEq "2022c"),
        dependencyLocations = [],
        dependencyEnvironments = mempty,
        dependencyTags = Map.empty
      }

spec :: Spec
spec = do
  -- This file includes an unparseable line. It should not cause an error.
  -- During regular operation the strategy outputs a warning in this case.
  condaOutputFile <- runIO (BS.readFile "test/Conda/testdata/conda-env-create-out.json")

  envDepParseSpec
  describe "build graph from conda list output" $
    it' "can build graph" $
      case eitherDecodeStrict condaOutputFile of
        Right deps -> do
          depGraph <- buildGraph <$> parseEnvCreateDeps deps
          depGraph `shouldBe'` expected
        Left err -> expectationFailure' $ "Parse JSON: " ++ err
