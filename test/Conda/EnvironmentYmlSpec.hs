{-# LANGUAGE QuasiQuotes #-}
module Conda.EnvironmentYmlSpec (
  spec,
) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Text.RawString.QQ (r)
import Data.Yaml (decodeEither', prettyPrintParseException)
import DepTypes (
  DepType (CondaType, PipType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.Grapher
import Graphing (Graphing)
import Strategy.Conda.EnvironmentYml (buildGraph)
import Test.Hspec
import Test.Hspec qualified as T

dependencyOne :: Dependency
dependencyOne =
  Dependency
    { dependencyType = CondaType
    , dependencyName = "name"
    , dependencyVersion = Just (CEq "version1")
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

dependencyTwo :: Dependency
dependencyTwo =
  Dependency
    { dependencyType = CondaType
    , dependencyName = "name"
    , dependencyVersion = Just (CEq "version2")
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

dependencyThree :: Dependency
dependencyThree =
  Dependency
    { dependencyType = CondaType
    , dependencyName = "name"
    , dependencyVersion = Nothing
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

expectedGraph :: Graphing Dependency
expectedGraph = run . evalGrapher $ do
  direct dependencyTwo
  direct $
    Dependency
      { dependencyType = CondaType
      , dependencyName = "biopython"
      , dependencyVersion = Just (CEq "1.78")
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }
  direct $
    Dependency
      { dependencyType = CondaType
      , dependencyName = "blas"
      , dependencyVersion = Just (CEq "1.0")
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }
  direct $
    Dependency
      { dependencyType = CondaType
      , dependencyName = "ca-certificates"
      , dependencyVersion = Just (CEq "2021.1.19")
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }
  direct $
    Dependency {
       dependencyType = PipType
      , dependencyName = "pytest-httpserver"
      , dependencyVersion = Just (CEq "1.0.6")
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }

condaEnvFile :: BS.ByteString
condaEnvFile =
  [r|name: testName
channels:
  - defaults
dependencies:
  - biopython=1.78=py38haf1e3a3_0
  - blas=1.0=mkl
  - name=version2
  - ca-certificates=2021.1.19=hecd8cb5_1
  - pip:
    - pytest-httpserver==1.0.6
prefix: /path/to/env
|]

spec :: T.Spec
spec = do
  T.describe "buildGraph with real environment.yml" $
    T.it "can parse environment.yml" $ do
      case decodeEither' condaEnvFile of
        Left err -> expectationFailure $ "Failed to parse: " ++ prettyPrintParseException err
        Right deps -> buildGraph deps `shouldBe` expectedGraph
