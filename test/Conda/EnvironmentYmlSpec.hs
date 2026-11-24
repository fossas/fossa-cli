{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Conda.EnvironmentYmlSpec (
  spec,
) where

import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Stack (runStack)
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Yaml (decodeEither', prettyPrintParseException)
import DepTypes (
  DepType (CondaType, PipType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.Grapher
import Effect.ReadFS (runReadFSIO)
import Graphing (Graphing)
import Path (mkRelFile)
import Path.IO (makeAbsolute)
import ResultUtil (assertOnSuccess)
import Strategy.Conda.EnvironmentYml (analyze, buildGraph, graphBreadth)
import Test.Hspec
import Test.Hspec qualified as T
import Text.RawString.QQ (r)
import Types (GraphBreadth (Complete, Partial))

dep :: DepType -> Text -> Text -> Dependency
dep depType depName version =
  Dependency
    { dependencyType = depType
    , dependencyName = depName
    , dependencyVersion = Just (CEq version)
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

expectedGraph :: Graphing Dependency
expectedGraph = run . evalGrapher $ do
  direct $ dep CondaType "name" "version2"
  direct $ dep CondaType "biopython" "1.78"
  direct $ dep CondaType "blas" "1.0"
  direct $ dep CondaType "ca-certificates" "2021.1.19"
  direct $ dep PipType "pytest-httpserver" "1.0.6"

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

-- | What both testdata files resolve to: the conda package and the one pip
-- requirement the parser reads. Everything else under @pip:@ is dropped.
expectedPartialGraph :: Graphing Dependency
expectedPartialGraph = run . evalGrapher $ do
  direct $ dep CondaType "python" "3.11"
  direct $ dep PipType "pytest-httpserver" "1.0.6"

spec :: T.Spec
spec = do
  -- pip entries that name no package: an -r file, a pip option, a local path
  -- and a bare URL. They used to fail the whole environment.yml with
  -- @Could not parse "-r requirements.txt"@.
  pipOptionsFile <- runIO $ makeAbsolute $(mkRelFile "test/Conda/testdata/environment-pip-options.yml")
  -- a named requirement the parser cannot read (an @in@ marker)
  unreadablePipFile <- runIO $ makeAbsolute $(mkRelFile "test/Conda/testdata/environment-unreadable-pip.yml")

  T.describe "buildGraph with real environment.yml" $
    T.it "can parse environment.yml" $ do
      case decodeEither' condaEnvFile of
        Left err -> expectationFailure $ "Failed to parse: " ++ prettyPrintParseException err
        Right deps -> do
          buildGraph deps `shouldBe` expectedGraph
          graphBreadth deps `shouldBe` Complete

  T.describe "analyze with pip entries that name no package" $
    T.it "skips -r files, pip options, paths and URLs, and reports a partial graph" $ do
      result <- analyze pipOptionsFile & runReadFSIO & runDiagnostics & runStack
      assertOnSuccess result $ \warnings (graph, breadth) -> do
        graph `shouldBe` expectedPartialGraph
        breadth `shouldBe` Partial
        warnings `shouldSatisfy` null

  T.describe "analyze with a pip requirement the parser cannot read" $
    T.it "warns about it, leaves it out and reports a partial graph" $ do
      result <- analyze unreadablePipFile & runReadFSIO & runDiagnostics & runStack
      assertOnSuccess result $ \warnings (graph, breadth) -> do
        graph `shouldBe` expectedPartialGraph
        breadth `shouldBe` Partial
        length warnings `shouldBe` 1
