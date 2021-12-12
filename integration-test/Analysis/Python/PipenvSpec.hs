{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.Python.PipenvSpec (spec) where

import Analysis.FixtureExpectationUtils
import Analysis.FixtureUtils
import Path
import Strategy.Python.Pipenv qualified as Pipenv
import Test.Hspec
import Types (GraphBreadth (Complete))

pipenv :: AnalysisTestFixture (Pipenv.PipenvProject)
pipenv =
  AnalysisTestFixture
    "pipenv"
    Pipenv.discover
    LocalEnvironment
    Nothing
    $ FixtureArtifact
      "https://github.com/pypa/pipenv/archive/refs/tags/v2021.11.23.tar.gz"
      [reldir|python/pipenv/pipenv/|]
      [reldir|pipenv-2021.11.23/|]

spec :: Spec
spec = do
  testSuiteDepResultSummary pipenv "pipenv" (DependencyResultsSummary 90 90 0 1 Complete)
