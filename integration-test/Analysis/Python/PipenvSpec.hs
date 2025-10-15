{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.Python.PipenvSpec (spec) where

import Analysis.FixtureExpectationUtils
import Analysis.FixtureUtils
import App.Types (Mode (NonStrict))
import Path
import Strategy.Python.Pipenv qualified as Pipenv
import Test.Hspec
import Types (DiscoveredProjectType (PipenvProjectType), GraphBreadth (Complete))

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
  testSuiteDepResultSummary NonStrict pipenv PipenvProjectType (DependencyResultsSummary 90 6 0 2 Complete)
