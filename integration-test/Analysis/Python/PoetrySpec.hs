{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.Python.PoetrySpec (spec) where

import Analysis.FixtureExpectationUtils
import Analysis.FixtureUtils
import Path
import Strategy.Python.Poetry qualified as Poetry
import Test.Hspec
import Types (DiscoveredProjectType (..), GraphBreadth (Complete))

poetry :: AnalysisTestFixture (Poetry.PoetryProject)
poetry =
  AnalysisTestFixture
    "poetry"
    Poetry.discover
    LocalEnvironment
    Nothing
    $ FixtureArtifact
      "https://github.com/python-poetry/poetry/archive/72497bcb66b5a1cc20e3aa95973c523a22b05bfa.tar.gz"
      [reldir|python/poetry/poetry/|]
      [reldir|poetry-72497bcb66b5a1cc20e3aa95973c523a22b05bfa/|]

spec :: Spec
spec = do
  testSuiteDepResultSummary poetry PoetryProjectType (DependencyResultsSummary 65 29 69 1 Complete)
