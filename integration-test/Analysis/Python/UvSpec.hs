{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.Python.UvSpec (spec) where

import Analysis.FixtureExpectationUtils
import Analysis.FixtureUtils
import App.Types (Mode (NonStrict))
import Path
import Strategy.Python.Uv qualified as Uv
import Test.Hspec
import Types (DiscoveredProjectType (UvProjectType), GraphBreadth (Complete))

uv :: AnalysisTestFixture (Uv.UvProject)
uv =
  AnalysisTestFixture
    "uv"
    Uv.discover
    LocalEnvironment
    Nothing
    $ FixtureArtifact
      "https://github.com/fpgmaas/cookiecutter-uv/archive/refs/tags/0.0.11.tar.gz"
      [reldir|python/uv/cookiecutter-uv/|]
      [reldir|cookiecutter-uv-0.0.11/|]

spec :: Spec
spec = do
  testSuiteDepResultSummary NonStrict uv UvProjectType (DependencyResultsSummary 72 12 122 1 Complete)
