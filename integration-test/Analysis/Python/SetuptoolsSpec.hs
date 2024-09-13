{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.Python.SetuptoolsSpec (spec) where

import Analysis.FixtureExpectationUtils
import Analysis.FixtureUtils
import App.Types (Mode (NonStrict))
import Path
import Strategy.Python.Setuptools qualified as Setuptools
import Test.Hspec
import Types (DiscoveredProjectType (..), GraphBreadth (..))

theFuck :: AnalysisTestFixture (Setuptools.SetuptoolsProject)
theFuck =
  AnalysisTestFixture
    "theFuck"
    Setuptools.discover
    LocalEnvironment
    Nothing
    $ FixtureArtifact
      "https://github.com/nvbn/thefuck/archive/refs/tags/3.31.tar.gz"
      [reldir|python/setuptools/thefuck/|]
      [reldir|thefuck-3.31/|]

flask :: AnalysisTestFixture (Setuptools.SetuptoolsProject)
flask =
  AnalysisTestFixture
    "flask"
    Setuptools.discover
    LocalEnvironment
    Nothing
    $ FixtureArtifact
      "https://github.com/pallets/flask/archive/refs/tags/2.0.2.tar.gz"
      [reldir|python/setuptools/flask/|]
      [reldir|flask-2.0.2/|]

spec :: Spec
spec = do
  testSuiteDepResultSummary NonStrict theFuck SetuptoolsProjectType (DependencyResultsSummary 16 16 0 2 Partial)
  testSuiteDepResultSummary NonStrict flask SetuptoolsProjectType (DependencyResultsSummary 4 4 0 1 Partial)
