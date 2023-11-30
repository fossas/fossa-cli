{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.Python.SetuptoolsSpec (spec) where

import Analysis.FixtureExpectationUtils
import Analysis.FixtureUtils
import Effect.Exec (AllowErr (..), Command (..))
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

pipInstallProjectCmd :: Command
pipInstallProjectCmd =
  Command
    "python3"
    [ "-m"
    , "venv"
    , "../venv"
    , "&& source .venv/bin/activate"
    , "&& python -m pip install ."
    , "&& python -m pip install -r requirements.txt"
    ]
    Never

pythonDynamic :: AnalysisTestFixture (Setuptools.SetuptoolsProject)
pythonDynamic =
  AnalysisTestFixture
    "pythonDynamic"
    Setuptools.discover
    LocalEnvironment
    (Just pipInstallProjectCmd)
    $ FixtureArtifact
      "https://github.com/fossas/example-projects/archive/c52751b1dc8495a1127b59d9d3a1e3e783651e0b.zip"
      [reldir|python/setuptools/example/|]
      [reldir|example-projects-c52751b1dc8495a1127b59d9d3a1e3e783651e0b/python/setuptools/|]

spec :: Spec
spec = do
  testSuiteDepResultSummary theFuck SetuptoolsProjectType (DependencyResultsSummary 16 16 0 2 Partial)
  testSuiteDepResultSummary flask SetuptoolsProjectType (DependencyResultsSummary 4 4 0 1 Partial)
  testSuiteDepResultSummary pythonDynamic SetuptoolsProjectType (DependencyResultsSummary 10 4 7 2 Partial)
