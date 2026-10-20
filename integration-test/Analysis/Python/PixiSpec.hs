{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.Python.PixiSpec (spec) where

import Analysis.FixtureExpectationUtils
import Analysis.FixtureUtils
import App.Types (Mode (NonStrict))
import Path
import Strategy.Pixi qualified as Pixi
import Test.Hspec
import Types (DiscoveredProjectType (PixiProjectType), GraphBreadth (Complete))

-- | A v7 lock that installs the project itself as `pypi: .`.
snakemakeSlurm :: AnalysisTestFixture (Pixi.PixiProject)
snakemakeSlurm =
  AnalysisTestFixture
    "snakemake-executor-plugin-slurm"
    Pixi.discover
    LocalEnvironment
    Nothing
    $ FixtureArtifact
      "https://github.com/snakemake/snakemake-executor-plugin-slurm/archive/refs/tags/v2.8.0.tar.gz"
      [reldir|python/pixi/snakemake-executor-plugin-slurm/|]
      [reldir|snakemake-executor-plugin-slurm-2.8.0/|]

-- | A v6 lock that installs the project itself as `pypi: ./`, in its dev environment only.
bigwigLoader :: AnalysisTestFixture (Pixi.PixiProject)
bigwigLoader =
  AnalysisTestFixture
    "bigwig-loader"
    Pixi.discover
    LocalEnvironment
    Nothing
    $ FixtureArtifact
      "https://github.com/pfizer-opensource/bigwig-loader/archive/refs/tags/v0.3.2.tar.gz"
      [reldir|python/pixi/bigwig-loader/|]
      [reldir|bigwig-loader-0.3.2/|]

spec :: Spec
spec = do
  testSuiteDepResultSummary NonStrict snakemakeSlurm PixiProjectType (DependencyResultsSummary 109 9 218 1 Complete)
  testSuiteDepResultSummary NonStrict bigwigLoader PixiProjectType (DependencyResultsSummary 336 26 1245 1 Complete)
