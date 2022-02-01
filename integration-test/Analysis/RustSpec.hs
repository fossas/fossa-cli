{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.RustSpec (spec) where

import Analysis.FixtureExpectationUtils
import Analysis.FixtureUtils
import Path
import Strategy.Cargo qualified as Cargo
import Test.Hspec
import Types (DiscoveredProjectType (..), GraphBreadth (Complete))

rustEnv :: FixtureEnvironment
rustEnv = NixEnv ["rustc", "cargo"]

bat :: AnalysisTestFixture (Cargo.CargoProject)
bat =
  AnalysisTestFixture
    "bat"
    Cargo.discover
    rustEnv
    Nothing
    $ FixtureArtifact
      "https://github.com/sharkdp/bat/archive/refs/tags/v0.18.3.tar.gz"
      [reldir|rust/bat/|]
      [reldir|bat-0.18.3//|]

fd :: AnalysisTestFixture (Cargo.CargoProject)
fd =
  AnalysisTestFixture
    "fd"
    Cargo.discover
    rustEnv
    Nothing
    $ FixtureArtifact
      "https://github.com/sharkdp/fd/archive/refs/tags/v8.3.0.tar.gz"
      [reldir|rust/fd/|]
      [reldir|fd-8.3.0/|]

spec :: Spec
spec = do
  testSuiteDepResultSummary bat CargoProjectType (DependencyResultsSummary 146 29 269 1 Complete)
  testSuiteDepResultSummary fd CargoProjectType (DependencyResultsSummary 71 25 137 1 Complete)
