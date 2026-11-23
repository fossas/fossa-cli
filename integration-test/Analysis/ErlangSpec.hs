{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.ErlangSpec (spec) where

import Analysis.FixtureExpectationUtils
import Analysis.FixtureUtils
import App.Types (Mode (NonStrict))
import Discovery.Simple (cachedDiscover)
import Discovery.Walk (walkWithFilters')
import Path
import Strategy.Rebar3 qualified as Rebar3
import Test.Hspec
import Types (DiscoveredProjectType (..), GraphBreadth (Complete))

erlangEnv :: FixtureEnvironment
erlangEnv = NixEnv ["erlang", "rebar3"]

emqx :: AnalysisTestFixture (Rebar3.RebarProject)
emqx =
  AnalysisTestFixture
    "emqx"
    (\dir -> cachedDiscover Rebar3ProjectType Rebar3.mkProject dir (walkWithFilters' Rebar3.discoverVisitor dir))
    erlangEnv
    Nothing
    $ FixtureArtifact
      "https://github.com/emqx/emqx/archive/refs/tags/v4.3.10.tar.gz"
      [reldir|erlang/emqx/|]
      [reldir|emqx-4.3.10/|]

spec :: Spec
spec = do
  testSuiteDepResultSummary NonStrict emqx Rebar3ProjectType (DependencyResultsSummary 0 0 0 1 Complete)
