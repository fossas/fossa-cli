{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.ErlangSpec (spec) where

import Analysis.FixtureExpectationUtils
import Analysis.FixtureUtils
import Path
import Strategy.Rebar3 qualified as Rebar3
import Test.Hspec
import Types (GraphBreadth (Complete))

erlangEnv :: FixtureEnvironment
erlangEnv = NixEnv ["erlang", "rebar3"]

cowboy :: AnalysisTestFixture (Rebar3.RebarProject)
cowboy =
  AnalysisTestFixture
    "cowboy"
    Rebar3.discover
    erlangEnv
    Nothing
    $ FixtureArtifact
      "https://github.com/ninenines/cowboy/archive/refs/tags/2.9.0.tar.gz"
      [reldir|erlang/cowboy/|]
      [reldir|cowboy-2.9.0/|]

emqx :: AnalysisTestFixture (Rebar3.RebarProject)
emqx =
  AnalysisTestFixture
    "emqx"
    Rebar3.discover
    erlangEnv
    Nothing
    $ FixtureArtifact
      "https://github.com/emqx/emqx/archive/refs/tags/v4.3.10.tar.gz"
      [reldir|erlang/emqx/|]
      [reldir|emqx-4.3.10/|]

spec :: Spec
spec = do
  testSuiteDepResultSummary cowboy "rebar3" (DependencyResultsSummary 2 2 0 1 Complete)
  testSuiteDepResultSummary emqx "rebar3" (DependencyResultsSummary 0 0 0 1 Complete)
