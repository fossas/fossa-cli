{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.RubySpec (spec) where

import Analysis.FixtureExpectationUtils
import Analysis.FixtureUtils
import Path
import Strategy.Bundler qualified as Bundler
import Test.Hspec
import Types (GraphBreadth (Complete))

rails :: AnalysisTestFixture (Bundler.BundlerProject)
rails =
  AnalysisTestFixture
    "rails"
    Bundler.discover
    LocalEnvironment
    Nothing
    $ FixtureArtifact
    "https://github.com/rails/rails/archive/refs/tags/v6.1.4.tar.gz"
      [reldir|ruby/rails/|]
      [reldir|rails-6.1.4/|]

spec :: Spec
spec = do
  testSuiteDepResultSummary rails "bundler" (DependencyResultsSummary 210 70 293 1 Complete)
