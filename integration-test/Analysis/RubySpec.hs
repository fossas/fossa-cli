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
    $ FixtureArtifact
      "https://github.com/rails/rails/archive/refs/tags/v6.1.4.tar.gz"
      [reldir|ruby/rails/|]
      $ Just [reldir|rails-6.1.4/|]

testRailSrcRepo :: Spec
testRailSrcRepo =
  aroundAll (withAnalysisOf rails) $ do
    describe "rails" $ do
      it "should find targets" $ \(result, extractedDir) -> do
        shouldFindProjectOf ("bundler", extractedDir) result
      it "should have expected dependency results" $ \(result, extractedDir) -> do
        (DependencyResultsSummary 210 70 293 1 Complete) `expectDepResultsSummary` (getDepResultsOf result ("bundler", extractedDir))

spec :: Spec
spec = do
  testRailSrcRepo
