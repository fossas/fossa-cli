{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.Python.PoetrySpec (spec) where

import Analysis.FixtureExpectationUtils
import Analysis.FixtureUtils
import Path
import Strategy.Python.Poetry qualified as Poetry
import Test.Hspec
import Types (GraphBreadth (Complete))

poetry :: AnalysisTestFixture (Poetry.PoetryProject)
poetry =
  AnalysisTestFixture
    "poetry"
    Poetry.discover
    LocalEnvironment
    $ FixtureArtifact
      "https://github.com/python-poetry/poetry/archive/72497bcb66b5a1cc20e3aa95973c523a22b05bfa.tar.gz"
      [reldir|python/poetry/poetry/|]
      $ Just [reldir|poetry-72497bcb66b5a1cc20e3aa95973c523a22b05bfa/|]

testPoetrySrcRepo :: Spec
testPoetrySrcRepo =
  aroundAll (withAnalysisOf poetry) $ do
    describe "poetry" $ do
      it "should find targets" $ \(result, extractedDir) -> do
        shouldFindProjectOf ("poetry", extractedDir) result
      it "should have expected dependency results" $ \(result, extractedDir) -> do
        (DependencyResultsSummary 66 29 69 1 Complete) `expectDepResultsSummary` (getDepResultsOf result ("poetry", extractedDir))

spec :: Spec
spec = do
  testPoetrySrcRepo
