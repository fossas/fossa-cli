{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.Python.PoetrySpec (spec) where
import Path
import Strategy.Python.Poetry qualified as Poetry
import Test.Hspec
import Types (GraphBreadth (Complete))
import Analysis.FixtureExpectationUtils
import Analysis.FixtureUtils

poetry :: AnalysisTestFixture (Poetry.PoetryProject)
poetry =
  AnalysisTestFixture
    "poetry"
    Poetry.discover
    LocalEnvironment
    Nothing
    $ FixtureArtifact "https://github.com/python-poetry/poetry/archive/refs/tags/0.12.7.tar.gz" [reldir|python/poetry/poetry/|] (Just [reldir|poetry-0.12.7/|])

testPoetrySrcRepo :: Spec
testPoetrySrcRepo =
  aroundAll (withAnalysisOf poetry) $ do
    describe "poetry" $ do
      it "should find targets" $ \(result, extractedDir) -> do
        shouldFindProjectOf ("poetry", extractedDir) result
      it "should have expected dependency results" $ \(result, extractedDir) -> do
        print result
        (DependencyResultsSummary 81 0 105 1 Complete) `expectDepResultsSummary` (getDepResultsOf result ("poetry", extractedDir))


spec :: Spec
spec = do
  testPoetrySrcRepo