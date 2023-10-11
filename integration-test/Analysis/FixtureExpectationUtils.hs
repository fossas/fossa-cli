module Analysis.FixtureExpectationUtils (
  DependencyResultsSummary (..),

  -- * spec hooks
  withAnalysisOf,

  -- * spec helpers
  testSuiteDepResultSummary,
  testSuiteHasSomeDepResults,

  -- * expectation helpers
  getDepResultsOf,
  expectProject,
  expectDepResultsSummary,
) where

import Analysis.FixtureUtils (AnalysisTestFixture (..), FixtureArtifact (..), getArtifact, performDiscoveryAndAnalyses)
import App.Fossa.Analyze.Types (AnalyzeProject)
import Control.Algebra (Has)
import Control.Effect.Lift (Lift, sendIO)
import Data.List (find)
import Data.Maybe (isJust)
import Data.String.Conversion (toString)
import Graphing
import Path
import Path.IO qualified as PIO
import Test.Hspec (Expectation, Spec, aroundAll, describe, expectationFailure, it, shouldBe, shouldNotBe)
import Types (DependencyResults (..), DiscoveredProject (..), DiscoveredProjectType (..), GraphBreadth)

-- | Tabulated property of the DependencyResult.
data DependencyResultsSummary = DependencyResultsSummary
  { numDeps :: Int
  , numDirectDeps :: Int
  , numEdges :: Int
  , numManifestFiles :: Int
  , graphType :: GraphBreadth
  }
  deriving (Show, Eq, Ord)

summarize :: DependencyResults -> DependencyResultsSummary
summarize dr =
  DependencyResultsSummary
    (length . vertexList $ gr)
    (length . directList $ gr)
    (length . edgesList $ gr)
    (length . dependencyManifestFiles $ dr)
    (dependencyGraphBreadth dr)
  where
    gr = dependencyGraph dr

-- | Performs discovery and analysis for all discovered project for provided analysis integration fixture.
withAnalysisOf ::
  (Has (Lift IO) sig m, AnalyzeProject a, MonadFail m) =>
  AnalysisTestFixture a ->
  (([(DiscoveredProject a, DependencyResults)], Path Abs Dir) -> m b) ->
  m ()
withAnalysisOf a runTest = do
  extractedDir <- getArtifact (artifact a)
  res <- performDiscoveryAndAnalyses extractedDir a
  _ <- runTest (res, extractedDir </> (scopedDir . artifact $ a))
  sendIO $ PIO.removeDirRecur extractedDir

-- | Retrieves a dependency result from discovered project of a given type and path.
getDepResultsOf ::
  [(DiscoveredProject a, DependencyResults)] ->
  (DiscoveredProjectType, Path Abs Dir) ->
  Maybe DependencyResults
getDepResultsOf result (projType, projPath) =
  snd <$> withProjectOfType result (projType, projPath)

-- | Expects a discovered project of type at given path.
expectProject ::
  (Show a, Eq a) =>
  (DiscoveredProjectType, Path Abs Dir) ->
  [(DiscoveredProject a, DependencyResults)] ->
  Expectation
expectProject (projType, projPath) result =
  fst <$> withProjectOfType result (projType, projPath) `shouldNotBe` Nothing

-- | Expects summary of dependency results from dependency result.
expectDepResultsSummary :: DependencyResultsSummary -> Maybe DependencyResults -> Expectation
expectDepResultsSummary depResultSummary depResult = case depResult of
  Nothing -> expectationFailure "expected to have dependency graph, for property assertion, but received nothing!"
  Just dr -> summarize dr `shouldBe` depResultSummary

withProjectOfType ::
  [(DiscoveredProject a, DependencyResults)] ->
  (DiscoveredProjectType, Path Abs Dir) ->
  Maybe (DiscoveredProject a, DependencyResults)
withProjectOfType result (projType, projPath) =
  find (\(dr, _) -> projectType dr == projType && projectPath dr == projPath) result

testSuiteHasSomeDepResults :: (AnalyzeProject a, Show a, Eq a) => AnalysisTestFixture a -> DiscoveredProjectType -> Spec
testSuiteHasSomeDepResults fixture projType = aroundAll (withAnalysisOf fixture) $
  describe (toString $ testName fixture) $
    do
      it "should find targets" $ \(result, extractedDir) ->
        expectProject (projType, extractedDir) result

      it "should have some dependency results" $ \(result, extractedDir) ->
        isJust (getDepResultsOf result (projType, extractedDir)) `shouldBe` True

testSuiteDepResultSummary :: (AnalyzeProject a, Show a, Eq a) => AnalysisTestFixture a -> DiscoveredProjectType -> DependencyResultsSummary -> Spec
testSuiteDepResultSummary fixture projType depResultSummary =
  aroundAll (withAnalysisOf fixture) $
    describe (toString $ testName fixture) $
      do
        it "should find targets" $ \(result, extractedDir) ->
          expectProject (projType, extractedDir) result

        it "should have expected dependency results" $ \(result, extractedDir) ->
          depResultSummary `expectDepResultsSummary` getDepResultsOf result (projType, extractedDir)
