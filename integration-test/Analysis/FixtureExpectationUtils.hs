module Analysis.FixtureExpectationUtils (
  DependencyResultsSummary (..),

  -- * spec hooks
  withAnalysisOf,

  -- * expectation helpers
  getDepResultsOf,
  shouldFindProjectOf,
  expectDepResultsSummary,
  expectNumDeps,
  expectNumDirects,
  expectNumEdges,
  expectDep,
  expectDirectDep,
  expectEdgeBetween,
  expectGraphBreadth,
  expectNumManifests,
) where

import Analysis.FixtureUtils (AnalysisTestFixture (..), FixtureArtifact (..), getArtifact, performDiscoveryAndAnalyses)
import App.Fossa.Analyze.Types (AnalyzeProject)
import Control.Algebra (Has)
import Control.Effect.Lift (Lift, sendIO)
import Control.Monad (unless)
import Data.List (find, isInfixOf)
import Data.String.Conversion (toString, toText)
import Data.Text (Text)
import Graphing
import Path
import Path.IO qualified as PIO
import Test.Hspec (Expectation, HasCallStack, expectationFailure, shouldNotBe)
import Types (Dependency (..), DependencyResults (..), DiscoveredProject (..), GraphBreadth)

-- | Tabulated property of the DependencyResult.
data DependencyResultsSummary = DependencyResultsSummary
  { numDeps :: Int
  , numDirectDeps :: Int
  , numEdges :: Int
  , numManifestFiles :: Int
  , graphType :: GraphBreadth
  }
  deriving (Show, Eq, Ord)

-- | Performs discovery and analysis for all discovered project fop provided analysis integration fixture.
withAnalysisOf :: (Has (Lift IO) sig m, AnalyzeProject a, MonadFail m) => AnalysisTestFixture a -> (([(DiscoveredProject a, DependencyResults)], Path Abs Dir) -> m b) -> m ()
withAnalysisOf a runTest = do
  extractedDir <- getArtifact (artifact a)
  res <- performDiscoveryAndAnalyses a
  _ <- case scopedDir . artifact $ a of
    Nothing -> runTest (res, extractedDir)
    Just sd -> runTest (res, extractedDir </> sd)
  sendIO $ PIO.removeDirRecur extractedDir

-- Expectation Helpers
-- Provides expectation functions for graph properties and dependency resulting.

-- | Retrieves a dependency result from discovered project of a given type and path.
getDepResultsOf :: [(DiscoveredProject a, DependencyResults)] -> (Text, Path Abs Dir) -> Maybe DependencyResults
getDepResultsOf result (projType, projPath) = snd <$> withProjectOfType result (projType, projPath)

-- | Expects a discovered project of type at given path.
shouldFindProjectOf :: (Show a, Eq a) => (Text, Path Abs Dir) -> [(DiscoveredProject a, DependencyResults)] -> Expectation
shouldFindProjectOf (projType, projPath) result = fst <$> withProjectOfType result (projType, projPath) `shouldNotBe` Nothing

-- | Expects number of dependencies in dependency results graphing.
expectNumDeps :: Int -> Maybe DependencyResults -> Expectation
expectNumDeps numDeps = expectOnDepGraph numDeps (shouldBeEqual "number of dependencies") (length . vertexList)

-- | Expects number of direct dependencies in dependency results graphing.
expectNumDirects :: Int -> Maybe DependencyResults -> Expectation
expectNumDirects numDirects = expectOnDepGraph numDirects (shouldBeEqual "number of direct dependencies") (length . directList)

-- | Expects number of edges in depndency graphing results.
expectNumEdges :: Int -> Maybe DependencyResults -> Expectation
expectNumEdges numEdges = expectOnDepGraph numEdges (shouldBeEqual "number of edges") (length . edgesList)

-- | Expects a dependency in dependency results graphing.
expectDep :: Dependency -> Maybe DependencyResults -> Expectation
expectDep dep = expectOnDepGraph [dep] (shouldContain "dependency within graph") vertexList

-- | Expects a direct dependency in dependency results graphing.
expectDirectDep :: Dependency -> Maybe DependencyResults -> Expectation
expectDirectDep dep = expectOnDepGraph [dep] (shouldContain "direct dependency within graph") directList

-- | Expects edge between two dependency in dependency results graphing.
expectEdgeBetween :: Dependency -> Dependency -> Maybe DependencyResults -> Expectation
expectEdgeBetween depFrom depTo = expectOnDepGraph [(depFrom, depTo)] (shouldContain "edges within graph") edgesList

-- | Expects graph breadth from dependency result.
expectGraphBreadth :: GraphBreadth -> Maybe DependencyResults -> Expectation
expectGraphBreadth breadth depResult = case depResult of
  Nothing -> expectationFailure "expected to have dependency graph, for property assertion, but received nothing!"
  Just dr -> shouldBeEqual "graph type to be" (dependencyGraphBreadth dr) breadth

-- | Expects number of manifest files from dependency result.
expectNumManifests :: Int -> Maybe DependencyResults -> Expectation
expectNumManifests numManifests depResult = case depResult of
  Nothing -> expectationFailure "expected to have dependency graph, for property assertion, but received nothing!"
  Just dr -> shouldBeEqual "graph type to be" (length $ dependencyManifestFiles dr) numManifests

-- | Expects summary of dependency results from dependency result.
expectDepResultsSummary :: DependencyResultsSummary -> Maybe DependencyResults -> Expectation
expectDepResultsSummary depResultSummary depResult = do
  expectNumEdges (numEdges depResultSummary) depResult
  expectNumDeps (numDeps depResultSummary) depResult
  expectNumDirects (numDirectDeps depResultSummary) depResult
  expectGraphBreadth (graphType depResultSummary) depResult
  expectNumManifests (numManifestFiles depResultSummary) depResult

-- --------------------------------
-- Internal Expectation helpers

withProjectOfType :: [(DiscoveredProject a, DependencyResults)] -> (Text, Path Abs Dir) -> Maybe (DiscoveredProject a, DependencyResults)
withProjectOfType result (projType, projPath) = find (\(dr, _) -> projectType dr == projType && projectPath dr == projPath) result

shouldSatisfyWithMsg :: (a -> a -> Bool) -> Text -> a -> a -> Expectation
shouldSatisfyWithMsg comparator errorMsg expected actual = expectTrue (toString errorMsg) (comparator expected actual)
  where
    expectTrue :: HasCallStack => String -> Bool -> Expectation
    expectTrue msg b = unless b (expectationFailure msg)

expectOnDepGraph :: a -> (a -> a -> Expectation) -> (Graphing Dependency -> a) -> Maybe DependencyResults -> Expectation
expectOnDepGraph expected expectationRunner getter depResult = case depResult of
  Nothing -> expectationFailure "expected to have dependency graph, for property assertion, but received nothing!"
  Just d -> expectationRunner (getter $ dependencyGraph d) expected

shouldBeEqual :: (Eq a, Show a) => Text -> a -> a -> Expectation
shouldBeEqual errorScope expected actual = shouldSatisfyWithMsg (==) errorMsg expected actual
  where
    errorMsg :: Text
    errorMsg = "For " <> errorScope <> " - expected: " <> toText (show expected) <> ", but got: " <> toText (show actual)

shouldContain :: (Eq a, Show a) => Text -> [a] -> [a] -> Expectation
shouldContain errorScope expected actual = shouldSatisfyWithMsg (isInfixOf) errorMsg expected actual
  where
    errorMsg :: Text
    errorMsg = "For " <> errorScope <> " - expected to contain: " <> toText (show actual) <> ", but it was not part of: " <> toText (show expected)
