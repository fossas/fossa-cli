module App.Fossa.API.BuildWaitSpec (spec) where

import App.Fossa.API.BuildWait (waitForBuild, waitForIssues, waitForReportReadiness, waitForScanCompletion)
import App.Types (LocatorType (..))
import Control.Algebra (Has)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Control.Effect.Lift (Lift)
import Control.Monad (void)
import Control.Timeout (Cancel, Duration (Seconds), timeout')
import Fossa.API.Types (
  Build (..),
  BuildStatus (StatusCreated, StatusFailed, StatusRunning, StatusSucceeded),
  BuildTask (..),
  RevisionDependencyCache (RevisionDependencyCache),
  RevisionDependencyCacheStatus (Ready, Waiting),
 )
import Test.Effect (expectFatal', it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe)
import Test.MockApi (MockApi, alwaysReturns, returnsOnce)

runWithTimeout :: Has (Lift IO) sig m => (Cancel -> m a) -> m ()
runWithTimeout = void . timeout' (Seconds 1)

spec :: Spec
spec =
  describe "BuildWait" $ do
    describe "waitForScanCompletion" $ do
      let commonExpectations = expectGetApiOpts >> expectGetProject
      it' "should return when the build is complete" $ do
        commonExpectations
        expectGetLatestBuild StatusSucceeded
        runWithTimeout $
          waitForScanCompletion Fixtures.projectRevision LocatorTypeCustom
      it' "should retry periodically if the build is not complete" $ do
        commonExpectations
        expectGetLatestBuild StatusCreated
        expectGetLatestBuild StatusRunning
        expectGetLatestBuild StatusSucceeded
        runWithTimeout $
          waitForScanCompletion Fixtures.projectRevision LocatorTypeCustom
      it' "should die if the build fails" $ do
        commonExpectations
        expectGetLatestBuild StatusFailed
        expectFatal' . runWithTimeout $
          waitForScanCompletion Fixtures.projectRevision LocatorTypeCustom
      it' "should cancel when the timeout expires" $ do
        commonExpectations
        expectBuildAlwaysRunning
        expectFatal' . runWithTimeout $
          waitForScanCompletion Fixtures.projectRevision LocatorTypeCustom
    describe "waitForIssues" $ do
      let commonExpectations = do
            expectGetApiOpts
            expectGetOrganization
            expectGetProject
      it' "should return when the issues are avilable" $ do
        commonExpectations
        expectIssuesAvailable
        runWithTimeout $ \cancel -> do
          issues <- waitForIssues Fixtures.projectRevision Nothing LocatorTypeCustom cancel
          issues `shouldBe'` Fixtures.issuesAvailable

      it' "should return when the issues diffs are available" $ do
        commonExpectations
        expectDiffIssuesAvailable
        runWithTimeout $ \cancel -> do
          issues <- waitForIssues Fixtures.projectRevision (Just Fixtures.diffRevision) LocatorTypeCustom cancel
          issues `shouldBe'` Fixtures.issuesDiffAvailable

      it' "should retry periodically if the issues are not available" $ do
        commonExpectations
        expectIssuesPending
        expectIssuesAvailable
        runWithTimeout $ \cancel -> do
          issues <- waitForIssues Fixtures.projectRevision Nothing LocatorTypeCustom cancel
          issues `shouldBe'` Fixtures.issuesAvailable

      it' "should cancel when the timeout expires" $ do
        commonExpectations
        expectIssuesAlwaysWaiting
        expectFatal' . runWithTimeout $
          waitForIssues Fixtures.projectRevision Nothing LocatorTypeCustom
    describe "waitForBuild" $ do
      it' "should return when the build is complete" $ do
        expectGetApiOpts
        expectGetLatestBuild StatusSucceeded
        runWithTimeout $
          waitForBuild Fixtures.projectRevision LocatorTypeCustom
      it' "should retry periodically if the build is not complete" $ do
        expectGetApiOpts
        expectGetLatestBuild StatusCreated
        expectGetLatestBuild StatusRunning
        expectGetLatestBuild StatusSucceeded
        runWithTimeout $
          waitForBuild Fixtures.projectRevision LocatorTypeCustom
      it' "should die if the build fails" $ do
        expectGetApiOpts
        expectGetLatestBuild StatusFailed
        expectFatal' . runWithTimeout $
          waitForBuild Fixtures.projectRevision LocatorTypeCustom
      it' "should cancel when the timeout expires" $ do
        expectGetApiOpts
        expectBuildAlwaysRunning
        expectFatal' . runWithTimeout $
          waitForBuild Fixtures.projectRevision LocatorTypeCustom

    describe "waitForReportReadiness" $ do
      it' "should return when the dependency cache are ready" $ do
        expectGetApiOpts
        expectGetOrganization
        expectIssuesAvailable
        expectGetRevisionCache Ready
        runWithTimeout $
          waitForReportReadiness Fixtures.projectRevision

      it' "should retry periodically if the revision's dependency cache is in waiting state" $ do
        expectGetApiOpts
        expectGetOrganization
        expectIssuesAvailable
        expectGetRevisionCache Waiting
        expectGetRevisionCache Waiting
        expectGetRevisionCache Waiting
        expectGetRevisionCache Ready
        runWithTimeout $
          waitForReportReadiness Fixtures.projectRevision

      it' "should cancel when the timeout expires" $ do
        expectGetApiOpts
        expectGetOrganization
        expectIssuesAvailable
        expectRevisionDependencyCacheAlwaysWaiting
        expectFatal' . runWithTimeout $
          waitForReportReadiness Fixtures.projectRevision

expectGetApiOpts :: Has MockApi sig m => m ()
expectGetApiOpts =
  GetApiOpts `alwaysReturns` Fixtures.apiOpts

expectGetOrganization :: Has MockApi sig m => m ()
expectGetOrganization = GetOrganization `alwaysReturns` Fixtures.organization

expectGetProject :: Has MockApi sig m => m ()
expectGetProject = (GetProject Fixtures.projectRevision LocatorTypeCustom) `alwaysReturns` Fixtures.project

expectGetLatestBuild :: Has MockApi sig m => BuildStatus -> m ()
expectGetLatestBuild status =
  (GetLatestBuild Fixtures.projectRevision LocatorTypeCustom)
    `returnsOnce` Fixtures.build{buildTask = BuildTask{buildTaskStatus = status}}

expectGetRevisionCache :: Has MockApi sig m => RevisionDependencyCacheStatus -> m ()
expectGetRevisionCache status =
  (GetRevisionDependencyCacheStatus Fixtures.projectRevision)
    `returnsOnce` (RevisionDependencyCache status)

expectIssuesAvailable :: Has MockApi sig m => m ()
expectIssuesAvailable =
  (GetIssues Fixtures.projectRevision Nothing LocatorTypeCustom)
    `returnsOnce` Fixtures.issuesAvailable

expectDiffIssuesAvailable :: Has MockApi sig m => m ()
expectDiffIssuesAvailable =
  GetIssues Fixtures.projectRevision (Just Fixtures.diffRevision) LocatorTypeCustom
    `returnsOnce` Fixtures.issuesDiffAvailable

expectIssuesPending :: Has MockApi sig m => m ()
expectIssuesPending =
  (GetIssues Fixtures.projectRevision Nothing) LocatorTypeCustom
    `returnsOnce` Fixtures.issuesPending

expectBuildAlwaysRunning :: Has MockApi sig m => m ()
expectBuildAlwaysRunning =
  (GetLatestBuild Fixtures.projectRevision LocatorTypeCustom)
    `alwaysReturns` Fixtures.build{buildTask = BuildTask{buildTaskStatus = StatusRunning}}

expectRevisionDependencyCacheAlwaysWaiting :: Has MockApi sig m => m ()
expectRevisionDependencyCacheAlwaysWaiting =
  (GetRevisionDependencyCacheStatus Fixtures.projectRevision)
    `alwaysReturns` RevisionDependencyCache Waiting

expectIssuesAlwaysWaiting :: Has MockApi sig m => m ()
expectIssuesAlwaysWaiting =
  (GetIssues Fixtures.projectRevision Nothing LocatorTypeCustom)
    `alwaysReturns` Fixtures.issuesPending
