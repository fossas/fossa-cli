module App.Fossa.API.BuildWaitSpec (spec) where

import App.Fossa.API.BuildWait (waitForBuild, waitForIssues, waitForScanCompletion)
import Control.Algebra (Has)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Control.Effect.Lift (Lift)
import Control.Monad (void)
import Control.Timeout (Cancel, Duration (Seconds), timeout')
import Data.Text (Text)
import Fossa.API.Types (
  Build (..),
  BuildStatus (StatusCreated, StatusFailed, StatusRunning, StatusSucceeded),
  BuildTask (..),
  Project (projectIsMonorepo),
  ScanResponse (responseScanStatus),
 )
import Srclib.Types (Locator (..))
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
      describe "VPS projects" $ do
        let commonExpectations = do
              expectGetApiOpts
              expectGetOrganization
              expectGetMonorepoProject
              expectGetLatestScan
        it' "should return when the build is complete" $ do
          commonExpectations
          expectGetScan (Just "AVAILABLE")
          runWithTimeout $
            waitForScanCompletion Fixtures.projectRevision
        it' "should retry periodically if the build is not complete" $ do
          commonExpectations
          expectGetScan (Just "NOT AVAILABLE")
          expectGetScan Nothing
          expectGetScan (Just "AVAILABLE")
          runWithTimeout $
            waitForScanCompletion Fixtures.projectRevision
        it' "should die if the build errors" $ do
          commonExpectations
          expectGetScan (Just "ERROR")
          expectFatal' . runWithTimeout $
            waitForScanCompletion Fixtures.projectRevision
        it' "should cancel when the timeout expires" $ do
          commonExpectations
          expectScanAlwaysPending
          expectFatal' . runWithTimeout $
            waitForScanCompletion Fixtures.projectRevision
      describe "Non-VPS projects" $ do
        let commonExpectations = expectGetApiOpts >> expectGetProject
        it' "should return when the build is complete" $ do
          commonExpectations
          expectGetLatestBuild StatusSucceeded
          runWithTimeout $
            waitForScanCompletion Fixtures.projectRevision
        it' "should retry periodically if the build is not complete" $ do
          commonExpectations
          expectGetLatestBuild StatusCreated
          expectGetLatestBuild StatusRunning
          expectGetLatestBuild StatusSucceeded
          runWithTimeout $
            waitForScanCompletion Fixtures.projectRevision
        it' "should die if the build fails" $ do
          commonExpectations
          expectGetLatestBuild StatusFailed
          expectFatal' . runWithTimeout $
            waitForScanCompletion Fixtures.projectRevision
        it' "should cancel when the timeout expires" $ do
          commonExpectations
          expectBuildAlwaysRunning
          expectFatal' . runWithTimeout $
            waitForScanCompletion Fixtures.projectRevision
    describe "waitForIssues" $ do
      let commonExpectations = do
            expectGetApiOpts
            expectGetOrganization
            expectGetProject
      it' "should return when the issues are avilable" $ do
        commonExpectations
        expectIssuesAvailable
        runWithTimeout $ \cancel -> do
          issues <- waitForIssues Fixtures.projectRevision Nothing cancel
          issues `shouldBe'` Fixtures.issuesAvailable

      it' "should return when the issues diffs are available" $ do
        commonExpectations
        expectDiffIssuesAvailable
        runWithTimeout $ \cancel -> do
          issues <- waitForIssues Fixtures.projectRevision (Just Fixtures.diffRevision) cancel
          issues `shouldBe'` Fixtures.issuesDiffAvailable

      it' "should retry periodically if the issues are not available" $ do
        commonExpectations
        expectIssuesPending
        expectIssuesAvailable
        runWithTimeout $ \cancel -> do
          issues <- waitForIssues Fixtures.projectRevision Nothing cancel
          issues `shouldBe'` Fixtures.issuesAvailable

      it' "should cancel when the timeout expires" $ do
        commonExpectations
        expectIssuesAlwaysWaiting
        expectFatal' . runWithTimeout $
          waitForIssues Fixtures.projectRevision Nothing
    describe "waitForBuild" $ do
      it' "should return when the build is complete" $ do
        expectGetApiOpts
        expectGetLatestBuild StatusSucceeded
        runWithTimeout $
          waitForBuild Fixtures.projectRevision
      it' "should retry periodically if the build is not complete" $ do
        expectGetApiOpts
        expectGetLatestBuild StatusCreated
        expectGetLatestBuild StatusRunning
        expectGetLatestBuild StatusSucceeded
        runWithTimeout $
          waitForBuild Fixtures.projectRevision
      it' "should die if the build fails" $ do
        expectGetApiOpts
        expectGetLatestBuild StatusFailed
        expectFatal' . runWithTimeout $
          waitForBuild Fixtures.projectRevision
      it' "should cancel when the timeout expires" $ do
        expectGetApiOpts
        expectBuildAlwaysRunning
        expectFatal' . runWithTimeout $
          waitForBuild Fixtures.projectRevision

testVpsLocator :: Locator
testVpsLocator = Locator{locatorFetcher = "custom", locatorProject = "42/testProjectName", locatorRevision = Nothing}

expectGetApiOpts :: Has MockApi sig m => m ()
expectGetApiOpts =
  GetApiOpts `alwaysReturns` Fixtures.apiOpts

expectGetOrganization :: Has MockApi sig m => m ()
expectGetOrganization = GetOrganization `alwaysReturns` Fixtures.organization

expectGetProject :: Has MockApi sig m => m ()
expectGetProject = (GetProject Fixtures.projectRevision) `alwaysReturns` Fixtures.project

expectGetMonorepoProject :: Has MockApi sig m => m ()
expectGetMonorepoProject = (GetProject Fixtures.projectRevision) `alwaysReturns` Fixtures.project{projectIsMonorepo = True}

expectGetLatestBuild :: Has MockApi sig m => BuildStatus -> m ()
expectGetLatestBuild status =
  (GetLatestBuild Fixtures.projectRevision)
    `returnsOnce` Fixtures.build{buildTask = BuildTask{buildTaskStatus = status}}

expectGetLatestScan :: Has MockApi sig m => m ()
expectGetLatestScan =
  (GetLatestScan testVpsLocator Fixtures.projectRevision)
    `returnsOnce` Fixtures.scanResponse

expectGetScan :: Has MockApi sig m => Maybe Text -> m ()
expectGetScan scanStatus =
  (GetScan testVpsLocator Fixtures.scanId)
    `returnsOnce` Fixtures.scanResponse{responseScanStatus = scanStatus}

expectIssuesAvailable :: Has MockApi sig m => m ()
expectIssuesAvailable =
  (GetIssues Fixtures.projectRevision Nothing)
    `returnsOnce` Fixtures.issuesAvailable

expectDiffIssuesAvailable :: Has MockApi sig m => m ()
expectDiffIssuesAvailable =
  GetIssues Fixtures.projectRevision (Just Fixtures.diffRevision)
    `returnsOnce` Fixtures.issuesDiffAvailable

expectIssuesPending :: Has MockApi sig m => m ()
expectIssuesPending =
  (GetIssues Fixtures.projectRevision Nothing)
    `returnsOnce` Fixtures.issuesPending

expectScanAlwaysPending :: Has MockApi sig m => m ()
expectScanAlwaysPending =
  (GetScan testVpsLocator Fixtures.scanId)
    `alwaysReturns` Fixtures.scanResponse{responseScanStatus = Nothing}

expectBuildAlwaysRunning :: Has MockApi sig m => m ()
expectBuildAlwaysRunning =
  (GetLatestBuild Fixtures.projectRevision)
    `alwaysReturns` Fixtures.build{buildTask = BuildTask{buildTaskStatus = StatusRunning}}

expectIssuesAlwaysWaiting :: Has MockApi sig m => m ()
expectIssuesAlwaysWaiting =
  (GetIssues Fixtures.projectRevision Nothing)
    `alwaysReturns` Fixtures.issuesPending
