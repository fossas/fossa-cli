module App.Fossa.API.BuildWaitSpec (spec) where

import App.Fossa.Config.BuildWait (WaitConfig (..))
import App.Fossa.API.BuildWait (waitForBuild, waitForIssues, waitForScanCompletion)
import Control.Algebra (Has)
import Control.Carrier.Reader (ReaderC, runReader)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Control.Effect.Lift (Lift)
import Control.Monad (void)
import Control.Timeout (Cancel, Duration (MilliSeconds, Seconds), timeout')
import Data.Text (Text)
import Fossa.API.Types (
  Build (..),
  BuildStatus (StatusCreated, StatusFailed, StatusRunning, StatusSucceeded),
  BuildTask (..),
  Issues (issuesStatus),
  Project (projectIsMonorepo),
  ScanResponse (responseScanStatus),
 )
import Srclib.Types (Locator (..))
import Test.Effect (expectFatal', it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe)
import Test.MockApi (MockApi, alwaysReturns, returnsOnce)

runWithConfigAndTimeout ::
  Has (Lift IO) sig m =>
  (Cancel -> ReaderC WaitConfig m a) ->
  m ()
runWithConfigAndTimeout =
  void
    . runReader waitConfig
    . timeout' (Seconds 1)

spec :: Spec
spec =
  describe "BuildWait" $ do
    describe "waitForScanCompletion" $ do
      describe "VPS projects" $ do
        let commonExpectations = do
              expectGetOrganization
              expectGetMonorepoProject
              expectGetLatestScan
        it' "should return when the build is complete" $ do
          commonExpectations
          expectGetScan (Just "AVAILABLE")
          runWithConfigAndTimeout $
            waitForScanCompletion Fixtures.projectRevision
        it' "should retry periodically if the build is not complete" $ do
          commonExpectations
          expectGetScan (Just "NOT AVAILABLE")
          expectGetScan Nothing
          expectGetScan (Just "AVAILABLE")
          runWithConfigAndTimeout $
            waitForScanCompletion Fixtures.projectRevision
        it' "should die if the build errors" $ do
          commonExpectations
          expectGetScan (Just "ERROR")
          expectFatal' . runWithConfigAndTimeout $
            waitForScanCompletion Fixtures.projectRevision
        it' "should cancel when the timeout expires" $ do
          commonExpectations
          expectScanAlwaysPending
          expectFatal' . runWithConfigAndTimeout $
            waitForScanCompletion Fixtures.projectRevision
      describe "Non-VPS projects" $ do
        let commonExpectations = expectGetProject
        it' "should return when the build is complete" $ do
          commonExpectations
          expectGetLatestBuild StatusSucceeded
          runWithConfigAndTimeout $
            waitForScanCompletion Fixtures.projectRevision
        it' "should retry periodically if the build is not complete" $ do
          commonExpectations
          expectGetLatestBuild StatusCreated
          expectGetLatestBuild StatusRunning
          expectGetLatestBuild StatusSucceeded
          runWithConfigAndTimeout $
            waitForScanCompletion Fixtures.projectRevision
        it' "should die if the build fails" $ do
          commonExpectations
          expectGetLatestBuild StatusFailed
          expectFatal' . runWithConfigAndTimeout $
            waitForScanCompletion Fixtures.projectRevision
        it' "should cancel when the timeout expires" $ do
          commonExpectations
          expectBuildAlwaysRunning
          expectFatal' . runWithConfigAndTimeout $
            waitForScanCompletion Fixtures.projectRevision
    describe "waitForIssues" $ do
      it' "should return when the issues are avilable" $ do
        expectGetOrganization
        expectGetProject
        getIssues "AVAILABLE"
        runWithConfigAndTimeout $ \cancel -> do
          issues <- waitForIssues Fixtures.projectRevision cancel
          issues `shouldBe'` Fixtures.issues

      it' "should retry periodically if the issues are not available" $ do
        expectGetOrganization
        expectGetProject
        getIssues "WAITING"
        getIssues "AVAILABLE"
        runWithConfigAndTimeout $ \cancel -> do
          issues <- waitForIssues Fixtures.projectRevision cancel
          issues `shouldBe'` Fixtures.issues

      it' "should cancel when the timeout expires" $ do
        expectGetOrganization
        expectGetProject
        expectIssuesAlwaysWaiting
        expectFatal' . runWithConfigAndTimeout $
          waitForIssues Fixtures.projectRevision
    describe "waitForBuild" $ do
      it' "should return when the build is complete" $ do
        expectGetLatestBuild StatusSucceeded
        runWithConfigAndTimeout $
          waitForBuild Fixtures.projectRevision
      it' "should retry periodically if the build is not complete" $ do
        expectGetLatestBuild StatusCreated
        expectGetLatestBuild StatusRunning
        expectGetLatestBuild StatusSucceeded
        runWithConfigAndTimeout $
          waitForBuild Fixtures.projectRevision
      it' "should die if the build fails" $ do
        expectGetLatestBuild StatusFailed
        expectFatal' . runWithConfigAndTimeout $
          waitForBuild Fixtures.projectRevision
      it' "should cancel when the timeout expires" $ do
        expectBuildAlwaysRunning
        expectFatal' . runWithConfigAndTimeout $
          waitForBuild Fixtures.projectRevision

testVpsLocator :: Locator
testVpsLocator = Locator{locatorFetcher = "custom", locatorProject = "42/testProjectName", locatorRevision = Nothing}

waitConfig :: WaitConfig
waitConfig = WaitConfig{apiPollDelay = MilliSeconds 1}

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

getIssues :: Has MockApi sig m => Text -> m ()
getIssues issuesStatus =
  (GetIssues Fixtures.projectRevision)
    `returnsOnce` Fixtures.issues{issuesStatus = issuesStatus}

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
  (GetIssues Fixtures.projectRevision)
    `alwaysReturns` Fixtures.issues{issuesStatus = "WAITING"}
