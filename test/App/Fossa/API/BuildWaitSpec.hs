module App.Fossa.API.BuildWaitSpec (spec) where

import App.Fossa.API.BuildWait (WaitConfig (WaitConfig, apiPollDelay), waitForBuild, waitForIssues, waitForScanCompletion)
import Control.Algebra (Has)
import Control.Carrier.Reader (ReaderC, runReader)
import Control.Carrier.Simple (SimpleC)
import Control.Carrier.State.Strict (StateC)
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
import Test.MockApi (ApiExpectation, alwaysReturns, returnsOnce, runWithApiExpectations)

runWithApiAndTimeout ::
  (Has (Lift IO) sig m) =>
  [ApiExpectation] ->
  (Cancel -> SimpleC FossaApiClientF (StateC [ApiExpectation] (ReaderC WaitConfig m)) b) ->
  m ()
runWithApiAndTimeout expectations =
  void
    . runReader waitConfig
    . runWithApiExpectations expectations
    . timeout' (Seconds 1)

spec :: Spec
spec =
  describe "BuildWait" $ do
    describe "waitForScanCompletion" $ do
      describe "VPS projects" $ do
        let commonExpectations =
              [ getOrganizationExpectation
              , getMonorepoProjectExpectation
              , getLatestScanExpectation
              ]
        it' "should return when the build is complete" $ do
          runWithApiAndTimeout
            ( commonExpectations
                <> [ getScanExpectation (Just "AVAILABLE")
                   ]
            )
            $ waitForScanCompletion Fixtures.projectRevision
        it' "should retry periodically if the build is not complete" $ do
          runWithApiAndTimeout
            ( commonExpectations
                <> [ getScanExpectation (Just "NOT AVAILABLE")
                   , getScanExpectation Nothing
                   , getScanExpectation (Just "AVAILABLE")
                   ]
            )
            $ waitForScanCompletion Fixtures.projectRevision
        it' "should die if the build errors" $ do
          expectFatal'
            . runWithApiAndTimeout
              ( commonExpectations
                  <> [ getScanExpectation (Just "ERROR")
                     ]
              )
            $ waitForScanCompletion Fixtures.projectRevision
        it' "should cancel when the timeout expires" $
          do
            expectFatal'
            . runWithApiAndTimeout (scanForever : commonExpectations)
            $ waitForScanCompletion Fixtures.projectRevision
      describe "Non-VPS projects" $ do
        let commonExpectations = [getProjectExpectation]
        it' "should return when the build is complete" $
          do
            runWithApiAndTimeout
              ( commonExpectations
                  <> [ getLatestBuildExpectation StatusSucceeded
                     ]
              )
            $ waitForScanCompletion Fixtures.projectRevision
        it' "should retry periodically if the build is not complete" $ do
          runWithApiAndTimeout
            ( commonExpectations
                <> [ getLatestBuildExpectation StatusCreated
                   , getLatestBuildExpectation StatusRunning
                   , getLatestBuildExpectation StatusSucceeded
                   ]
            )
            $ waitForScanCompletion Fixtures.projectRevision
        it' "should die if the build fails" $
          do
            expectFatal'
            . runWithApiAndTimeout
              ( commonExpectations
                  <> [ getLatestBuildExpectation StatusFailed
                     ]
              )
            $ waitForScanCompletion Fixtures.projectRevision
        it' "should cancel when the timeout expires" $ do
          expectFatal'
            . runWithApiAndTimeout (buildForever : commonExpectations)
            $ waitForScanCompletion Fixtures.projectRevision
    describe "waitForIssues" $ do
      it' "should return when the issues are avilable" $
        do
          runWithApiAndTimeout
            [ getOrganizationExpectation
            , getProjectExpectation
            , getIssues "AVAILABLE"
            ]
          $ \cancel -> do
            issues <- waitForIssues Fixtures.projectRevision cancel
            issues `shouldBe'` Fixtures.issues

      it' "should retry periodically if the issues are not available" $
        do
          runWithApiAndTimeout
            [ getOrganizationExpectation
            , getProjectExpectation
            , getIssues "WAITING"
            , getIssues "AVAILABLE"
            ]
          $ \cancel -> do
            issues <- waitForIssues Fixtures.projectRevision cancel
            issues `shouldBe'` Fixtures.issues

      it' "should cancel when the timeout expires" $
        do
          expectFatal'
          . runWithApiAndTimeout
            [ getOrganizationExpectation
            , getProjectExpectation
            , issuesForeverWaiting
            ]
          $ waitForIssues Fixtures.projectRevision
    describe "waitForBuild" $ do
      it' "should return when the build is complete" $
        do
          runWithApiAndTimeout
            [ getLatestBuildExpectation StatusSucceeded
            ]
          $ waitForBuild Fixtures.projectRevision
      it' "should retry periodically if the build is not complete" $ do
        runWithApiAndTimeout
          [ getLatestBuildExpectation StatusCreated
          , getLatestBuildExpectation StatusRunning
          , getLatestBuildExpectation StatusSucceeded
          ]
          $ waitForBuild Fixtures.projectRevision
      it' "should die if the build fails" $
        do
          expectFatal'
          . runWithApiAndTimeout
            [ getLatestBuildExpectation StatusFailed
            ]
          $ waitForBuild Fixtures.projectRevision
      it' "should cancel when the timeout expires" $ do
        expectFatal'
          . runWithApiAndTimeout [buildForever]
          $ waitForBuild Fixtures.projectRevision

testVpsLocator :: Locator
testVpsLocator = Locator{locatorFetcher = "custom", locatorProject = "42/testProjectName", locatorRevision = Nothing}

waitConfig :: WaitConfig
waitConfig = WaitConfig{apiPollDelay = MilliSeconds 1}

getOrganizationExpectation :: ApiExpectation
getOrganizationExpectation = GetOrganization `alwaysReturns` Fixtures.organization

getProjectExpectation :: ApiExpectation
getProjectExpectation = (GetProject Fixtures.projectRevision) `alwaysReturns` Fixtures.project

getMonorepoProjectExpectation :: ApiExpectation
getMonorepoProjectExpectation = (GetProject Fixtures.projectRevision) `alwaysReturns` Fixtures.project{projectIsMonorepo = True}

getLatestBuildExpectation :: BuildStatus -> ApiExpectation
getLatestBuildExpectation status =
  (GetLatestBuild Fixtures.projectRevision)
    `returnsOnce` Fixtures.build{buildTask = BuildTask{buildTaskStatus = status}}

getLatestScanExpectation :: ApiExpectation
getLatestScanExpectation =
  (GetLatestScan testVpsLocator Fixtures.projectRevision)
    `returnsOnce` Fixtures.scanResponse

getScanExpectation :: Maybe Text -> ApiExpectation
getScanExpectation scanStatus =
  (GetScan testVpsLocator Fixtures.scanId)
    `returnsOnce` Fixtures.scanResponse{responseScanStatus = scanStatus}

getIssues :: Text -> ApiExpectation
getIssues issuesStatus =
  (GetIssues Fixtures.projectRevision)
    `returnsOnce` Fixtures.issues{issuesStatus = issuesStatus}

scanForever :: ApiExpectation
scanForever =
  (GetScan testVpsLocator Fixtures.scanId)
    `alwaysReturns` Fixtures.scanResponse{responseScanStatus = Nothing}

buildForever :: ApiExpectation
buildForever =
  (GetLatestBuild Fixtures.projectRevision)
    `alwaysReturns` Fixtures.build{buildTask = BuildTask{buildTaskStatus = StatusRunning}}

issuesForeverWaiting :: ApiExpectation
issuesForeverWaiting =
  (GetIssues Fixtures.projectRevision)
    `alwaysReturns` Fixtures.issues{issuesStatus = "WAITING"}
