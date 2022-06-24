module App.Fossa.ReportSpec (spec) where

import App.Fossa.Config.Report (ReportConfig (..), ReportOutputFormat (ReportJson), ReportType (..))
import App.Fossa.Report (fetchReport)
import Control.Algebra (Has)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Control.Timeout (Duration (MilliSeconds))
import Test.Effect (expectFatal', it')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe, runIO)
import Test.MockApi (MockApi, alwaysReturns, fails, returnsOnce)

reportConfig :: IO ReportConfig
reportConfig = do
  baseDir <- Fixtures.baseDir
  pure
    ReportConfig
      { apiOpts = Fixtures.apiOpts
      , baseDir = baseDir
      , outputFormat = ReportJson
      , timeoutDuration = MilliSeconds 100
      , reportType = Attribution
      , revision = Fixtures.projectRevision
      }

spec :: Spec
spec =
  describe "Report" $ do
    config <- runIO reportConfig
    it' "should timeout if build-completion doesn't complete" $ do
      expectBuildPending
      expectFatal' $ fetchReport config
    it' "should timeout if issue-generation doesn't complete" $ do
      expectBuildSuccess
      expectFetchIssuesPending
      expectFatal' $ fetchReport config
    it' "should fetch a report when the build and issues are ready" $ do
      expectBuildSuccess
      expectFetchIssuesSuccess
      expectFetchReportSuccess
      fetchReport config
    it' "should die if fetching the build fails" $ do
      expectBuildError
      expectFatal' $ fetchReport config
    it' "should die if fetching issues fails" $ do
      expectBuildSuccess
      expectFetchIssuesError
      expectFatal' $ fetchReport config
    it' "should die if fetching the report fails" $ do
      expectBuildSuccess
      expectFetchIssuesSuccess
      expectFetchReportError
      expectFatal' $ fetchReport config

expectBuildSuccess :: (Has MockApi sig m) => m ()
expectBuildSuccess = do
  (GetProject Fixtures.projectRevision) `returnsOnce` Fixtures.project
  (GetLatestBuild Fixtures.projectRevision) `returnsOnce` Fixtures.successfulBuild

expectBuildPending :: (Has MockApi sig m) => m ()
expectBuildPending = do
  GetApiOpts `alwaysReturns` Fixtures.apiOpts -- It needs to fetch the poll delay
  (GetProject Fixtures.projectRevision) `returnsOnce` Fixtures.project
  (GetLatestBuild Fixtures.projectRevision) `alwaysReturns` Fixtures.pendingBuild

expectBuildError :: (Has MockApi sig m) => m ()
expectBuildError = do
  (GetProject Fixtures.projectRevision) `returnsOnce` Fixtures.project
  (GetLatestBuild Fixtures.projectRevision) `fails` "Mock failure: GetLatestBuild"

expectFetchIssuesSuccess :: (Has MockApi sig m) => m ()
expectFetchIssuesSuccess =
  (GetIssues Fixtures.projectRevision Nothing) `returnsOnce` Fixtures.issuesAvailable

expectFetchIssuesPending :: (Has MockApi sig m) => m ()
expectFetchIssuesPending = do
  GetApiOpts `alwaysReturns` Fixtures.apiOpts -- It needs to fetch the poll delay
  (GetIssues Fixtures.projectRevision Nothing) `alwaysReturns` Fixtures.issuesPending

expectFetchIssuesError :: (Has MockApi sig m) => m ()
expectFetchIssuesError =
  (GetIssues Fixtures.projectRevision Nothing) `fails` "Mock failure: GetIssues"

expectFetchReportSuccess :: (Has MockApi sig m) => m ()
expectFetchReportSuccess =
  (GetAttribution Fixtures.projectRevision ReportJson)
    `returnsOnce` Fixtures.attributionReportAsSerializedJson

expectFetchReportError :: (Has MockApi sig m) => m ()
expectFetchReportError =
  (GetAttribution Fixtures.projectRevision ReportJson)
    `fails` "Mock failure: GetAttribution"
