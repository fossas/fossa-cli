module App.Fossa.ReportSpec (spec) where

import App.Fossa.Config.Report (ReportConfig (..), ReportOutputFormat (ReportJson), ReportType (..), parseReportOutputFormat)
import App.Fossa.Report (fetchReport)
import App.Types (IssueLocatorType (..))
import Control.Algebra (Has)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Control.Timeout (Duration (MilliSeconds))
import Data.Foldable (for_)
import Fossa.API.Types (RevisionDependencyCache (RevisionDependencyCache), RevisionDependencyCacheStatus (Ready))
import Test.Effect (expectFatal', it')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
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
      expectGetOrganization
      expectBuildPending
      expectFatal' $ fetchReport config
    it' "should timeout if issue-generation doesn't complete" $ do
      expectGetOrganization
      expectBuildSuccess
      expectFetchIssuesPending
      expectFatal' $ fetchReport config
    it' "should fetch a report when the build and issues are ready" $ do
      expectGetOrganization
      expectBuildSuccess
      expectFetchIssuesSuccess
      expectFetchRevisionDependencyCacheSuccess
      expectFetchReportSuccess
      fetchReport config
    it' "should die if fetching the build fails" $ do
      expectGetOrganization
      expectBuildError
      expectFatal' $ fetchReport config
    it' "should die if fetching issues fails" $ do
      expectGetOrganization
      expectBuildSuccess
      expectFetchIssuesError
      expectFatal' $ fetchReport config
    it' "should die if fetching the report fails" $ do
      expectGetOrganization
      expectBuildSuccess
      expectFetchIssuesSuccess
      expectFetchRevisionDependencyCacheSuccess
      expectFetchReportError
      expectFatal' $ fetchReport config

    parseReportOutputSpec

parseReportOutputSpec :: Spec
parseReportOutputSpec =
  describe "Every value of ReportOutputJson can be parsed from a string matching its Show instance" $
    for_ (enumFromTo minBound maxBound) $
      \reportFmt ->
        let fmt = show reportFmt
         in it ("Parses \"" <> fmt <> "\"") $ (parseReportOutputFormat fmt) `shouldBe` Just reportFmt

expectBuildSuccess :: (Has MockApi sig m) => m ()
expectBuildSuccess = do
  (GetProject Fixtures.projectRevision IssueLocatorCustom) `returnsOnce` Fixtures.project
  (GetLatestBuild Fixtures.projectRevision IssueLocatorCustom) `returnsOnce` Fixtures.successfulBuild

expectBuildPending :: (Has MockApi sig m) => m ()
expectBuildPending = do
  GetApiOpts `alwaysReturns` Fixtures.apiOpts -- It needs to fetch the poll delay
  (GetProject Fixtures.projectRevision IssueLocatorCustom) `returnsOnce` Fixtures.project
  (GetLatestBuild Fixtures.projectRevision IssueLocatorCustom) `alwaysReturns` Fixtures.pendingBuild

expectBuildError :: (Has MockApi sig m) => m ()
expectBuildError = do
  (GetProject Fixtures.projectRevision IssueLocatorCustom) `returnsOnce` Fixtures.project
  (GetLatestBuild Fixtures.projectRevision IssueLocatorCustom) `fails` "Mock failure: GetLatestBuild"

expectFetchIssuesSuccess :: (Has MockApi sig m) => m ()
expectFetchIssuesSuccess =
  (GetIssues Fixtures.projectRevision Nothing IssueLocatorCustom) `returnsOnce` Fixtures.issuesAvailable

expectFetchIssuesPending :: (Has MockApi sig m) => m ()
expectFetchIssuesPending = do
  GetApiOpts `alwaysReturns` Fixtures.apiOpts -- It needs to fetch the poll delay
  (GetIssues Fixtures.projectRevision Nothing IssueLocatorCustom) `alwaysReturns` Fixtures.issuesPending

expectFetchIssuesError :: (Has MockApi sig m) => m ()
expectFetchIssuesError =
  (GetIssues Fixtures.projectRevision Nothing IssueLocatorCustom) `fails` "Mock failure: GetIssues"

expectFetchReportSuccess :: (Has MockApi sig m) => m ()
expectFetchReportSuccess =
  (GetAttribution Fixtures.projectRevision ReportJson)
    `returnsOnce` Fixtures.attributionReportAsSerializedJson

expectFetchReportError :: (Has MockApi sig m) => m ()
expectFetchReportError =
  (GetAttribution Fixtures.projectRevision ReportJson)
    `fails` "Mock failure: GetAttribution"

expectFetchRevisionDependencyCacheSuccess :: (Has MockApi sig m) => m ()
expectFetchRevisionDependencyCacheSuccess =
  (GetRevisionDependencyCacheStatus Fixtures.projectRevision)
    `returnsOnce` RevisionDependencyCache Ready

expectGetOrganization :: Has MockApi sig m => m ()
expectGetOrganization = GetOrganization `alwaysReturns` Fixtures.organization
