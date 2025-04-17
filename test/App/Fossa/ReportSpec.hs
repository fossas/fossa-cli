module App.Fossa.ReportSpec (spec) where

import App.Fossa.Config.Report (ReportConfig (..), ReportOutputFormat (ReportJson), ReportType (..), parseReportOutputFormat)
import App.Fossa.Report (fetchReport)
import App.Types (LocatorType (..))
import Control.Algebra (Has)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Control.Timeout (Duration (MilliSeconds))
import Data.Foldable (for_)
import Fossa.API.Types (RevisionDependencyCache (RevisionDependencyCache), RevisionDependencyCacheStatus (Ready))
import Test.Effect (expectFatal', it')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.Core.Spec (fdescribe)
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
      , fetchSBOMReport = False
      }

spec :: Spec
spec =
  describe "Report" $ do
    baseConfig <- runIO reportConfig
    customBuildSpec baseConfig
    sbomBuildSpec baseConfig

customBuildSpec :: ReportConfig -> Spec
customBuildSpec config = describe "Custom build" $ do
  let buildType = LocatorTypeCustom
  it' "should timeout if build-completion doesn't complete" $ do
    expectGetOrganization
    expectBuildPending buildType
    expectBuildPending LocatorTypeSBOM
    expectFatal' $ fetchReport config
  it' "should timeout if issue-generation doesn't complete" $ do
    expectGetOrganization
    expectBuildSuccess buildType
    expectFetchIssuesPending buildType
    expectFatal' $ fetchReport config
  it' "should fetch a report when the build and issues are ready" $ do
    expectGetOrganization
    expectBuildSuccess buildType
    expectFetchIssuesSuccess buildType
    expectFetchRevisionDependencyCacheSuccess buildType
    expectFetchReportSuccess buildType
    fetchReport config
  it' "should die if fetching the build fails" $ do
    expectGetOrganization
    expectBuildError buildType
    -- It will try to see if there's an SBOM with that name, so this failure happens too.
    expectBuildError LocatorTypeSBOM
    expectFatal' $ fetchReport config
  it' "should die if fetching issues fails" $ do
    expectGetOrganization
    expectBuildSuccess buildType
    expectFetchIssuesError buildType
    expectFatal' $ fetchReport config
  it' "should die if fetching the report fails" $ do
    expectGetOrganization
    expectBuildSuccess buildType
    expectFetchIssuesSuccess buildType
    expectFetchRevisionDependencyCacheSuccess buildType
    expectFetchReportError buildType
    expectFatal' $ fetchReport config

  parseReportOutputSpec

sbomBuildSpec :: ReportConfig -> Spec
sbomBuildSpec config = describe "Custom build" $ do
  let config' = config{fetchSBOMReport = True}
      buildType = LocatorTypeSBOM
  it' "should timeout if build-completion doesn't complete" $ do
    expectGetOrganization
    expectBuildPending buildType
    expectFatal' $ fetchReport config'
  it' "should timeout if issue-generation doesn't complete" $ do
    expectGetOrganization
    expectBuildSuccess buildType
    expectFetchIssuesPending buildType
    expectFatal' $ fetchReport config'
  it' "should fetch a report when the build and issues are ready" $ do
    expectGetOrganization
    expectBuildSuccess buildType
    expectFetchIssuesSuccess buildType
    expectFetchRevisionDependencyCacheSuccess buildType
    expectFetchReportSuccess buildType
    fetchReport config'
  it' "should die if fetching the build fails" $ do
    expectGetOrganization
    expectBuildError buildType
    expectFatal' $ fetchReport config'
  it' "should die if fetching issues fails" $ do
    expectGetOrganization
    expectBuildSuccess buildType
    expectFetchIssuesError buildType
    expectFatal' $ fetchReport config'
  it' "should die if fetching the report fails" $ do
    expectGetOrganization
    expectBuildSuccess buildType
    expectFetchIssuesSuccess buildType
    expectFetchRevisionDependencyCacheSuccess buildType
    expectFetchReportError buildType
    expectFatal' $ fetchReport config'

  parseReportOutputSpec

parseReportOutputSpec :: Spec
parseReportOutputSpec =
  describe "Every value of ReportOutputJson can be parsed from a string matching its Show instance" $
    for_ (enumFromTo minBound maxBound) $
      \reportFmt ->
        let fmt = show reportFmt
         in it ("Parses \"" <> fmt <> "\"") $ (parseReportOutputFormat fmt) `shouldBe` Just reportFmt

expectBuildSuccess :: (Has MockApi sig m) => LocatorType -> m ()
expectBuildSuccess locType = do
  (GetProject Fixtures.projectRevision locType) `returnsOnce` Fixtures.project
  (GetLatestBuild Fixtures.projectRevision locType) `returnsOnce` Fixtures.successfulBuild

expectBuildPending :: (Has MockApi sig m) => LocatorType -> m ()
expectBuildPending locType = do
  GetApiOpts `alwaysReturns` Fixtures.apiOpts -- It needs to fetch the poll delay
  (GetProject Fixtures.projectRevision locType) `returnsOnce` Fixtures.project
  (GetLatestBuild Fixtures.projectRevision locType) `alwaysReturns` Fixtures.pendingBuild

expectBuildError :: (Has MockApi sig m) => LocatorType -> m ()
expectBuildError locType = do
  (GetProject Fixtures.projectRevision locType) `returnsOnce` Fixtures.project
  (GetLatestBuild Fixtures.projectRevision locType) `fails` "Mock failure: GetLatestBuild"

expectFetchIssuesSuccess :: (Has MockApi sig m) => LocatorType -> m ()
expectFetchIssuesSuccess locType =
  (GetIssues Fixtures.projectRevision Nothing locType) `returnsOnce` Fixtures.issuesAvailable

expectFetchIssuesPending :: (Has MockApi sig m) => LocatorType -> m ()
expectFetchIssuesPending locType = do
  GetApiOpts `alwaysReturns` Fixtures.apiOpts -- It needs to fetch the poll delay
  (GetIssues Fixtures.projectRevision Nothing locType) `alwaysReturns` Fixtures.issuesPending

expectFetchIssuesError :: (Has MockApi sig m) => LocatorType -> m ()
expectFetchIssuesError locType =
  (GetIssues Fixtures.projectRevision Nothing locType) `fails` "Mock failure: GetIssues"

expectFetchReportSuccess :: (Has MockApi sig m) => LocatorType -> m ()
expectFetchReportSuccess locType =
  (GetAttribution Fixtures.projectRevision ReportJson locType)
    `returnsOnce` Fixtures.attributionReportAsSerializedJson

expectFetchReportError :: (Has MockApi sig m) => LocatorType -> m ()
expectFetchReportError locType =
  (GetAttribution Fixtures.projectRevision ReportJson locType)
    `fails` "Mock failure: GetAttribution"

expectFetchRevisionDependencyCacheSuccess :: (Has MockApi sig m) => LocatorType -> m ()
expectFetchRevisionDependencyCacheSuccess locType =
  (GetRevisionDependencyCacheStatus Fixtures.projectRevision locType)
    `returnsOnce` RevisionDependencyCache Ready

expectGetOrganization :: Has MockApi sig m => m ()
expectGetOrganization = GetOrganization `alwaysReturns` Fixtures.organization
