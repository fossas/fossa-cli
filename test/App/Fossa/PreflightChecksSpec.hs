module App.Fossa.PreflightChecksSpec (spec) where

import App.Fossa.PreflightChecks (PreflightCommandChecks (..), preflightChecks)
import Control.Algebra (Has)
import Control.Carrier.Debug (ignoreDebug)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Test.Effect (expectFatal', it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec)
import Test.Hspec.Core.Spec (describe)
import Test.MockApi (
  MockApi,
  alwaysReturns,
  returnsOnce,
 )

expectPushToken :: Has MockApi sig m => m ()
expectPushToken = GetTokenType `alwaysReturns` Fixtures.pushToken

expectFullAccessToken :: Has MockApi sig m => m ()
expectFullAccessToken = GetTokenType `alwaysReturns` Fixtures.fullAccessToken

expectOrganizationWithPremiumSubscription :: Has MockApi sig m => m ()
expectOrganizationWithPremiumSubscription = GetOrganization `alwaysReturns` Fixtures.organizationWithPremiumSubscription

expectOrganization :: Has MockApi sig m => m ()
expectOrganization = GetOrganization `alwaysReturns` Fixtures.organization

expectOrganizationWithPreflightChecks :: Has MockApi sig m => m ()
expectOrganizationWithPreflightChecks = GetOrganization `alwaysReturns` Fixtures.organizationWithPreflightChecks

analyzeChecks :: PreflightCommandChecks
analyzeChecks = AnalyzeChecks Fixtures.projectRevision Fixtures.projectMetadata

spec :: Spec
spec = do
  describe "preflight checks" $ do
    it' "should pass all checks for test command" $ do
      expectOrganizationWithPreflightChecks
      res <- ignoreDebug $ preflightChecks TestChecks
      res `shouldBe'` ()
    it' "should pass all check for report command" $ do
      expectOrganizationWithPremiumSubscription
      expectFullAccessToken
      res <- preflightChecks ReportChecks
      res `shouldBe'` ()
    it' "should fail full access token check for report command" $ do
      expectOrganizationWithPremiumSubscription
      expectPushToken
      expectFatal' $ ignoreDebug $ preflightChecks ReportChecks
    it' "should fail premium subscription check for report command" $ do
      expectOrganizationWithPreflightChecks
      expectFullAccessToken
      expectFatal' $ ignoreDebug $ preflightChecks ReportChecks
    it' "should pass all custom upload permission checks for analyze command" $ do
      expectOrganizationWithPreflightChecks
      (GetCustomBuildPermissons Fixtures.projectRevision Fixtures.projectMetadata) `returnsOnce` Fixtures.validCustomUploadPermissions
      res <- ignoreDebug $ preflightChecks analyzeChecks
      res `shouldBe'` ()
    it' "should pass all checks while skipping permission checks for analyze command" $ do
      expectOrganization
      res <- ignoreDebug $ preflightChecks analyzeChecks
      res `shouldBe'` ()
    it' "should fail edit project check for analyze command" $ do
      expectOrganizationWithPreflightChecks
      (GetCustomBuildPermissons Fixtures.projectRevision Fixtures.projectMetadata) `returnsOnce` Fixtures.invalidEditProjectPermission
      expectFatal' $ ignoreDebug $ preflightChecks analyzeChecks
    it' "should fail create project check for analyze command" $ do
      expectOrganizationWithPreflightChecks
      (GetCustomBuildPermissons Fixtures.projectRevision Fixtures.projectMetadata) `returnsOnce` Fixtures.invalidCreateProjectPermission
      expectFatal' $ ignoreDebug $ preflightChecks analyzeChecks
    it' "should fail create team project check for analyze command" $ do
      expectOrganizationWithPreflightChecks
      (GetCustomBuildPermissons Fixtures.projectRevision Fixtures.projectMetadata) `returnsOnce` Fixtures.invalidCreateTeamProjectPermission
      expectFatal' $ ignoreDebug $ preflightChecks analyzeChecks
    it' "should fail create project only for team check for analyze command" $ do
      expectOrganizationWithPreflightChecks
      (GetCustomBuildPermissons Fixtures.projectRevision Fixtures.projectMetadata) `returnsOnce` Fixtures.invalidCreateProjectOnlyToTeamPermission
      expectFatal' $ ignoreDebug $ preflightChecks analyzeChecks
    it' "should fail edit release group check for analyze command" $ do
      expectOrganizationWithPreflightChecks
      (GetCustomBuildPermissons Fixtures.projectRevision Fixtures.projectMetadata) `returnsOnce` Fixtures.invalidEditReleaseGroupPermission
      expectFatal' $ ignoreDebug $ preflightChecks analyzeChecks
    it' "should fail create team projects for release group check for analyze command" $ do
      expectOrganizationWithPreflightChecks
      (GetCustomBuildPermissons Fixtures.projectRevision Fixtures.projectMetadata) `returnsOnce` Fixtures.invalidCreateTeamProjectsForReleaseGroupPermission
      expectFatal' $ ignoreDebug $ preflightChecks analyzeChecks
