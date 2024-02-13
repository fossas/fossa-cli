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
  fails,
  returnsOnce,
 )

expectPushToken :: Has MockApi sig m => m ()
expectPushToken = GetTokenType `returnsOnce` Fixtures.pushToken

expectFullAccessToken :: Has MockApi sig m => m ()
expectFullAccessToken = GetTokenType `returnsOnce` Fixtures.fullAccessToken

expectFreeSubscription :: Has MockApi sig m => m ()
expectFreeSubscription = GetSubscription `returnsOnce` Fixtures.freeSubscription

expectPremiumSubscription :: Has MockApi sig m => m ()
expectPremiumSubscription = GetSubscription `returnsOnce` Fixtures.premiumSubscription

expectOrganization :: Has MockApi sig m => m ()
expectOrganization = GetOrganization `alwaysReturns` Fixtures.organization

spec :: Spec
spec = do
  describe "preflight checks" $ do
    it' "should pass all checks for test command" $ do
      expectFreeSubscription
      expectFullAccessToken
      expectOrganization
      res <- ignoreDebug $ preflightChecks TestChecks
      res `shouldBe'` ()
    it' "should fail full access token check for test command" $ do
      expectOrganization
      expectFreeSubscription
      expectPushToken
      expectFatal' $ ignoreDebug $ preflightChecks TestChecks
    it' "should pass all check for report command" $ do
      expectOrganization
      expectPremiumSubscription
      expectFullAccessToken
      res <- ignoreDebug $ preflightChecks ReportChecks
      res `shouldBe'` ()
    it' "should fail full access token check for report command" $ do
      expectOrganization
      expectPremiumSubscription
      expectPushToken
      expectFatal' $ ignoreDebug $ preflightChecks ReportChecks
    it' "should fail premium subscription check for report command" $ do
      expectOrganization
      expectFreeSubscription
      expectFullAccessToken
      expectFatal' $ ignoreDebug $ preflightChecks ReportChecks

-- it' "should fail full access check for test command" . expectFatal' $ do
--   GetOrganization `fails` "Invalid API Token"
--   ignoreDebug preflightChecks ReportChecks