module App.Fossa.PreflightChecksSpec (
  spec,
) where

import App.Fossa.PreflightChecks (preflightChecks)
import Control.Algebra (Has)
import Control.Carrier.Debug (ignoreDebug)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Test.Effect (expectFatal', it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec)
import Test.Hspec.Core.Spec (describe)
import Test.MockApi (
  MockApi,
  fails,
  returnsOnce,
 )

expectOrganization :: Has MockApi sig m => m ()
expectOrganization = GetOrganization `returnsOnce` Fixtures.organization

spec :: Spec
spec = do
  describe "preflight checks" $ 
    do
      it' "should pass all checks" $ 
        do
          expectOrganization
          res <- ignoreDebug preflightChecks
          res `shouldBe'` ()
      it' "should fail all checks"
        . expectFatal'
        $ do
          GetOrganization `fails` "Invalid API Token"
          ignoreDebug preflightChecks
