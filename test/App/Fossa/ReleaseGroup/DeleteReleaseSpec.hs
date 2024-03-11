module App.Fossa.ReleaseGroup.DeleteReleaseSpec (
  spec,
) where

import App.Fossa.Config.ReleaseGroup.DeleteRelease (DeleteReleaseConfig (..))
import App.Fossa.ReleaseGroup.CreateSpec (apiOpts')
import App.Fossa.ReleaseGroup.DeleteRelease (deleteReleaseMain)
import Control.Algebra (Has)
import Control.Carrier.Debug (ignoreDebug)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Fossa.API.Types (Organization (..))
import Test.Effect (expectFatal', it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe)
import Test.MockApi (MockApi, alwaysReturns, fails)

deleteReleaseConfig :: DeleteReleaseConfig
deleteReleaseConfig =
  DeleteReleaseConfig
    { apiOpts = apiOpts'
    , releaseGroupTitle = "title"
    , releaseGroupRelease = "release"
    }
expectDeleteReleaseGroupReleaseSuccess :: Has MockApi sig m => m ()
expectDeleteReleaseGroupReleaseSuccess = DeleteReleaseGroupRelease "title" "release" `alwaysReturns` ()

expectDeleteReleaseGroupReleaseToFail :: Has MockApi sig m => m ()
expectDeleteReleaseGroupReleaseToFail = fails (DeleteReleaseGroupRelease "title" "release") "fails"

spec :: Spec
spec = do
  describe "Delete Release Group Release" $ do
    it' "should skip release group release deletion" $ do
      let org = Fixtures.organization
      GetOrganization `alwaysReturns` org
      res <- ignoreDebug $ deleteReleaseMain deleteReleaseConfig
      res `shouldBe'` ()

    it' "should delete release group release" $ do
      let org = Fixtures.organization{orgSupportsReleaseGroups = True}
      GetOrganization `alwaysReturns` org
      expectDeleteReleaseGroupReleaseSuccess
      res <- ignoreDebug $ deleteReleaseMain deleteReleaseConfig
      res `shouldBe'` ()

    it' "should fail to delete release group release" $ do
      let org = Fixtures.organization{orgSupportsReleaseGroups = True}
      GetOrganization `alwaysReturns` org
      expectDeleteReleaseGroupReleaseToFail
      expectFatal' $ ignoreDebug $ deleteReleaseMain deleteReleaseConfig
