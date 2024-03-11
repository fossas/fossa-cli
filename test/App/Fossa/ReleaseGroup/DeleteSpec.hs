module App.Fossa.ReleaseGroup.DeleteSpec (spec) where

import App.Fossa.Config.ReleaseGroup.Delete (DeleteConfig (..))
import App.Fossa.ReleaseGroup.CreateSpec (apiOpts')
import App.Fossa.ReleaseGroup.Delete (deleteMain)
import Control.Algebra (Has)
import Control.Carrier.Debug (ignoreDebug)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Fossa.API.Types (Organization (..))
import Test.Effect (expectFatal', it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe)
import Test.MockApi (MockApi, alwaysReturns, fails)

deleteConfig :: DeleteConfig
deleteConfig =
  DeleteConfig
    { apiOpts = apiOpts'
    , releaseTitle = "title"
    }
expectDeleteReleaseGroupSuccess :: Has MockApi sig m => m ()
expectDeleteReleaseGroupSuccess = DeleteReleaseGroup "title" `alwaysReturns` ()

expectDeleteReleaseGroupToFail :: Has MockApi sig m => m ()
expectDeleteReleaseGroupToFail = fails (DeleteReleaseGroup "title") "fails"

spec :: Spec
spec = do
  describe "Delete Release Group" $ do
    it' "should skip release group deletion" $ do
      let org = Fixtures.organization
      GetOrganization `alwaysReturns` org
      res <- ignoreDebug $ deleteMain deleteConfig
      res `shouldBe'` ()

    it' "should delete release group" $ do
      let org = Fixtures.organization{orgSupportsReleaseGroups = True}
      GetOrganization `alwaysReturns` org
      expectDeleteReleaseGroupSuccess
      res <- ignoreDebug $ deleteMain deleteConfig
      res `shouldBe'` ()

    it' "should fail to delete release group" $ do
      let org = Fixtures.organization{orgSupportsReleaseGroups = True}
      GetOrganization `alwaysReturns` org
      expectDeleteReleaseGroupToFail
      expectFatal' $ ignoreDebug $ deleteMain deleteConfig
