module App.Fossa.ReleaseGroup.DeleteSpec (spec) where

import App.Fossa.Config.ReleaseGroup.Delete (DeleteConfig (..))
import App.Fossa.ReleaseGroup.CreateSpec (apiOpts')
import App.Fossa.ReleaseGroup.Delete (deleteMain)
import Control.Algebra (Has)
import Control.Carrier.Debug (ignoreDebug)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Fossa.API.CoreTypes qualified as Types
import Test.Effect (expectFatal', it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe)
import Test.MockApi (MockApi, alwaysReturns, fails, returnsOnce)

deleteConfig :: DeleteConfig
deleteConfig = DeleteConfig apiOpts' "example-title"

expectDeleteReleaseGroupSuccess :: Has MockApi sig m => m ()
expectDeleteReleaseGroupSuccess = DeleteReleaseGroup 1 `alwaysReturns` ()

expectDeleteReleaseGroupToFail :: Has MockApi sig m => m ()
expectDeleteReleaseGroupToFail = fails (DeleteReleaseGroup 1) "fails"

spec :: Spec
spec = do
  describe "Delete Release Group" $ do
    it' "should fail when the release group to delete does not exist when deleting a release group" $ do
      GetReleaseGroups `returnsOnce` [Fixtures.releaseGroup{Types.releaseGroupTitle = "example-title-2"}]
      expectFatal' $ ignoreDebug $ deleteMain deleteConfig

    it' "should fail to delete release group" $ do
      GetReleaseGroups `returnsOnce` [Fixtures.releaseGroup]
      expectDeleteReleaseGroupToFail
      expectFatal' $ ignoreDebug $ deleteMain deleteConfig

    it' "should delete release group" $ do
      GetReleaseGroups `returnsOnce` [Fixtures.releaseGroup]
      expectDeleteReleaseGroupSuccess
      res <- ignoreDebug $ deleteMain deleteConfig
      res `shouldBe'` ()
