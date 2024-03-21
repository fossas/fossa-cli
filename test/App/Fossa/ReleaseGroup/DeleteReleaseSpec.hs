module App.Fossa.ReleaseGroup.DeleteReleaseSpec (
  spec,
) where

import App.Fossa.Config.ReleaseGroup.DeleteRelease (DeleteReleaseConfig (..))
import App.Fossa.ReleaseGroup.CreateSpec (apiOpts')
import App.Fossa.ReleaseGroup.DeleteRelease (deleteReleaseMain)
import Control.Algebra (Has)
import Control.Carrier.Debug (ignoreDebug)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Fossa.API.Types qualified as Types
import Test.Effect (expectFatal', it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe)
import Test.MockApi (MockApi, alwaysReturns, fails, returnsOnce)

deleteReleaseConfig :: DeleteReleaseConfig
deleteReleaseConfig =
  DeleteReleaseConfig
    { apiOpts = apiOpts'
    , releaseGroupTitle = "example-title"
    , releaseGroupReleaseTitle = "example-release-title"
    }

expectDeleteReleaseGroupReleaseSuccess :: Has MockApi sig m => m ()
expectDeleteReleaseGroupReleaseSuccess = DeleteReleaseGroupRelease "1" "2" `alwaysReturns` ()

expectDeleteReleaseGroupReleaseToFail :: Has MockApi sig m => m ()
expectDeleteReleaseGroupReleaseToFail = fails (DeleteReleaseGroupRelease "1" "2") "fails"

spec :: Spec
spec = do
  describe "Delete Release Group Release" $ do
    it' "should fail if release group does not exist when deleting a release" $ do
      GetReleaseGroups `returnsOnce` [Fixtures.releaseGroup{Types.releaseGroupTitle = "example-title-2"}]
      expectFatal' $ ignoreDebug $ deleteReleaseMain deleteReleaseConfig

    it' "should fail when there is only one release" $ do
      GetReleaseGroups `returnsOnce` [Fixtures.releaseGroup]
      GetReleaseGroupReleases "1" `returnsOnce` [Fixtures.release]
      expectFatal' $ ignoreDebug $ deleteReleaseMain deleteReleaseConfig

    it' "should fail to delete release group release" $ do
      GetReleaseGroups `returnsOnce` [Fixtures.releaseGroup]
      GetReleaseGroupReleases "1" `returnsOnce` [Fixtures.release, Fixtures.release{Types.releaseGroupReleaseTitle = "example-release-title-2"}]
      expectDeleteReleaseGroupReleaseToFail
      expectFatal' $ ignoreDebug $ deleteReleaseMain deleteReleaseConfig

    it' "should delete release group release" $ do
      GetReleaseGroups `returnsOnce` [Fixtures.releaseGroup]
      GetReleaseGroupReleases "1" `returnsOnce` [Fixtures.release, Fixtures.release{Types.releaseGroupReleaseTitle = "example-release-title-2"}]
      expectDeleteReleaseGroupReleaseSuccess
      res <- ignoreDebug $ deleteReleaseMain deleteReleaseConfig
      res `shouldBe'` ()
