module App.Fossa.ReleaseGroup.AddProjectsSpec (
  spec,
) where

import App.Fossa.Config.ReleaseGroup.AddProjects (AddProjectsConfig (..))
import App.Fossa.Config.ReleaseGroup.CreateSpec (expectedReleaseGroupReleaseRevisionFromConfig)
import App.Fossa.ReleaseGroup.AddProjects (addProjectsMain)
import App.Fossa.ReleaseGroup.CreateSpec (apiOpts')
import Control.Algebra (Has)
import Control.Carrier.Debug (ignoreDebug)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Fossa.API.Types (Organization (..))
import Test.Effect (expectFatal', it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe)
import Test.MockApi (MockApi, alwaysReturns, fails)

addProjectsConfig :: AddProjectsConfig
addProjectsConfig =
  AddProjectsConfig
    { apiOpts = apiOpts'
    , title = "title"
    , releaseGroupReleaseRevision = expectedReleaseGroupReleaseRevisionFromConfig
    }
expectReleaseGroupAddProjectsSuccess :: Has MockApi sig m => m ()
expectReleaseGroupAddProjectsSuccess = (AddReleaseGroupProjects "title" expectedReleaseGroupReleaseRevisionFromConfig) `alwaysReturns` Fixtures.addReleaseGroupProjectsResponse

expectReleaseGroupAddProjectsToFail :: Has MockApi sig m => m ()
expectReleaseGroupAddProjectsToFail = fails (AddReleaseGroupProjects "title" expectedReleaseGroupReleaseRevisionFromConfig) "fails"

spec :: Spec
spec = do
  describe "Add Release Group Projects" $ do
    it' "should fail when org does not support release groups" $ do
      let org = Fixtures.organization{orgSupportsReleaseGroups = False}
      GetOrganization `alwaysReturns` org
      expectFatal' $ ignoreDebug $ addProjectsMain addProjectsConfig

    it' "should add projects to release groups" $ do
      let org = Fixtures.organization
      GetOrganization `alwaysReturns` org
      expectReleaseGroupAddProjectsSuccess
      res <- ignoreDebug $ addProjectsMain addProjectsConfig
      res `shouldBe'` ()

    it' "should fail adding projects to release group" $ do
      let org = Fixtures.organization
      GetOrganization `alwaysReturns` org
      expectReleaseGroupAddProjectsToFail
      expectFatal' $ ignoreDebug $ addProjectsMain addProjectsConfig
