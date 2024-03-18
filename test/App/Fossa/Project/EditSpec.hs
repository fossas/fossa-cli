module App.Fossa.Project.EditSpec (
  spec,
  apiOpts',
) where

import App.Fossa.Config.Project.Edit (EditConfig (..))
import App.Fossa.Project.Edit (editMain)
import App.Types (ProjectMetadataRevision (..))
import Control.Algebra (Has)
import Control.Carrier.Debug (ignoreDebug)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Fossa.API.Types (ApiKey (..), ApiOpts (..), Organization (orgSupportsProjects), defaultApiPollDelay)
import Test.Effect (expectFatal', it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe)
import Test.MockApi (MockApi, alwaysReturns, fails)

apiOpts' :: ApiOpts
apiOpts' =
  ApiOpts
    { apiOptsUri = Nothing
    , apiOptsApiKey = ApiKey "123"
    , apiOptsPollDelay = defaultApiPollDelay
    }

editConfig :: EditConfig
editConfig =
  EditConfig
    { apiOpts = apiOpts'
    , projectLocator = "locator"
    , projectMetadataRevision = projectMetadataRevision'
    }

projectMetadataRevision' :: ProjectMetadataRevision
projectMetadataRevision' =
  ProjectMetadataRevision
    { projectTitleRevision = Just "some title"
    , projectUrlRevision = Nothing
    , projectJiraKeyRevision = Nothing
    , projectLinkRevision = Nothing
    , projectTeamRevision = Nothing
    , projectPolicyRevision = Nothing
    , projectLabelRevision = []
    }

expectEditProjectSuccess :: Has MockApi sig m => m ()
expectEditProjectSuccess = EditProject "locator" projectMetadataRevision' `alwaysReturns` Fixtures.projectResponse

expectEditProjectFail :: Has MockApi sig m => m ()
expectEditProjectFail = fails (EditProject "locator" projectMetadataRevision') "fails"

spec :: Spec
spec = do
  describe "Edit Project" $ do
    it' "should fail when org does not support projects" $ do
      let org = Fixtures.organization{orgSupportsProjects = False}
      GetOrganization `alwaysReturns` org
      expectFatal' $ ignoreDebug $ editMain editConfig

    it' "should edit project" $ do
      let org = Fixtures.organization
      GetOrganization `alwaysReturns` org
      expectEditProjectSuccess
      res <- ignoreDebug $ editMain editConfig
      res `shouldBe'` ()

    it' "should fail to edit project" $ do
      let org = Fixtures.organization
      GetOrganization `alwaysReturns` org
      expectEditProjectFail
      expectFatal' $ ignoreDebug $ editMain editConfig
