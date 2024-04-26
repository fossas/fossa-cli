module App.Fossa.ReleaseGroup.AddProjectsSpec (
  spec,
) where

import App.Fossa.Config.ReleaseGroup.AddProjects (AddProjectsConfig (..))
import App.Fossa.Config.ReleaseGroup.CreateSpec (expectedReleaseGroupReleaseRevisionFromConfig)
import App.Fossa.ReleaseGroup.AddProjects (addProjectsMain, constructUpdateRequest)
import App.Fossa.ReleaseGroup.CreateSpec (apiOpts')
import Control.Algebra (Has)
import Control.Carrier.Debug (ignoreDebug)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Fossa.API.CoreTypes (UpdateReleaseProjectRequest (..), UpdateReleaseRequest (..))
import Fossa.API.CoreTypes qualified as Types
import Test.Effect (expectFatal', it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.MockApi (MockApi, alwaysReturns, fails, returnsOnce)

addProjectsConfig :: AddProjectsConfig
addProjectsConfig =
  AddProjectsConfig
    { apiOpts = apiOpts'
    , title = "example-title"
    , releaseGroupReleaseRevision = expectedReleaseGroupReleaseRevisionFromConfig
    }

updateReleaseRequest :: UpdateReleaseRequest
updateReleaseRequest = do
  let expectedUpdateProjectReq =
        UpdateReleaseProjectRequest
          { updateReleaseProjectLocator = "custom+1/git@github.com/fossa-cli"
          , updateReleaseProjectRevisionId = "custom+1/git@github.com/fossa-cli$12345"
          , updateReleaseProjectBranch = "main"
          , targetReleaseGroupId = Nothing
          }
  UpdateReleaseRequest
    { updatedReleaseTitle = "example-release-title"
    , updatedProjects = [expectedUpdateProjectReq]
    }

expectedRelease :: Types.ReleaseGroupRelease
expectedRelease = do
  let project =
        Fixtures.releaseProject
          { Types.releaseProjectLocator = "custom+1/git@github.com/fossa-cli"
          , Types.releaseProjectRevisionId = "custom+1/git@github.com/fossa-cli$12345"
          , Types.releaseProjectBranch = "main"
          }
  Fixtures.release{Types.releaseGroupReleaseProjects = [project]}

expectReleaseGroupAddProjectsSuccess :: Has MockApi sig m => m ()
expectReleaseGroupAddProjectsSuccess = (UpdateReleaseGroupRelease 1 2 updateReleaseRequest) `alwaysReturns` expectedRelease

expectReleaseGroupAddProjectsToFail :: Has MockApi sig m => m ()
expectReleaseGroupAddProjectsToFail = fails (UpdateReleaseGroupRelease 1 2 updateReleaseRequest) "fails"

spec :: Spec
spec = do
  constructUpdateRequestSpec

  describe "Add Release Group Projects" $ do
    it' "should fail if release group does not exist when updating a release" $ do
      GetReleaseGroups `returnsOnce` [Fixtures.releaseGroup{Types.releaseGroupTitle = "example-title-2"}]
      expectFatal' $ ignoreDebug $ addProjectsMain addProjectsConfig

    it' "should fail to update the release" $ do
      GetReleaseGroups `returnsOnce` [Fixtures.releaseGroup]
      GetReleaseGroupReleases 1 `returnsOnce` [Fixtures.release]
      expectReleaseGroupAddProjectsToFail
      expectFatal' $ ignoreDebug $ addProjectsMain addProjectsConfig

    it' "should successfully to update the release" $ do
      GetReleaseGroups `returnsOnce` [Fixtures.releaseGroup]
      GetReleaseGroupReleases 1 `returnsOnce` [Fixtures.release]
      expectReleaseGroupAddProjectsSuccess
      res <- ignoreDebug $ addProjectsMain addProjectsConfig
      res `shouldBe'` ()

constructUpdateRequestSpec :: Spec
constructUpdateRequestSpec = do
  describe "constructUpdateRequest" $ do
    it "should successfully create an UpdateReleaseRequest" $ do
      let release = Fixtures.release
          releaseRevision = expectedReleaseGroupReleaseRevisionFromConfig
          res = constructUpdateRequest release releaseRevision
      res `shouldBe` updateReleaseRequest
