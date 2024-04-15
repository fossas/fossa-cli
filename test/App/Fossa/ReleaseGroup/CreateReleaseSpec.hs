module App.Fossa.ReleaseGroup.CreateReleaseSpec (
  spec,
) where

import App.Fossa.Config.ReleaseGroup.CreateRelease (CreateReleaseConfig (..))
import App.Fossa.Config.ReleaseGroup.CreateSpec (expectedReleaseGroupReleaseRevisionFromConfig)
import App.Fossa.ReleaseGroup.CreateRelease (createReleaseMain)
import App.Fossa.ReleaseGroup.CreateSpec (apiOpts')
import Control.Algebra (Has)
import Control.Carrier.Debug (ignoreDebug)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Fossa.API.Types qualified as Types
import Test.Effect (expectFatal', it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe)
import Test.MockApi (MockApi, alwaysReturns, fails, returnsOnce)

createReleaseConfig :: CreateReleaseConfig
createReleaseConfig =
  CreateReleaseConfig
    { apiOpts = apiOpts'
    , releaseGroupTitle = "example-title"
    , releaseGroupReleaseRevision = expectedReleaseGroupReleaseRevisionFromConfig
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

expectReleaseGroupCreateReleaseSuccess :: Has MockApi sig m => m ()
expectReleaseGroupCreateReleaseSuccess = (CreateReleaseGroupRelease 1 expectedReleaseGroupReleaseRevisionFromConfig) `alwaysReturns` expectedRelease

expectReleaseGroupCreateReleaseToFail :: Has MockApi sig m => m ()
expectReleaseGroupCreateReleaseToFail = fails (CreateReleaseGroupRelease 1 expectedReleaseGroupReleaseRevisionFromConfig) "fails"

spec :: Spec
spec = do
  describe "Create Release Group Release" $ do
    it' "should fail if release group does not exist when creating a release" $ do
      GetReleaseGroups `returnsOnce` [Fixtures.releaseGroup{Types.releaseGroupTitle = "non-existent-release-group-title"}]
      expectFatal' $ ignoreDebug $ createReleaseMain createReleaseConfig

    it' "should fail to create a release group release" $ do
      GetReleaseGroups `returnsOnce` [Fixtures.releaseGroup]
      expectReleaseGroupCreateReleaseToFail
      expectFatal' $ ignoreDebug $ createReleaseMain createReleaseConfig

    it' "should successfully create a release group release" $ do
      GetReleaseGroups `returnsOnce` [Fixtures.releaseGroup]
      expectReleaseGroupCreateReleaseSuccess
      res <- ignoreDebug $ createReleaseMain createReleaseConfig
      res `shouldBe'` ()