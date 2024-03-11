module App.Fossa.ReleaseGroup.CreateSpec (
  spec,
  apiOpts',
) where

import App.Fossa.Config.ReleaseGroup.Create (CreateConfig (..))
import App.Fossa.Config.ReleaseGroup.CreateSpec (expectedReleaseGroupRevisionFromConfig)
import App.Fossa.ReleaseGroup.Create (createMain)
import Control.Algebra (Has)
import Control.Carrier.Debug (ignoreDebug)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Fossa.API.Types (ApiKey (..), ApiOpts (..), Organization (orgSupportsReleaseGroups), defaultApiPollDelay)
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

createConfig :: CreateConfig
createConfig =
  CreateConfig
    { apiOpts = apiOpts'
    , releaseGroupRevision = expectedReleaseGroupRevisionFromConfig
    }
expectCreateReleaseGroupSuccess :: Has MockApi sig m => m ()
expectCreateReleaseGroupSuccess = CreateReleaseGroup expectedReleaseGroupRevisionFromConfig `alwaysReturns` Fixtures.createReleaseGroupResponse

expectCreateReleaseGroupToFail :: Has MockApi sig m => m ()
expectCreateReleaseGroupToFail = fails (CreateReleaseGroup expectedReleaseGroupRevisionFromConfig) "fails"

spec :: Spec
spec = do
  describe "Create Release Group" $ do
    it' "should skip release group creation" $ do
      let org = Fixtures.organization
      GetOrganization `alwaysReturns` org
      res <- ignoreDebug $ createMain createConfig
      res `shouldBe'` ()

    it' "should create release group" $ do
      let org = Fixtures.organization{orgSupportsReleaseGroups = True}
      GetOrganization `alwaysReturns` org
      expectCreateReleaseGroupSuccess
      res <- ignoreDebug $ createMain createConfig
      res `shouldBe'` ()

    it' "should fail to create release group" $ do
      let org = Fixtures.organization{orgSupportsReleaseGroups = True}
      GetOrganization `alwaysReturns` org
      expectCreateReleaseGroupToFail
      expectFatal' $ ignoreDebug $ createMain createConfig
