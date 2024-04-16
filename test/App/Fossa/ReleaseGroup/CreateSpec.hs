module App.Fossa.ReleaseGroup.CreateSpec (
  spec,
  apiOpts',
) where

import App.Fossa.Config.ReleaseGroup.Create (CreateConfig (..))
import App.Fossa.Config.ReleaseGroup.CreateSpec (expectedReleaseGroupReleaseRevisionFromConfig, expectedReleaseGroupRevisionFromConfig)
import App.Fossa.ReleaseGroup.Create (createMain)
import Control.Algebra (Has)
import Control.Carrier.Debug (ignoreDebug)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Fossa.API.CoreTypes (CreateReleaseGroupRequest (..), Policy (..), PolicyType (QUALITY, SECURITY), Team (..))
import Fossa.API.Types (ApiKey (..), ApiOpts (..), defaultApiPollDelay)
import Test.Effect (expectFatal', it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe)
import Test.MockApi (MockApi, alwaysReturns, returnsOnce)

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

createReleaseGroupReq :: CreateReleaseGroupRequest
createReleaseGroupReq =
  CreateReleaseGroupRequest
    { title = "example-title"
    , release = expectedReleaseGroupReleaseRevisionFromConfig
    , maybeLicensePolicyId = Just 70
    , maybeSecurityPolicyId = Just 71
    , maybeQualityPolicyId = Just 72
    , maybeTeamIds = Just [20, 21]
    }

expectCreateReleaseGroupSuccess :: Has MockApi sig m => m ()
expectCreateReleaseGroupSuccess = CreateReleaseGroup createReleaseGroupReq `alwaysReturns` Fixtures.createReleaseGroupResponse

spec :: Spec
spec = do
  describe "Create Release Group" $ do
    it' "should fail when a release group with the same name already exists" $ do
      GetReleaseGroups `returnsOnce` [Fixtures.releaseGroup]
      expectFatal' $ ignoreDebug $ createMain createConfig

    it' "should create release group" $ do
      GetReleaseGroups `returnsOnce` []
      GetTeams `returnsOnce` [Fixtures.team{teamId = 20, teamName = "team-1"}, Fixtures.team{teamId = 21, teamName = "team-2"}]
      GetPolicies `returnsOnce` [Fixtures.policy{policyId = 70, policyTitle = "example-license-policy"}, Fixtures.policy{policyId = 71, policyTitle = "example-security-policy", policyType = SECURITY}, Fixtures.policy{policyId = 72, policyTitle = "example-quality-policy", policyType = QUALITY}]
      expectCreateReleaseGroupSuccess
      res <- ignoreDebug $ createMain createConfig
      res `shouldBe'` ()
