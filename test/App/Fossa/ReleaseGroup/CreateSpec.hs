module App.Fossa.ReleaseGroup.CreateSpec (
  spec,
  apiOpts',
) where

import App.Fossa.Config.ReleaseGroup.Create (CreateConfig (..))
import App.Fossa.Config.ReleaseGroup.CreateSpec (expectedReleaseGroupReleaseRevisionFromConfig, expectedReleaseGroupRevisionFromConfig)
import App.Fossa.ReleaseGroup.Create (createMain, retrievePolicyId, retrieveTeamIds)
import Control.Algebra (Has)
import Control.Carrier.Debug (ignoreDebug)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Fossa.API.Types (ApiKey (..), ApiOpts (..), CreateReleaseGroupRequest (..), Policy (..), PolicyType (LICENSING, QUALITY, SECURITY), Team (..), defaultApiPollDelay)
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
    retrievePolicyIdSpec
    retrieveTeanIdsSpec

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

retrievePolicyIdSpec :: Spec
retrievePolicyIdSpec = do
  describe "retrievePolicyId" $ do
    it' "should fail when the policy title does not exist" $ do
      let policies = [Fixtures.policy]
      expectFatal' $ retrievePolicyId (Just "non-existent-policy-name") LICENSING policies

    it' "should fail when the policy type does not match" $ do
      let policies = [Fixtures.policy]
      expectFatal' $ retrievePolicyId (Just "example-policy") SECURITY policies

    it' "should fail when there are multiple policies with the same name" $ do
      let policies = [Fixtures.policy, Fixtures.policy]
      expectFatal' $ retrievePolicyId (Just "example-policy") SECURITY policies

    it' "should return Nothing" $ do
      let policies = [Fixtures.policy]
      res <- retrievePolicyId Nothing LICENSING policies
      res `shouldBe'` Nothing

    it' "should successfully retrieve policy id" $ do
      let policies = [Fixtures.policy]
      res <- retrievePolicyId (Just "example-policy") LICENSING policies
      res `shouldBe'` Just 7

retrieveTeanIdsSpec :: Spec
retrieveTeanIdsSpec = do
  describe "retrieveTeamIds" $ do
    it' "should fail if not all team names exist in org" $ do
      let teams = [Fixtures.team, Fixtures.team{teamId = 11, teamName = "example-team-2"}]
      expectFatal' $ retrieveTeamIds (Just ["example-team-2", "non-existent-team"]) teams

    it' "should successfully retrieve all team ids" $ do
      let teams = [Fixtures.team, Fixtures.team{teamId = 11, teamName = "example-team-2"}]
      res <- retrieveTeamIds Nothing teams
      res `shouldBe'` Nothing

    it' "should successfully retrieve all team ids" $ do
      let teams = [Fixtures.team, Fixtures.team{teamId = 11, teamName = "example-team-2"}]
      res <- retrieveTeamIds (Just ["example-team", "example-team-2"]) teams
      res `shouldBe'` Just [10, 11]
