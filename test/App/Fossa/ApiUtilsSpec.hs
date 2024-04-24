module App.Fossa.ApiUtilsSpec (
  spec,
) where

import App.Fossa.ApiUtils (retrieveLabelIds, retrievePolicyId, retrieveTeamIds, retrieveTeamIdsWithMaybe)
import Fossa.API.CoreTypes (Label (..), Labels (..), PolicyType (..), Team (..))
import Test.Effect (expectFatal', it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe)

spec :: Spec
spec = do
  retrievePolicyIdSpec
  retrieveTeamIdsWithMaybeSpec
  retrieveTeamIdsSpec
  retrieveLabelIdsSpec

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

retrieveTeamIdsWithMaybeSpec :: Spec
retrieveTeamIdsWithMaybeSpec = do
  describe "retrieveTeamIdsWithMaybe" $ do
    it' "should return Nothing if the team names is Nothing" $ do
      let teams = [Fixtures.team, Fixtures.team{teamId = 11, teamName = "example-team-2"}]
      res <- retrieveTeamIdsWithMaybe Nothing teams
      res `shouldBe'` Nothing

    it' "should successfully retrieve all team ids" $ do
      let teams = [Fixtures.team, Fixtures.team{teamId = 11, teamName = "example-team-2"}]
      res <- retrieveTeamIdsWithMaybe (Just ["example-team", "example-team-2"]) teams
      res `shouldBe'` Just [10, 11]

retrieveTeamIdsSpec :: Spec
retrieveTeamIdsSpec = do
  describe "retrieveTeamIds" $ do
    it' "should fail if not all team names exist in org" $ do
      let teams = [Fixtures.team, Fixtures.team{teamId = 11, teamName = "example-team-2"}]
      expectFatal' $ retrieveTeamIds (["example-team-2", "non-existent-team"]) teams

    it' "should successfully retrieve all team ids" $ do
      let teams = [Fixtures.team, Fixtures.team{teamId = 11, teamName = "example-team-2"}]
      res <- retrieveTeamIds ["example-team", "example-team-2"] teams
      res `shouldBe'` [10, 11]

retrieveLabelIdsSpec :: Spec
retrieveLabelIdsSpec = do
  describe "retrieveLabelIds" $ do
    let label1 = Label 1 "example-label"
    let label2 = Label 2 "example-label-2"
    let labels = Labels [label1, label2]

    it' "should return empty list and label warnings when no label names exists Labels" $ do
      res <- retrieveLabelIds ["non-existent-label", "non-existent-label-2"] labels
      let warning1 = "Label `non-existent-label` does not exist\nNavigate to `Organization Settings` in the FOSSA web UI to create new labels:  https://app.fossa.com/account/settings/organization"
          warning2 = "Label `non-existent-label-2` does not exist\nNavigate to `Organization Settings` in the FOSSA web UI to create new labels:  https://app.fossa.com/account/settings/organization"
          expectedWarnings = Just [warning1, warning2]
      res `shouldBe'` ([], expectedWarnings)

    it' "should return all labels" $ do
      res <- retrieveLabelIds ["example-label", "example-label-2"] labels
      res `shouldBe'` ([2, 1], Nothing)
