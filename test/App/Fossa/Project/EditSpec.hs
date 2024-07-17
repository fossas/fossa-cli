module App.Fossa.Project.EditSpec (
  spec,
  apiOpts',
) where

import App.Fossa.Config.Project.Edit (EditConfig (..), ProjectIdentifier (..))
import App.Fossa.Project.Edit (editMain)
import App.Types (Policy (..))
import Control.Algebra (Has)
import Control.Carrier.Debug (ignoreDebug)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Data.Text (Text)
import Fossa.API.CoreTypes qualified as Types
import Fossa.API.Types (ApiKey (..), ApiOpts (..), OrgId (..), Organization (organizationId), defaultApiPollDelay)
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

locator :: Text
locator = "custom+123/example-locator"

projId :: Text
projId = "example-locator"

revisionLocator :: Text
revisionLocator = locator <> "$789"

title :: Text
title = "example-title"

url :: Text
url = "example-url"

jiraKey :: Text
jiraKey = "example-jira-key"

link :: Text
link = "example-link"

policyName :: Text
policyName = "example-policy"

teamName :: Text
teamName = "example-team"

teamName2 :: Text
teamName2 = "example-team-2"

labelName :: Text
labelName = "example-label"

labelName2 :: Text
labelName2 = "example-label-2"

editConfig :: EditConfig
editConfig =
  EditConfig
    { apiOpts = apiOpts'
    , projectIdentifier = ProjectLocator locator
    , projectTitle = Just title
    , projectUrl = Just url
    , projectJiraKey = Just jiraKey
    , projectLink = Just link
    , teams = Just [teamName, teamName2]
    , projectPolicy = Just $ PolicyName policyName
    , projectLabels = Just [labelName2, labelName]
    }

emptyEditConfig :: EditConfig
emptyEditConfig =
  EditConfig
    { apiOpts = apiOpts'
    , projectIdentifier = ProjectLocator locator
    , projectTitle = Nothing
    , projectUrl = Nothing
    , projectJiraKey = Nothing
    , projectLink = Nothing
    , teams = Nothing
    , projectPolicy = Nothing
    , projectLabels = Nothing
    }

updateProjectReq :: Types.UpdateProjectRequest
updateProjectReq =
  Types.UpdateProjectRequest
    { Types.updateProjectTitle = Just title
    , Types.updateProjectUrl = Just url
    , Types.updateProjectIssueTrackerIds = Just [jiraKey]
    , Types.updateProjectLabelIds = Just [20, 21]
    , Types.updateProjectPolicyId = Just 7
    , Types.updateProjectDefaultBranch = Just "main"
    }

updateRevisionReq :: Types.UpdateRevisionRequest
updateRevisionReq = Types.UpdateRevisionRequest{Types.newRevisionLink = link}

addTeamProjectsReq :: Types.AddTeamProjectsRequest
addTeamProjectsReq =
  Types.AddTeamProjectsRequest
    { Types.projectLocators = [locator]
    , Types.action = Types.Add
    }

addTeamProjectsRes :: Types.AddTeamProjectsResponse
addTeamProjectsRes = Types.AddTeamProjectsResponse{Types.teamProjectLocators = [locator]}

label1 :: Types.Label
label1 =
  Types.Label
    { Types.labelId = 20
    , Types.labelName = labelName
    }

label2 :: Types.Label
label2 =
  Types.Label
    { Types.labelId = 21
    , Types.labelName = labelName2
    }

labels :: Types.Labels
labels = Types.Labels{Types.labels = [label1, label2]}

initialProject :: Types.Project
initialProject =
  Types.Project
    { Types.projectLocator = locator
    , Types.projectTitle = "initial-title"
    , Types.projectUrl = Nothing
    , Types.projectIssueTrackerIds = Just [jiraKey]
    , Types.projectPolicyId = Nothing
    , Types.projectLatestRevision = Just revisionLocator
    , Types.projectDefaultBranch = Just "main"
    }

updatedProject :: Types.Project
updatedProject =
  Types.Project
    { Types.projectLocator = locator
    , Types.projectTitle = title
    , Types.projectUrl = Just url
    , Types.projectIssueTrackerIds = Nothing
    , Types.projectPolicyId = Just 7
    , Types.projectLatestRevision = Just revisionLocator
    , Types.projectDefaultBranch = Just "main"
    }

revision :: Types.Revision
revision =
  Types.Revision
    { Types.revisionLocator = revisionLocator
    , Types.revisionLink = Just link
    }

expectUpdateProjectSuccess :: Has MockApi sig m => m ()
expectUpdateProjectSuccess = UpdateProject locator updateProjectReq `alwaysReturns` updatedProject

expectGetOrgSuccess :: Has MockApi sig m => m ()
expectGetOrgSuccess = GetOrganization `alwaysReturns` Fixtures.organization{organizationId = OrgId 123}

expectGetProjectSuccess :: Has MockApi sig m => m ()
expectGetProjectSuccess = GetProjectV2 locator `alwaysReturns` initialProject

expectUpdateRevisionSuccess :: Has MockApi sig m => m ()
expectUpdateRevisionSuccess = UpdateRevision revisionLocator updateRevisionReq `alwaysReturns` revision

expectGetTeamsSuccess :: Has MockApi sig m => m ()
expectGetTeamsSuccess = GetTeams `alwaysReturns` [Fixtures.team, Fixtures.team{Types.teamId = 11, Types.teamName = teamName2}]

expectGetPoliciesSuccess :: Has MockApi sig m => m ()
expectGetPoliciesSuccess = GetPolicies `alwaysReturns` [Fixtures.policy]

expectGetOrgLabelsSuccess :: Has MockApi sig m => m ()
expectGetOrgLabelsSuccess = GetOrgLabels `alwaysReturns` labels

spec :: Spec
spec = do
  describe "Edit Project" $ do
    it' "should fail when a project does not have a latest revision" $ do
      GetProjectV2 locator `returnsOnce` initialProject{Types.projectLatestRevision = Nothing}
      expectFatal' $ ignoreDebug $ editMain emptyEditConfig{projectLink = Just link}

    it' "should successfully execute with ProjectIdentifier ProjectLocator" $ do
      expectGetProjectSuccess
      expectGetPoliciesSuccess
      expectGetOrgLabelsSuccess
      expectUpdateProjectSuccess
      expectGetTeamsSuccess
      AddTeamProjects 10 addTeamProjectsReq `returnsOnce` addTeamProjectsRes
      AddTeamProjects 11 addTeamProjectsReq `returnsOnce` addTeamProjectsRes
      expectUpdateRevisionSuccess
      res <- ignoreDebug $ editMain editConfig
      res `shouldBe'` ()

    it' "should successfully execute with ProjectIdentifier ProjectId" $ do
      expectGetOrgSuccess
      expectGetProjectSuccess
      expectGetPoliciesSuccess
      expectGetOrgLabelsSuccess
      expectUpdateProjectSuccess
      expectGetTeamsSuccess
      AddTeamProjects 10 addTeamProjectsReq `returnsOnce` addTeamProjectsRes
      AddTeamProjects 11 addTeamProjectsReq `returnsOnce` addTeamProjectsRes
      expectUpdateRevisionSuccess
      res <- ignoreDebug $ editMain editConfig{projectIdentifier = ProjectId projId}
      res `shouldBe'` ()
