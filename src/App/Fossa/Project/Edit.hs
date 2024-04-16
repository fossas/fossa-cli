{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Project.Edit (
  editMain,
) where

import App.Fossa.ApiUtils (retrieveLabelIds, retrievePolicyId, retrieveTeamIds)
import App.Fossa.Config.Project.Edit (EditConfig (..), ProjectIdentifier (..))
import App.Types (Policy (..))
import Control.Algebra (Has)
import Control.Carrier.Diagnostics (context, errHelp)
import Control.Carrier.StickyLogger (logSticky)
import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Effect.FossaApiClient (FossaApiClient, addTeamProjects, getOrgLabels, getOrganization, getPolicies, getProjectV2, getTeams, updateProject, updateRevision)
import Control.Effect.Lift (Lift)
import Control.Effect.StickyLogger (StickyLogger)
import Control.Monad (void, when)
import Data.Foldable (traverse_)
import Data.Maybe (isJust)
import Data.String.Conversion (toText)
import Data.Text (Text, intercalate)
import Effect.Logger (Logger, logDebug, logInfo, logStdout, pretty)
import Fossa.API.CoreTypes (AddTeamProjectsRequest (..), AddTeamProjectsResponse (teamProjectLocators), PolicyType (LICENSING), Project (..), TeamProjectAction (Add), UpdateProjectRequest (..), UpdateRevisionRequest (UpdateRevisionRequest))
import Fossa.API.Types (Organization (organizationId))
import Text.Pretty.Simple (pShow)

editMain ::
  ( Has Diagnostics sig m
  , Has Logger sig m
  , Has (Lift IO) sig m
  , Has FossaApiClient sig m
  , Has StickyLogger sig m
  ) =>
  EditConfig ->
  m ()
editMain EditConfig{..} = do
  logInfo "Running FOSSA project edit"

  projectLocator <- case projectIdentifier of
    ProjectLocator projectLocator -> pure projectLocator
    ProjectId projectId -> do
      org <- getOrganization
      pure $ constructProjectLocatorFromProjectId org projectId

  project <- getProjectV2 projectLocator

  attemptToUpdateProjectMetadata projectLocator (projectDefaultBranch project) $ projectIssueTrackerIds project
  attemptToAddProjectToTeams projectLocator teams
  attemptToUpdateRevisionMetadata projectLocator (projectLatestRevision project)

  logStdout $ "Project " <> "`" <> projectLocator <> "` has been updated." <> "\n"
  where
    constructProjectLocatorFromProjectId :: Organization -> Text -> Text
    constructProjectLocatorFromProjectId org projId = "custom+" <> toText (show $ organizationId org) <> "/" <> projId

    attemptToUpdateProjectMetadata ::
      ( Has FossaApiClient sig m
      , Has Diagnostics sig m
      , Has StickyLogger sig m
      , Has Logger sig m
      ) =>
      Text ->
      Maybe Text ->
      Maybe [Text] ->
      m ()
    attemptToUpdateProjectMetadata projectLocator maybeDefaultBranch currentIssueTrackerIds = do
      --   Ensure that at least one project metadata field is being updated, otherwise the endpoint will throw an error
      when (or [isJust projectTitle, isJust projectUrl, isJust projectJiraKey, isJust projectLabels, isJust projectPolicy]) $ do
        logSticky "Updating project metadata"
        maybePolicyId <- case projectPolicy of
          Nothing -> pure Nothing
          Just policy -> case policy of
            PolicyId policyId -> pure $ Just policyId
            PolicyName policyName -> do
              policies <- getPolicies
              context "Retrieving license policy ID" $ retrievePolicyId (Just policyName) LICENSING policies

        orgLabels <- getOrgLabels
        maybeLabelIds <- case projectLabels of
          Nothing -> pure Nothing
          Just labels -> context "Retrieving label Ids" $ Just <$> retrieveLabelIds (labels) orgLabels

        -- Updating the project's issue tracker ids is an all or nothing transaction.
        -- Add the specified jira key to the current list of issue tracker ids if it isn't already present.
        -- If it is present in the list, use the project's existing issue tracker ids to ensure nothing is overwritten.
        let udpatedIssueTrackerIds = case projectJiraKey of
              Nothing -> Nothing
              Just jiraKey -> Just . updateIssueTrackerIds jiraKey =<< currentIssueTrackerIds
        let req =
              UpdateProjectRequest
                { updateProjectTitle = projectTitle
                , updateProjectUrl = projectUrl
                , updateProjectIssueTrackerIds = udpatedIssueTrackerIds
                , updateProjectLabelIds = maybeLabelIds
                , updateProjectPolicyId = maybePolicyId
                , -- This field needs to be set, otherwise the default branch will be removed
                  updateProjectDefaultBranch = maybeDefaultBranch
                }
        res <- updateProject projectLocator req
        logDebug $ "Updated project: " <> pretty (pShow res)

    updateIssueTrackerIds :: Text -> [Text] -> [Text]
    updateIssueTrackerIds jiraKey currentIssueTrackerIds =
      if jiraKey `elem` currentIssueTrackerIds
        then currentIssueTrackerIds
        else currentIssueTrackerIds ++ [jiraKey]

    attemptToUpdateRevisionMetadata ::
      ( Has FossaApiClient sig m
      , Has Diagnostics sig m
      , Has StickyLogger sig m
      ) =>
      Text ->
      Maybe Text ->
      m ()
    attemptToUpdateRevisionMetadata projectLocator maybeRevisionLocator = do
      case projectLink of
        Nothing -> pure ()
        Just projectLink' -> case maybeRevisionLocator of
          Nothing ->
            errHelp ("Ensure your project has completed a scan in order to update the revision link" :: Text) $
              fatalText $
                "No revision found for project: `" <> projectLocator <> "`"
          Just revisionLocator -> do
            logSticky "Updating revision metadata"
            void $ updateRevision revisionLocator $ UpdateRevisionRequest projectLink'

    attemptToAddProjectToTeams ::
      ( Has FossaApiClient sig m
      , Has Diagnostics sig m
      , Has Logger sig m
      , Has StickyLogger sig m
      ) =>
      Text ->
      Maybe [Text] ->
      m ()
    attemptToAddProjectToTeams projectLocator maybeTeamNames = do
      case maybeTeamNames of
        Nothing -> pure ()
        Just teamNames -> do
          logSticky $ "Adding Project: `" <> projectLocator <> "` to Teams: `" <> intercalate ", " teamNames <> "`"
          orgTeams <- getTeams
          teamIds <- context "Retrieving team IDs" $ retrieveTeamIds teamNames orgTeams

          logSticky "Teams in org:"
          logDebug $ pretty (pShow orgTeams)
          traverse_ (addTeamProjectWithLogs projectLocator) teamIds

    addTeamProjectWithLogs ::
      ( Has FossaApiClient sig m
      , Has Logger sig m
      , Has StickyLogger sig m
      ) =>
      Text ->
      Int ->
      m ()
    addTeamProjectWithLogs projectLocator teamId = do
      res <- addTeamProjects teamId $ AddTeamProjectsRequest [projectLocator] Add
      logSticky $ "Project: `" <> projectLocator <> "` added to teamId: `" <> toText teamId <> "`"
      logSticky $ "All projects in teamID: `" <> toText teamId <> "`"
      logDebug $ pretty (pShow (teamProjectLocators res))
