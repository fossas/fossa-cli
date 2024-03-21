{-# LANGUAGE RecordWildCards #-}

module App.Fossa.ReleaseGroup.AddProjects (
  addProjectsMain,
  constructUpdateRequest,
) where

import App.Fossa.Config.ReleaseGroup.AddProjects (AddProjectsConfig (..))
import App.Fossa.ReleaseGroup.Common (retrieveReleaseGroupId, retrieveReleaseGroupRelease)
import App.Types (ReleaseGroupProjectRevision (..), ReleaseGroupReleaseRevision (..))
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Effect.FossaApiClient (FossaApiClient, getReleaseGroupReleases, getReleaseGroups, updateReleaseGroupRelease)
import Control.Effect.Lift (Lift)
import Data.Set qualified as Set
import Data.String.Conversion (ToText (..))
import Data.Text (Text)
import Effect.Logger (Logger, logDebug, logInfo, logStdout, pretty)
import Fossa.API.Types (ReleaseGroupRelease (..), ReleaseProject (..), UpdateReleaseProjectRequest (..), UpdateReleaseRequest (..))
import Text.Pretty.Simple (pShow)

addProjectsMain ::
  ( Has Diagnostics sig m
  , Has Logger sig m
  , Has (Lift IO) sig m
  , Has FossaApiClient sig m
  ) =>
  AddProjectsConfig ->
  m ()
addProjectsMain AddProjectsConfig{..} = do
  logInfo "Running FOSSA release-group add-projects"

  releaseGroups <- getReleaseGroups
  maybeReleaseGroupId <- retrieveReleaseGroupId title releaseGroups
  case maybeReleaseGroupId of
    Nothing -> fatalText $ "Release group `" <> title <> "` not found"
    Just releaseGroupId -> do
      let releaseGroupIdText = toText releaseGroupId

      releases <- getReleaseGroupReleases releaseGroupIdText
      release <- retrieveReleaseGroupRelease (releaseTitle releaseGroupReleaseRevision) releases
      let releaseId = releaseGroupReleaseId release
      logDebug $ "The releases : " <> pretty (pShow (releases))

      let updateReleaseRequest = constructUpdateRequest release releaseGroupReleaseRevision
      res <- updateReleaseGroupRelease releaseGroupIdText (toText releaseId) updateReleaseRequest
      logStdout $ "Projects were added to release group release id: " <> toText (releaseGroupReleaseId res)
      logDebug $ "Projects added to release: " <> pretty (pShow (releaseGroupReleaseProjects res))

constructUpdateRequest :: ReleaseGroupRelease -> ReleaseGroupReleaseRevision -> UpdateReleaseRequest
constructUpdateRequest targetRelease releaseRevision = do
  let currentProjectLocators = projectLocatorSet targetRelease
      projectsReq = map (constructUpdateProjectRequest currentProjectLocators) $ releaseProjects releaseRevision

  UpdateReleaseRequest (releaseTitle releaseRevision) projectsReq
  where
    -- Given the release you want to modify, construct a set of the current projects in the release
    projectLocatorSet :: ReleaseGroupRelease -> Set.Set Text
    projectLocatorSet ReleaseGroupRelease{..} = Set.fromList $ map releaseProjectLocator releaseGroupReleaseProjects

    -- `maybeReleaseGroupId` in UpdateReleaseProjectRequest is used by CORE to identify if the project is a new project or if it needs an update.
    -- If the value Nothing then it means that it is a new project, otherwise it is an existing project that will be updated.
    -- TODO: An update means that the project will be queued for scanning. With the current logic, even if the revisionId and branch remain the same,
    --       we are still re-scanning the project. We can introduce a stricter filtering mechanism to only update the project if the revisionId / branch differ from the existing values
    constructUpdateProjectRequest :: Set.Set Text -> ReleaseGroupProjectRevision -> UpdateReleaseProjectRequest
    constructUpdateProjectRequest locatorSet ReleaseGroupProjectRevision{..} = do
      let maybeReleaseGroupId = if Set.member releaseGroupProjectLocator locatorSet then Just (releaseGroupReleaseId targetRelease) else Nothing
      UpdateReleaseProjectRequest releaseGroupProjectLocator releaseGroupProjectRevision releaseGroupProjectBranch maybeReleaseGroupId
