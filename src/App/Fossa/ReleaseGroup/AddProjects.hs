{-# LANGUAGE RecordWildCards #-}

module App.Fossa.ReleaseGroup.AddProjects (
  addProjectsMain,
  constructUpdateRequest,
) where

import App.Fossa.Config.ReleaseGroup.AddProjects (AddProjectsConfig (..))
import App.Fossa.ReleaseGroup.Common (retrieveReleaseGroupId, retrieveReleaseGroupRelease)
import App.Types (ReleaseGroupProjectRevision (..), ReleaseGroupReleaseRevision (..))
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, context, fatalText)
import Control.Effect.FossaApiClient (FossaApiClient, getOrganization, getReleaseGroupReleases, getReleaseGroups, resolveReleaseGroupRelease, updateReleaseGroupRelease)
import Control.Effect.Lift (Lift)
import Data.String.Conversion (ToText (..))
import Data.Text (Text)
import Effect.Logger (Logger, logDebug, logInfo, logStdout, pretty)
import Fossa.API.CoreTypes (ReleaseGroupRelease (..), ReleaseGroupReleaseLookup (..), UpdateReleaseProjectRequest (..), UpdateReleaseRequest (..))
import Fossa.API.Types (Organization (..))
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

  org <- getOrganization
  (releaseGroupId, releaseId) <-
    if orgSupportsFasterReleaseGroupAddProjects org
      then do
        -- Core resolves both ids in a single request and filters in the database,
        -- avoiding an unpaginated fetch of every release group and release.
        lookupRes <-
          context "Resolving release group release" $
            resolveReleaseGroupRelease title (releaseTitle releaseGroupReleaseRevision)
        pure (lookupReleaseGroupId lookupRes, lookupReleaseId lookupRes)
      else do
        -- Fallback for Core instances without the lookup endpoint: list every
        -- release group and release and match by title client-side.
        releaseGroups <- getReleaseGroups
        maybeReleaseGroupId <- context "Retrieving release group ID" $ retrieveReleaseGroupId title releaseGroups
        case maybeReleaseGroupId of
          Nothing -> fatalText $ "Release group `" <> title <> "` not found"
          Just releaseGroupId -> do
            releases <- getReleaseGroupReleases releaseGroupId
            release <- context "Retrieving release group release" $ retrieveReleaseGroupRelease (releaseTitle releaseGroupReleaseRevision) releases
            pure (releaseGroupId, releaseGroupReleaseId release)

  let updateReleaseRequest = constructUpdateRequest releaseGroupReleaseRevision
  res <- updateReleaseGroupRelease releaseGroupId releaseId updateReleaseRequest
  logStdout $ "Projects were added to release group release id: " <> toText (releaseGroupReleaseId res) <> "\n"
  logStdout $ "View the updated release at: " <> updatedReleaseLink releaseGroupId (releaseGroupReleaseId res) <> "\n"
  logDebug $ "Projects added to release: " <> pretty (pShow $ releaseGroupReleaseProjects res)
  where
    updatedReleaseLink :: Int -> Int -> Text
    updatedReleaseLink releaseGroupId releaseId = "https://app.fossa.com/projects/group/" <> toText releaseGroupId <> "/releases/" <> toText releaseId

constructUpdateRequest :: ReleaseGroupReleaseRevision -> UpdateReleaseRequest
constructUpdateRequest releaseRevision = UpdateReleaseRequest (releaseTitle releaseRevision) projectsReq
  where
    projectsReq :: [UpdateReleaseProjectRequest]
    projectsReq = map constructUpdateProjectRequest $ releaseProjects releaseRevision

    -- Core determines whether each project is new or an existing project to update by
    -- comparing against the release's current projects server-side, so the CLI only
    -- sends the desired locator, revision, and branch for each project.
    constructUpdateProjectRequest :: ReleaseGroupProjectRevision -> UpdateReleaseProjectRequest
    constructUpdateProjectRequest ReleaseGroupProjectRevision{..} =
      UpdateReleaseProjectRequest releaseGroupProjectLocator releaseGroupProjectRevision releaseGroupProjectBranch
