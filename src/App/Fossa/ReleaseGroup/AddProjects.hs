{-# LANGUAGE RecordWildCards #-}

module App.Fossa.ReleaseGroup.AddProjects (
  addProjectsMain,
) where

import App.Fossa.Config.ReleaseGroup.AddProjects (AddProjectsConfig (..))
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.FossaApiClient (FossaApiClient, addReleaseGroupProjects, getOrganization)
import Control.Effect.Lift (Lift)
import Control.Monad (when)
import Data.String.Conversion (ToText (..))
import Effect.Logger (Logger, logInfo, logStdout)
import Fossa.API.Types (AddReleaseGroupProjectsResponse (..), Organization (..))

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
  when (orgSupportsReleaseGroups org) $ do
    res <- addReleaseGroupProjects title releaseGroupReleaseRevision
    logStdout $ "Projects were added to release group release id: " <> toText (releaseGroupReleaseId res)
