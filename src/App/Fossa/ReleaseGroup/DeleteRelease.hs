{-# LANGUAGE RecordWildCards #-}

module App.Fossa.ReleaseGroup.DeleteRelease (
  deleteReleaseMain,
) where

import App.Fossa.Config.ReleaseGroup.DeleteRelease (DeleteReleaseConfig (..))
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.FossaApiClient (FossaApiClient, deleteReleaseGroupRelease, getOrganization)
import Control.Effect.Lift (Lift)
import Control.Monad (when)
import Effect.Logger (Logger, logInfo, logStdout)
import Fossa.API.Types (Organization (..))

deleteReleaseMain ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has FossaApiClient sig m
  ) =>
  DeleteReleaseConfig ->
  m ()
deleteReleaseMain DeleteReleaseConfig{..} = do
  logInfo "Running FOSSA release-group delete-release"
  org <- getOrganization
  when (orgSupportsReleaseGroups org) $ do
    deleteReleaseGroupRelease releaseGroupTitle releaseGroupRelease
    logStdout $ "Release group release " <> "`" <> releaseGroupRelease <> "`" <> " has been deleted"
