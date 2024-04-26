{-# LANGUAGE RecordWildCards #-}

module App.Fossa.ReleaseGroup.DeleteRelease (
  deleteReleaseMain,
) where

import App.Fossa.Config.ReleaseGroup.DeleteRelease (DeleteReleaseConfig (..))
import App.Fossa.ReleaseGroup.Common (retrieveReleaseGroupId, retrieveReleaseGroupRelease)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Effect.FossaApiClient (FossaApiClient, deleteReleaseGroupRelease, getReleaseGroupReleases, getReleaseGroups)
import Control.Effect.Lift (Lift)
import Control.Monad (when)
import Effect.Logger (Logger, logInfo, logStdout)
import Fossa.API.CoreTypes (ReleaseGroupRelease (..))

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

  releaseGroups <- getReleaseGroups
  maybeReleaseGroupId <- retrieveReleaseGroupId releaseGroupTitle releaseGroups
  case maybeReleaseGroupId of
    Nothing -> fatalText $ "Release group `" <> releaseGroupTitle <> "` not found"
    Just releaseGroupId -> do
      releases <- getReleaseGroupReleases releaseGroupId
      when (length releases <= 1) $ fatalText "You are not permitted to delete a release when there is only one release in your release group"

      release <- retrieveReleaseGroupRelease releaseGroupReleaseTitle releases
      let releaseId = releaseGroupReleaseId release
      deleteReleaseGroupRelease releaseGroupId releaseId

      logStdout $ "Release " <> "`" <> releaseGroupReleaseTitle <> "`" <> " has been deleted" <> "\n"
