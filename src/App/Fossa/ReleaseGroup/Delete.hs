{-# LANGUAGE RecordWildCards #-}

module App.Fossa.ReleaseGroup.Delete (
  deleteMain,
) where

import App.Fossa.Config.ReleaseGroup.Delete (DeleteConfig (..))
import App.Fossa.ReleaseGroup.Common (retrieveReleaseGroupId)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Effect.FossaApiClient (FossaApiClient, deleteReleaseGroup, getReleaseGroups)
import Control.Effect.Lift (Lift)
import Data.String.Conversion (toText)
import Effect.Logger (Logger, logInfo, logStdout)

deleteMain ::
  ( Has Diagnostics sig m
  , Has Logger sig m
  , Has (Lift IO) sig m
  , Has FossaApiClient sig m
  ) =>
  DeleteConfig ->
  m ()
deleteMain DeleteConfig{..} = do
  logInfo "Running FOSSA release-group delete"

  releaseGroups <- getReleaseGroups
  maybeReleaseGroupId <- retrieveReleaseGroupId releaseGroupTitle releaseGroups
  case maybeReleaseGroupId of
    Nothing -> fatalText $ "Release group `" <> releaseGroupTitle <> "` not found"
    Just releaseGroupId -> do
      deleteReleaseGroup $ toText releaseGroupId
      logStdout $ "Release group " <> "`" <> releaseGroupTitle <> "`" <> " has been deleted"
