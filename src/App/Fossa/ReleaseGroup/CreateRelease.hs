{-# LANGUAGE RecordWildCards #-}

module App.Fossa.ReleaseGroup.CreateRelease (
  createReleaseMain,
) where

import App.Fossa.Config.ReleaseGroup.CreateRelease (CreateReleaseConfig (..))
import App.Fossa.ReleaseGroup.Common (retrieveReleaseGroupId)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Effect.FossaApiClient (FossaApiClient, createReleaseGroupRelease, getReleaseGroups)
import Control.Effect.Lift (Lift)
import Data.String.Conversion (ToText (..))
import Data.Text (Text)
import Effect.Logger (Logger, logInfo, logStdout)
import Fossa.API.Types (ReleaseGroupRelease (..))

createReleaseMain ::
  ( Has Diagnostics sig m
  , Has Logger sig m
  , Has (Lift IO) sig m
  , Has FossaApiClient sig m
  ) =>
  CreateReleaseConfig ->
  m ()
createReleaseMain CreateReleaseConfig{..} = do
  logInfo "Running FOSSA release-group create-release"

  releaseGroups <- getReleaseGroups
  maybeReleaseGroupId <- retrieveReleaseGroupId releaseGroupTitle releaseGroups
  case maybeReleaseGroupId of
    Nothing -> fatalText $ "Release group `" <> releaseGroupTitle <> "` not found"
    Just releaseGroupId -> do
      res <- createReleaseGroupRelease releaseGroupId releaseGroupReleaseRevision
      let releaseId = releaseGroupReleaseId res
      logStdout $ "Created release group release with id: " <> toText releaseId <> "\n"
      logStdout $ "View the created release group release at: " <> createdReleaseGroupReleaseLink releaseGroupId releaseId <> "\n"
  where
    createdReleaseGroupReleaseLink :: Int -> Int -> Text
    createdReleaseGroupReleaseLink releaseGroupId releaseId = "https://app.fossa.com/projects/group/" <> toText releaseGroupId <> "/releases/" <> toText releaseId
