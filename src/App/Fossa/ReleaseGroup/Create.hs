{-# LANGUAGE RecordWildCards #-}

module App.Fossa.ReleaseGroup.Create (
  createMain,
  emitFossaVersionError,
) where

import App.Fossa.Config.ReleaseGroup.Create (CreateConfig (..))
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, errHelp, fatalText)
import Control.Effect.FossaApiClient (FossaApiClient, createReleaseGroup, getOrganization)
import Control.Effect.Lift (Lift)
import Data.String.Conversion (ToText (..))
import Data.Text (Text)
import Effect.Logger (Logger, logInfo, logStdout)
import Fossa.API.Types (CreateReleaseGroupResponse (..), Organization (orgSupportsReleaseGroups))

createMain ::
  ( Has Diagnostics sig m
  , Has Logger sig m
  , Has (Lift IO) sig m
  , Has FossaApiClient sig m
  ) =>
  CreateConfig ->
  m ()
createMain CreateConfig{..} = do
  logInfo "Running FOSSA release-group create"
  org <- getOrganization
  if orgSupportsReleaseGroups org
    then do
      res <- createReleaseGroup releaseGroupRevision
      logStdout $ "Created release group with id: " <> toText (releaseGroupId res)
    else emitFossaVersionError

emitFossaVersionError :: Has Diagnostics sig m => m ()
emitFossaVersionError = errHelp ("Upgrade your FOSSA version" :: Text) $ fatalText "The current version of FOSSA you are using does not support release groups"
