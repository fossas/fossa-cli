{-# LANGUAGE RecordWildCards #-}

module App.Fossa.ReleaseGroup.Delete (
  deleteMain,
) where

import App.Fossa.Config.ReleaseGroup.Delete (DeleteConfig (..))
import App.Fossa.ReleaseGroup.Create (emitFossaVersionError)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.FossaApiClient (FossaApiClient, deleteReleaseGroup, getOrganization)
import Control.Effect.Lift (Lift)
import Effect.Logger (Logger, logInfo, logStdout)
import Fossa.API.Types (Organization (..))

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
  org <- getOrganization
  if orgSupportsReleaseGroups org
    then do
      deleteReleaseGroup releaseTitle
      logStdout $ "Release group release " <> "`" <> releaseTitle <> "`" <> " has been deleted"
    else emitFossaVersionError
