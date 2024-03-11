{-# LANGUAGE RecordWildCards #-}

module App.Fossa.ReleaseGroup.Create (
  createMain,
) where

import App.Fossa.Config.ReleaseGroup.Create (CreateConfig (..))
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.FossaApiClient (FossaApiClient, createReleaseGroup, getOrganization)
import Control.Effect.Lift (Lift)
import Control.Monad (when)
import Data.String.Conversion (ToText (..))
import Effect.Logger (Logger, Pretty (pretty), logDebug, logInfo, logStdout)
import Fossa.API.Types (CreateReleaseGroupResponse (..), Organization (orgSupportsReleaseGroups))
import Text.Pretty.Simple (pShow)

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
  logDebug $ "The org ---- " <> pretty (pShow (org))
  when (orgSupportsReleaseGroups org) $ do
    res <- createReleaseGroup releaseGroupRevision
    logStdout $ "Created release group with id: " <> toText (releaseGroupId res)
