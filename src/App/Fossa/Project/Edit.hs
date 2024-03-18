{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Project.Edit (
  editMain,
) where

import App.Fossa.Config.Project.Edit (EditConfig (..))
import Control.Algebra (Has)
import Control.Carrier.Diagnostics (context)
import Control.Effect.Diagnostics (Diagnostics, errHelp, fatalText, warn)
import Control.Effect.FossaApiClient (FossaApiClient, editProject, getOrganization)
import Control.Effect.Lift (Lift)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Effect.Logger (Logger, logInfo, logStdout)
import Fossa.API.Types (Organization (..), ProjectResponse (..))

editMain ::
  ( Has Diagnostics sig m
  , Has Logger sig m
  , Has (Lift IO) sig m
  , Has FossaApiClient sig m
  ) =>
  EditConfig ->
  m ()
editMain EditConfig{..} = do
  logInfo "Running FOSSA project edit"
  org <- getOrganization
  if (orgSupportsProjects org)
    then do
      res <- editProject projectLocator projectMetadataRevision
      emitProjectWarnings res
      logStdout $ "Project " <> "`" <> projectLocator <> "` has been updated."
    else errHelp ("Upgrade your FOSSA version" :: Text) $ fatalText "The current version of FOSSA you are using does not support release groups"

emitProjectWarnings :: Has Diagnostics sig m => ProjectResponse -> m ()
emitProjectWarnings res = do
  context "Emit project warnings" $ traverse_ (traverse_ warn) $ projectWarnings res
