module App.Fossa.ReleaseGroup (
  releaseGroupSubCommand,
) where

import App.Fossa.Config.ReleaseGroup (ReleaseGroupCommand, ReleaseGroupConfig (..), mkSubCommand)
import App.Fossa.Config.ReleaseGroup.AddProjects qualified as AddProjects
import App.Fossa.Config.ReleaseGroup.Create as Create
import App.Fossa.Config.ReleaseGroup.CreateRelease as CreateRelease
import App.Fossa.Config.ReleaseGroup.Delete qualified as Delete
import App.Fossa.Config.ReleaseGroup.DeleteRelease qualified as DeleteRelease
import App.Fossa.ReleaseGroup.AddProjects (addProjectsMain)
import App.Fossa.ReleaseGroup.Create (createMain)
import App.Fossa.ReleaseGroup.CreateRelease (createReleaseMain)
import App.Fossa.ReleaseGroup.Delete (deleteMain)
import App.Fossa.ReleaseGroup.DeleteRelease (deleteReleaseMain)
import App.Fossa.Subcommand (SubCommand)
import Control.Algebra (Has)
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.Diagnostics (context)
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Lift)
import Effect.Logger (Logger, logInfo)

releaseGroupSubCommand :: SubCommand ReleaseGroupCommand ReleaseGroupConfig
releaseGroupSubCommand = mkSubCommand releaseGroupMain

releaseGroupMain ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  ReleaseGroupConfig ->
  m ()
releaseGroupMain subcommandConfig = do
  logInfo "Running FOSSA release-group"
  case subcommandConfig of
    AddProjectsCfg config -> context "Add projects to release group" $ ignoreDebug $ runFossaApiClient (AddProjects.apiOpts config) $ addProjectsMain config
    CreateCfg config -> context "Create release group" $ ignoreDebug $ runFossaApiClient (Create.apiOpts config) $ createMain config
    CreateReleaseCfg config -> context "Create release group release" $ ignoreDebug $ runFossaApiClient (CreateRelease.apiOpts config) $ createReleaseMain config
    DeleteCfg config -> context "Delete release group" $ ignoreDebug $ runFossaApiClient (Delete.apiOpts config) $ deleteMain config
    DeleteReleaseCfg config -> context "Delete release group release" $ ignoreDebug $ runFossaApiClient (DeleteRelease.apiOpts config) $ deleteReleaseMain config
