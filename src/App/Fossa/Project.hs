module App.Fossa.Project (
  projectSubCommand,
) where

import App.Fossa.Config.Project (ProjectCommand (..), ProjectConfig (..), mkSubCommand)
import App.Fossa.Config.Project.Edit qualified as Edit
import App.Fossa.Project.Edit (editMain)
import App.Fossa.Subcommand (SubCommand)
import Control.Algebra (Has)
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.Diagnostics (context)
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Carrier.StickyLogger (runStickyLogger)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Lift)
import Effect.Logger (Logger, Severity (SevInfo), logInfo)

projectSubCommand :: SubCommand ProjectCommand ProjectConfig
projectSubCommand = mkSubCommand projectMain

projectMain ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  ProjectConfig ->
  m ()
projectMain (EditCfg config) = do
  logInfo "Running FOSSA project"
  context "Add projects to release group" . runStickyLogger SevInfo . ignoreDebug . runFossaApiClient (Edit.apiOpts config) $ editMain config
