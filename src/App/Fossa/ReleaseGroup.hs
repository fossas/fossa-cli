module App.Fossa.ReleaseGroup (
  releaseGroupSubCommand,
) where

import App.Fossa.Config.ReleaseGroup (ReleaseGroupCommand, ReleaseGroupConfig (..), mkSubCommand)
import App.Fossa.ReleaseGroup.Create (createReleaseGroup)
import App.Fossa.Subcommand (SubCommand)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Lift)
import Effect.Exec (Exec)
import Effect.Logger (Logger, logInfo)

releaseGroupSubCommand :: SubCommand ReleaseGroupCommand ReleaseGroupConfig
releaseGroupSubCommand = mkSubCommand releaseGroupMain

releaseGroupMain ::
  ( Has (Lift IO) sig m
  , Has Exec sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  ReleaseGroupConfig ->
  m ()
releaseGroupMain subcommandConfig = do
  logInfo "Running FOSSA release-group"
  case subcommandConfig of
    Create config -> createReleaseGroup config
