module App.Fossa.Container (
  containerSubCommand,
) where

import App.Fossa.Config.Container (
  ContainerAnalyzeConfig (usesExperimentalScanner),
  ContainerCommand,
  ContainerScanConfig (..),
 )
import App.Fossa.Config.Container qualified as Config
import App.Fossa.Container.AnalyzeNative qualified as AnalyzeNative
import App.Fossa.Container.ListTargets (listTargets)
import App.Fossa.Container.Test qualified as Test
import App.Fossa.DebugDir (DebugDirRef)
import App.Fossa.Subcommand (SubCommand)
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
 )
import Control.Effect.Lift (Lift)
import Control.Effect.Reader (Reader)
import Control.Effect.Telemetry (Telemetry)
import Control.Monad (void, when)
import Effect.Exec (Exec)
import Effect.Logger (
  Logger,
  logWarn,
  vsep,
 )
import Effect.ReadFS (ReadFS)

containerSubCommand :: SubCommand ContainerCommand ContainerScanConfig
containerSubCommand = Config.mkSubCommand dispatch

dispatch ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Telemetry sig m
  ) =>
  ContainerScanConfig ->
  m ()
dispatch = \case
  AnalyzeCfg cfg -> do
    when (usesExperimentalScanner cfg) $
      logWarn $
        vsep
          [ "DEPRECATION NOTICE"
          , ""
          , "The 'experimental' container scanner is now the only available scanner, and is enabled automatically."
          , ""
          , "The --experimental-scanner flag is now deprecated, and has no effect."
          , "In the future, using this flag will cause a fatal error."
          , "To avoid these errors, remove the flag from your fossa commands."
          , ""
          ]

    void $ AnalyzeNative.analyzeExperimental cfg
  TestCfg cfg -> Test.test cfg
  ListTargetsCfg cfg -> listTargets cfg
