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
import App.Fossa.Subcommand (SubCommand)
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
 )
import Control.Effect.Lift (Lift)
import Control.Effect.Telemetry (Telemetry)
import Control.Monad (when)
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
          [ "Experimental scanner has been PROMOTED as default scanner. By default, FOSSA CLI uses"
          , "this native container scanner (previously known as experimental scanner) as default."
          , ""
          , "In future, using --experimental-scanner may yield fatal error. Please stop"
          , "using --experimental-scanner flag for container scanning."
          , ""
          ]
    AnalyzeNative.analyzeExperimental cfg
  TestCfg cfg -> Test.test cfg
  ListTargetsCfg cfg -> listTargets cfg
