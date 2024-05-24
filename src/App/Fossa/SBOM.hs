module App.Fossa.SBOM (
  sbomSubCommand,
) where

import App.Docs (fossaSBOMScannerUrl)
import App.Fossa.Config.SBOM (
  SBOMAnalyzeConfig (..),
  SBOMCommand,
  SBOMScanConfig (..),
 )
import App.Fossa.Config.SBOM qualified as Config
import App.Fossa.SBOM.Analyze qualified as Analyze
import App.Fossa.SBOM.Test qualified as Test
import App.Fossa.Subcommand (SubCommand)
import App.Support (supportUrl)
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
 )
import Control.Effect.Lift (Lift)
import Control.Effect.Telemetry (Telemetry)
import Control.Monad (void)
import Effect.Exec (Exec)
import Effect.Logger (
  Logger,
  Pretty (pretty),
  indent,
  logWarn,
  vsep,
 )
import Effect.ReadFS (ReadFS)

sbomSubCommand :: SubCommand SBOMCommand SBOMScanConfig
sbomSubCommand = Config.mkSubCommand dispatch

dispatch ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Telemetry sig m
  ) =>
  SBOMScanConfig ->
  m ()
dispatch = \case
  AnalyzeCfg cfg -> Analyze.analyze cfg
  TestCfg cfg -> Test.test cfg
