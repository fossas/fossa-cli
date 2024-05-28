module App.Fossa.SBOM (
  sbomSubCommand,
) where

import App.Fossa.Config.SBOM (
  SBOMCommand,
  SBOMScanConfig (..),
 )
import App.Fossa.Config.SBOM qualified as Config
import App.Fossa.SBOM.Analyze qualified as Analyze
import App.Fossa.SBOM.Test qualified as Test
import App.Fossa.Subcommand (SubCommand)
import Control.Carrier.Debug (ignoreDebug)
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
 )
import Control.Effect.Lift (Lift)
import Control.Monad (void)
import Effect.Logger (
  Logger,
 )

sbomSubCommand :: SubCommand SBOMCommand SBOMScanConfig
sbomSubCommand = Config.mkSubCommand dispatch

dispatch ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  SBOMScanConfig ->
  m ()
dispatch = \case
  -- TODO: deal with logging properly
  -- TODO: Add telemetry
  AnalyzeCfg cfg -> void $ ignoreDebug $ Analyze.analyze cfg
  TestCfg cfg -> Test.test cfg
