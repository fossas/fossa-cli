module App.Fossa.SBOM (
  sbomSubCommand,
) where

import App.Fossa.Analyze.Debug (collectDebugBundle)
import App.Fossa.Config.SBOM (
  SBOMCommand,
  SBOMScanConfig (..),
 )
import App.Fossa.Config.SBOM qualified as Config
import App.Fossa.SBOM.Analyze qualified as Analyze
import App.Fossa.SBOM.Test qualified as Test
import App.Fossa.Subcommand (SubCommand)
import Codec.Compression.GZip qualified as GZip
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
 )
import Control.Effect.Lift (Lift, sendIO)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Effect.Exec (Exec)
import Effect.Logger (
  Logger,
  Severity (..),
 )
import Effect.ReadFS (ReadFS)

sbomSubCommand :: SubCommand SBOMCommand SBOMScanConfig
sbomSubCommand = Config.mkSubCommand dispatch

debugBundlePath :: FilePath
debugBundlePath = "fossa.debug.json.gz"

dispatch ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Exec sig m
  ) =>
  SBOMScanConfig ->
  m ()
dispatch = \case
  -- TODO: Add telemetry
  AnalyzeCfg cfg -> do
    case Config.severity cfg of
      SevDebug -> do
        (scope, res) <- collectDebugBundle cfg $ Diag.errorBoundaryIO $ Analyze.analyze cfg
        sendIO . BL.writeFile debugBundlePath . GZip.compress $ Aeson.encode scope
        Diag.rethrow res
      _ -> ignoreDebug $ Analyze.analyze cfg
  TestCfg cfg -> Test.test cfg
