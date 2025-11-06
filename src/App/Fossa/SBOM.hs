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
import App.Fossa.Subcommand (SubCommand)
import App.Fossa.Test (testMain)
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
 )
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Telemetry (Telemetry)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.IORef (readIORef)
import Effect.Exec (Exec)
import Effect.Logger (Logger)
import Effect.ReadFS (ReadFS)
import System.FilePath ((</>))

sbomSubCommand :: SubCommand SBOMCommand SBOMScanConfig
sbomSubCommand = Config.mkSubCommand dispatch

debugBundlePath :: FilePath
debugBundlePath = "fossa.debug.json"

dispatch ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has Telemetry sig m
  ) =>
  SBOMScanConfig ->
  m ()
dispatch = \case
  AnalyzeCfg cfg -> do
    -- Read debug directory from config
    let maybeDebugDir = Config.debugDir cfg

    case maybeDebugDir of
      Just debugDir -> do
        (bundle, res) <- collectDebugBundle cfg $ Diag.errorBoundaryIO $ Analyze.analyze cfg

        -- Write debug JSON to debug directory (uncompressed)
        sendIO $ do
          let debugJsonPath = debugDir </> debugBundlePath
          BL.writeFile debugJsonPath $ Aeson.encode bundle

        Diag.rethrow res
      Nothing -> ignoreDebug $ Analyze.analyze cfg
  TestCfg cfg -> testMain cfg
