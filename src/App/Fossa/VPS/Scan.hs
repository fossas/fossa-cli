{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VPS.Scan
  ( scanMain,
    SkipIPRScan (..),
    LicenseOnlyScan (..)
  ) where

import Control.Effect.Lift (Lift)
import Control.Carrier.Diagnostics
import Effect.Exec
import System.Exit (exitFailure)

import App.Fossa.VPS.EmbeddedBinary
import App.Fossa.VPS.Scan.RunWiggins
import App.Fossa.VPS.Types
import App.Types (BaseDir (..), OverrideProject (..), ProjectMetadata (..))
import Data.Flag (Flag, fromFlag)
import Effect.Logger
import Fossa.API.Types (ApiOpts(..))
import App.Fossa.ProjectInference

-- | SkipIPRScan bool flag
data SkipIPRScan = SkipIPRScan

-- | LicenseOnlyScan bool flag
data LicenseOnlyScan = LicenseOnlyScan

scanMain :: BaseDir -> ApiOpts -> ProjectMetadata -> Severity -> OverrideProject -> FilterExpressions -> Flag SkipIPRScan -> Flag LicenseOnlyScan ->  IO ()
scanMain basedir apiOpts metadata logSeverity overrideProject fileFilters skipIprScan licenseOnlyScan = do
  result <- runDiagnostics $ withEmbeddedBinaries $ vpsScan basedir logSeverity overrideProject skipIprScan licenseOnlyScan fileFilters apiOpts metadata
  case result of
    Left failure -> do
      print $ renderFailureBundle failure
      exitFailure
    Right _ -> pure ()

----- main logic

vpsScan ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  ) => BaseDir -> Severity -> OverrideProject -> Flag SkipIPRScan -> Flag LicenseOnlyScan -> FilterExpressions -> ApiOpts -> ProjectMetadata -> BinaryPaths -> m ()
vpsScan (BaseDir basedir) logSeverity overrideProject skipIprFlag licenseOnlyScan fileFilters apiOpts metadata binaryPaths = withLogger logSeverity $ do
  projectRevision <- mergeOverride overrideProject <$> inferProject basedir
  let scanType = ScanType (fromFlag SkipIPRScan skipIprFlag) (fromFlag LicenseOnlyScan licenseOnlyScan)
  let wigginsOpts = generateWigginsOpts basedir logSeverity projectRevision scanType fileFilters apiOpts metadata

  logDebug "Running VPS plugin scan"
  runExecIO $ runWiggins binaryPaths wigginsOpts

runWiggins :: ( Has Exec sig m, Has Diagnostics sig m) => BinaryPaths -> WigginsOpts -> m ()
runWiggins binaryPaths opts = do
  execWiggins binaryPaths opts
