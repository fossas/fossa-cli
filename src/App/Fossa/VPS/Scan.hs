module App.Fossa.VPS.Scan (
  scanMain,
  FollowSymlinks (..),
  SkipIPRScan (..),
  LicenseOnlyScan (..),
) where

import Control.Carrier.Diagnostics
import Control.Effect.Lift (Lift)
import Effect.Exec
import System.Exit (exitFailure)

import App.Fossa.EmbeddedBinary
import App.Fossa.ProjectInference
import App.Fossa.VPS.Scan.RunWiggins
import App.Fossa.VPS.Types
import App.Types (BaseDir (..), OverrideProject (..), ProjectMetadata (..))
import Data.Flag (Flag, fromFlag)
import Data.Text
import Effect.Logger
import Fossa.API.Types (ApiOpts (..))

-- | FollowSymlinks bool flag
data FollowSymlinks = FollowSymlinks

-- | SkipIPRScan bool flag
data SkipIPRScan = SkipIPRScan

-- | LicenseOnlyScan bool flag
data LicenseOnlyScan = LicenseOnlyScan

scanMain :: BaseDir -> ApiOpts -> ProjectMetadata -> Severity -> OverrideProject -> FilterExpressions -> Flag FollowSymlinks -> Flag SkipIPRScan -> Flag LicenseOnlyScan -> IO ()
scanMain basedir apiOpts metadata logSeverity overrideProject fileFilters followSymlinks skipIprFlag licenseOnlyScan = do
  result <- runDiagnostics $ withWigginsBinary $ vpsScan basedir logSeverity overrideProject followSymlinks skipIprFlag licenseOnlyScan fileFilters apiOpts metadata
  case result of
    Left failure -> do
      print $ renderFailureBundle failure
      exitFailure
    Right _ -> pure ()

----- main logic

vpsScan ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  ) =>
  BaseDir ->
  Severity ->
  OverrideProject ->
  Flag FollowSymlinks ->
  Flag SkipIPRScan ->
  Flag LicenseOnlyScan ->
  FilterExpressions ->
  ApiOpts ->
  ProjectMetadata ->
  BinaryPaths ->
  m ()
vpsScan (BaseDir basedir) logSeverity overrideProject followSymlinks skipIprFlag licenseOnlyScan fileFilters apiOpts metadata binaryPaths = withDefaultLogger logSeverity $ do
  projectRevision <- mergeOverride overrideProject <$> (inferProjectFromVCS basedir <||> inferProjectDefault basedir)
  saveRevision projectRevision

  let scanType = ScanType (fromFlag FollowSymlinks followSymlinks) (fromFlag SkipIPRScan skipIprFlag) (fromFlag LicenseOnlyScan licenseOnlyScan)
  let wigginsOpts = generateWigginsScanOpts basedir logSeverity projectRevision scanType fileFilters apiOpts metadata

  logInfo "Running VPS plugin scan"
  stdout <- runExecIO $ runWiggins binaryPaths wigginsOpts
  logInfo $ pretty stdout

runWiggins :: (Has Exec sig m, Has Diagnostics sig m) => BinaryPaths -> WigginsOpts -> m Text
runWiggins binaryPaths opts = do
  execWiggins binaryPaths opts
