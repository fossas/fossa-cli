module App.Fossa.Container.AnalyzeNative (
  analyzeExperimental,
) where

import App.Fossa.Analyze.Debug (collectDebugBundle)
import App.Fossa.Config.Common (
  ScanDestination (OutputStdout, UploadScan),
 )
import App.Fossa.Config.Container.Analyze (
  ContainerAnalyzeConfig (imageLocator, scanDestination),
 )
import App.Fossa.Config.Container.Analyze qualified as Config
import App.Fossa.Config.Container.Common (ImageText (unImageText))
import App.Fossa.Container.Sources.DockerTarball (analyzeExportedTarball)
import Codec.Compression.GZip qualified as GZip
import Control.Carrier.Debug (Debug, ignoreDebug)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Effect.Diagnostics (Diagnostics, fatalText, fromEitherShow)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Telemetry (Telemetry)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.String.Conversion (ConvertUtf8 (decodeUtf8), toString)
import Data.Text (Text)
import Effect.Exec (Exec)
import Effect.Logger (
  Has,
  Logger,
  Severity (..),
  logInfo,
  logStdout,
 )
import Effect.ReadFS (ReadFS, getCurrentDir)
import Path (Abs, File, Path, SomeBase (Abs, Rel), parseSomeFile, (</>))

data ContainerImageSource
  = ContainerExportedTarball (Path Abs File)
  | ContainerDockerImage Text
  | ContainerOCIRegistry Text Text
  deriving (Show, Eq)

debugBundlePath :: FilePath
debugBundlePath = "fossa.container.debug.json.gz"

analyzeExperimental ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has Telemetry sig m
  ) =>
  ContainerAnalyzeConfig ->
  m ()
analyzeExperimental cfg =
  case Config.severity cfg of
    SevDebug -> do
      (scope, res) <- collectDebugBundle cfg $ Diag.errorBoundaryIO $ analyze cfg
      sendIO . BL.writeFile debugBundlePath . GZip.compress $ Aeson.encode scope
      Diag.rethrow res
    _ -> ignoreDebug $ analyze cfg

analyze ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has Telemetry sig m
  , Has Debug sig m
  ) =>
  ContainerAnalyzeConfig ->
  m ()
analyze cfg = do
  logInfo "Running container scanning with fossa experimental-scanner!"
  parsedSource <- parseContainerImageSource (unImageText $ imageLocator cfg)
  scannedImage <- case parsedSource of
    ContainerDockerImage _ -> fatalText "container images from daemon are not yet supported!"
    ContainerOCIRegistry _ _ -> fatalText "container images from oci registry are not yet supported!"
    ContainerExportedTarball tarball -> analyzeExportedTarball tarball

  case scanDestination cfg of
    OutputStdout -> logStdout . decodeUtf8 $ Aeson.encode scannedImage
    UploadScan _ _ -> fatalText "experimental scanner does not allow to submit projects"

parseContainerImageSource ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  ) =>
  Text ->
  m (ContainerImageSource)
parseContainerImageSource = parseExportedTarballSource

parseExportedTarballSource ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  ) =>
  Text ->
  m (ContainerImageSource)
parseExportedTarballSource path = do
  cwd <- getCurrentDir
  someTarballFile <- fromEitherShow $ parseSomeFile (toString path)
  resolvedAbsPath <- case someTarballFile of
    Abs absPath -> pure absPath
    Rel relPath -> pure $ cwd </> relPath
  pure $ ContainerExportedTarball resolvedAbsPath
