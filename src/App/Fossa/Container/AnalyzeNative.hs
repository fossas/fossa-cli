{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Container.AnalyzeNative (
  analyzeExperimental,

  -- * for testing
  uploadScan,
) where

import App.Fossa.API.BuildLink (getFossaBuildUrl)
import App.Fossa.Analyze.Debug (collectDebugBundle)
import App.Fossa.Config.Common (
  ScanDestination (OutputStdout, UploadScan),
 )
import App.Fossa.Config.Container.Analyze (
  ContainerAnalyzeConfig (imageLocator, revisionOverride, scanDestination),
 )
import App.Fossa.Config.Container.Analyze qualified as Config
import App.Fossa.Config.Container.Common (ImageText (unImageText))
import App.Fossa.Container.Sources.DockerTarball (analyzeExportedTarball)
import App.Types (
  OverrideProject (OverrideProject),
  ProjectMetadata,
  ProjectRevision (ProjectRevision, projectBranch, projectName, projectRevision),
  overrideBranch,
  overrideName,
  overrideRevision,
 )
import Codec.Compression.GZip qualified as GZip
import Container.Errors (EndpointDoesNotSupportNativeContainerScan (EndpointDoesNotSupportNativeContainerScan))
import Container.Types (ContainerScan (..))
import Control.Carrier.Debug (Debug, ignoreDebug)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Effect.Diagnostics (Diagnostics, fatal, fatalText, fromEitherShow)
import Control.Effect.FossaApiClient (FossaApiClient, getOrganization, uploadNativeContainerScan)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Telemetry (Telemetry)
import Control.Monad (void)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.String.Conversion (decodeUtf8, toString)
import Data.Text (Text)
import Effect.Exec (Exec)
import Effect.Logger (
  Has,
  Logger,
  Pretty (pretty),
  Severity (..),
  logError,
  logInfo,
  logStdout,
  viaShow,
 )
import Effect.ReadFS (ReadFS, getCurrentDir)
import Fossa.API.Types (Organization (orgSupportsNativeContainerScan), UploadResponse (uploadError), uploadLocator)
import Path (Abs, File, Path, SomeBase (Abs, Rel), parseSomeFile, (</>))
import Srclib.Types (Locator)

data ContainerImageSource
  = ContainerExportedTarball (Path Abs File)
  | ContainerDockerImage Text
  | ContainerOCIRegistry Text Text
  deriving (Show, Eq)

debugBundlePath :: FilePath
debugBundlePath = "fossa.debug.json.gz"

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

  let revision = extractRevision (revisionOverride cfg) scannedImage
  case scanDestination cfg of
    OutputStdout -> logStdout . decodeUtf8 $ Aeson.encode scannedImage
    UploadScan apiOpts projectMeta ->
      void $ runFossaApiClient apiOpts $ uploadScan revision projectMeta scannedImage
  where
    extractRevision :: OverrideProject -> ContainerScan -> ProjectRevision
    extractRevision OverrideProject{..} ContainerScan{..} =
      ProjectRevision
        (fromMaybe imageTag overrideName)
        (fromMaybe imageDigest overrideRevision)
        overrideBranch

uploadScan ::
  ( Has Diagnostics sig m
  , Has FossaApiClient sig m
  , Has Logger sig m
  ) =>
  ProjectRevision ->
  ProjectMetadata ->
  ContainerScan ->
  m Locator
uploadScan revision projectMeta containerScan =
  do
    supportsNativeScan <- orgSupportsNativeContainerScan <$> getOrganization
    if not supportsNativeScan
      then fatal EndpointDoesNotSupportNativeContainerScan
      else do
        logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
        logInfo ("Using project revision: `" <> pretty (projectRevision revision) <> "`")
        let branchText = fromMaybe "No branch (detached HEAD)" $ projectBranch revision
        logInfo ("Using branch: `" <> pretty branchText <> "`")

        resp <- uploadNativeContainerScan revision projectMeta containerScan
        let locator = uploadLocator resp
        buildUrl <- getFossaBuildUrl revision locator

        logInfo "View FOSSA Report:"
        logInfo ("  " <> pretty buildUrl)
        traverse_ (\err -> logError $ "FOSSA error: " <> viaShow err) (uploadError resp)

        -- We return locator for purely
        -- purpose of testing.
        pure locator

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
