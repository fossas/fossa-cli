{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Container.AnalyzeNative (
  analyzeExperimental,

  -- * for testing
  uploadScan,
  ContainerImageSource (..),
  parseDockerEngineSource,
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
import App.Fossa.Container.Sources.DockerEngine (analyzeFromDockerEngine)
import App.Fossa.Container.Sources.DockerTarball (analyzeExportedTarball)
import App.Fossa.Container.Sources.Registry (analyzeFromRegistry)
import App.Types (
  OverrideProject (OverrideProject),
  ProjectMetadata,
  ProjectRevision (ProjectRevision, projectBranch, projectName, projectRevision),
  overrideBranch,
  overrideName,
  overrideRevision,
 )
import Codec.Compression.GZip qualified as GZip
import Container.Docker.SourceParser (RegistryImageSource, parseImageUrl)
import Container.Errors (EndpointDoesNotSupportNativeContainerScan (EndpointDoesNotSupportNativeContainerScan))
import Container.Types (ContainerScan (..))
import Control.Carrier.Debug (Debug, ignoreDebug)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.DockerEngineApi (runDockerEngineApi)
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Effect.Diagnostics (Diagnostics, ToDiagnostic, context, errCtx, fatal, fatalText, fromEitherShow, (<||>))
import Control.Effect.DockerEngineApi (DockerEngineApi, getDockerImageSize, isDockerEngineAccessible)
import Control.Effect.FossaApiClient (FossaApiClient, getOrganization, uploadNativeContainerScan)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Telemetry (Telemetry)
import Control.Monad (unless, void)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.String.Conversion (
  ConvertUtf8 (decodeUtf8),
  decodeUtf8,
  toString,
  toText,
 )
import Data.Text (Text)
import Data.Text qualified as Text
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
  vsep,
 )
import Effect.ReadFS (ReadFS, doesFileExist, getCurrentDir)
import Fossa.API.Types (Organization (orgSupportsNativeContainerScan), UploadResponse (uploadError), uploadLocator)
import Path (Abs, File, Path, SomeBase (Abs, Rel), parseSomeFile, (</>))
import Srclib.Types (Locator)
import System.Info (arch)
import Text.Megaparsec (errorBundlePretty, parse)

data ContainerImageSource
  = ContainerExportedTarball (Path Abs File)
  | ContainerDockerImage Text
  | ContainerOCIRegistry RegistryImageSource
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
  parsedSource <- runDockerEngineApi $ parseContainerImageSource (unImageText $ imageLocator cfg)
  scannedImage <- case parsedSource of
    ContainerDockerImage imgTag -> context "Analyzing via Docker Engine Api" $ analyzeFromDockerEngine imgTag
    ContainerOCIRegistry registrySrc -> context "Analyzing via Registry" $ analyzeFromRegistry registrySrc
    ContainerExportedTarball tarball -> context "Analyzing via Tarball" $ analyzeExportedTarball tarball

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
  , Has Logger sig m
  , Has DockerEngineApi sig m
  ) =>
  Text ->
  m ContainerImageSource
parseContainerImageSource src =
  parseExportedTarballSource src
    -- <||> parseDockerEngineSource src
    <||> parseOciRegistrySource src

parseDockerEngineSource ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  , Has DockerEngineApi sig m
  ) =>
  Text ->
  m ContainerImageSource
parseDockerEngineSource imgTag = do
  canAccessDockerEngine <- isDockerEngineAccessible

  unless canAccessDockerEngine $
    fatalText "Docker Engine (via sdk api) could not be accessed."

  unless (Text.isInfixOf ":" imgTag) $
    fatalText $
      "Docker Engine (via sdk api) requires image tag, in form of repo:tag, you provided: " <> imgTag

  -- Confirm image is available by checking image size
  -- It will throw diag error, if image is missing
  imgSize <- toText . show <$> errCtx (DockerEngineImageNotPresentLocally imgTag) (getDockerImageSize imgTag)
  logInfo . pretty $ "Discovered image for: " <> imgTag <> " (of " <> imgSize <> " bytes) via docker engine api."

  pure $ ContainerDockerImage imgTag

parseExportedTarballSource ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  ) =>
  Text ->
  m ContainerImageSource
parseExportedTarballSource path = do
  cwd <- getCurrentDir
  someTarballFile <- fromEitherShow $ parseSomeFile (toString path)
  resolvedAbsPath <- case someTarballFile of
    Abs absPath -> pure absPath
    Rel relPath -> pure $ cwd </> relPath
  doesFileExist' <- doesFileExist resolvedAbsPath
  unless doesFileExist' $
    fatalText $
      "Could not locate tarball source at filepath: " <> toText resolvedAbsPath
  pure $ ContainerExportedTarball resolvedAbsPath

newtype DockerEngineImageNotPresentLocally = DockerEngineImageNotPresentLocally Text
instance ToDiagnostic DockerEngineImageNotPresentLocally where
  renderDiagnostic (DockerEngineImageNotPresentLocally tag) =
    vsep
      [ pretty $ "Could not find: " <> (toString tag) <> " in local repository."
      , pretty $ "Perform: docker pull " <> (toString tag) <> ", prior to running fossa."
      ]

parseOciRegistrySource ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  ) =>
  Text ->
  m ContainerImageSource
parseOciRegistrySource tag = case parse (parseImageUrl defaultArch) "" tag of
  Left err -> fatal $ errorBundlePretty err
  Right registrySource -> pure $ ContainerOCIRegistry registrySource
  where
    -- Get current runtime arch, We use this to find suitable image,
    -- if multi-platform image is discovered. This is similar to
    -- how docker pull, and existing behavior works
    --
    -- Ref: https://docs.docker.com/desktop/multi-arch/
    defaultArch :: Text
    defaultArch =
      toText $
        if arch == "x86_64" -- x86_64 is equivalent to amd64
          then "amd64"
          else arch
