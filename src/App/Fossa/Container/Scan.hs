{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Container.Scan (
  extractTag,
  extractRevision,
  scanImage,
  scanImageNoAnalysis,
  parseContainerImageSource,
  parseDockerEngineSource,
  ContainerImageSource (..),
) where

import App.Fossa.Config.Analyze (WithoutDefaultFilters)
import App.Fossa.Config.Container.Common (ImageText (unImageText))
import App.Fossa.Container.Sources.DockerArchive (analyzeFromDockerArchive, revisionFromDockerArchive)
import App.Fossa.Container.Sources.DockerEngine (analyzeFromDockerEngine, revisionFromDockerEngine)
import App.Fossa.Container.Sources.Podman (analyzeFromPodman, podmanInspectImage, revisionFromPodman)
import App.Fossa.Container.Sources.Registry (analyzeFromRegistry, revisionFromRegistry, runWithCirceReexport)
import App.Types (OverrideProject (..), ProjectRevision (ProjectRevision))
import Container.Docker.SourceParser (RegistryImageSource (..), parseImageUrl, showReferenceWithSep)
import Container.Types (ContainerScan (..))
import Control.Carrier.Diagnostics (recover)
import Control.Carrier.DockerEngineApi (runDockerEngineApi)
import Control.Effect.Debug (Debug)
import Control.Effect.Diagnostics (Diagnostics, ToDiagnostic, context, errCtx, fatal, fatalText, fromEitherShow, fromMaybeText, (<||>))
import Control.Effect.DockerEngineApi (DockerEngineApi, getDockerImageSize, isDockerEngineAccessible)
import Control.Effect.Lift (Has, Lift)
import Control.Effect.Path (withSystemTempDir)
import Control.Effect.Telemetry (Telemetry)
import Control.Monad (join, unless)
import Data.Flag (Flag)
import Data.Functor (($>))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.String.Conversion (
  toString,
  toText,
 )
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Extra (breakOnEndAndRemove)
import Diag.Diagnostic qualified as Diag (
  ToDiagnostic (renderDiagnostic),
 )
import Discovery.Filters (AllFilters (..))
import Effect.Exec (Exec, execThrow')
import Effect.Logger (
  Logger,
  Pretty (pretty),
  logInfo,
 )
import Effect.ReadFS (ReadFS, doesFileExist, getCurrentDir)
import Errata (Errata (..))
import Path (Abs, File, Path, SomeBase (Abs, Rel), parseSomeFile, (</>))
import Text.Megaparsec (errorBundlePretty, parse)

data ContainerImageSource
  = DockerArchive (Path Abs File)
  | DockerEngine Text
  | Podman Text
  | Registry RegistryImageSource
  deriving (Show, Eq)

extractTag :: Has Diagnostics sig m => [Text] -> m Text
extractTag tags = do
  firstTag <- fromMaybeText "No image tags found" $ listToMaybe tags
  tagTuple <- fromMaybeText "Image was not in the format name:tag" $ breakOnEndAndRemove ":" firstTag
  pure $ fst tagTuple

extractRevision :: OverrideProject -> ContainerScan -> ProjectRevision
extractRevision OverrideProject{..} ContainerScan{..} =
  ProjectRevision
    (fromMaybe imageTag overrideName)
    (fromMaybe imageDigest overrideRevision)
    overrideBranch

scanImage ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has Telemetry sig m
  , Has Debug sig m
  ) =>
  AllFilters ->
  Flag WithoutDefaultFilters ->
  Bool ->
  ImageText ->
  Text ->
  Text ->
  m ContainerScan
scanImage filters withoutDefaultFilters systemDepsOnly imgText dockerHost imageArch = do
  parsedSource <- runDockerEngineApi dockerHost $ parseContainerImageSource (unImageText imgText) imageArch
  circePoweredScan <- withSystemTempDir "fossa-container-export-tmp" $ \dir -> do
    tarball <- runWithCirceReexport imgText dir
    join <$> traverse (recover . analyzeTarball) tarball
  maybe (legacyScan parsedSource) (pure . correctCirceSource parsedSource) circePoweredScan
  where
    analyzeTarball = context "Analyzing docker archive" . analyzeFromDockerArchive systemDepsOnly filters withoutDefaultFilters
    legacyScan src = do
      case src of
        DockerArchive tarball -> context "Analyzing tarball" $ analyzeTarball tarball
        DockerEngine imgTag ->
          context "Analyzing via Docker engine API" $
            analyzeFromDockerEngine systemDepsOnly filters withoutDefaultFilters dockerHost imgTag
        Podman img ->
          context "Analyzing via podman" $
            analyzeFromPodman systemDepsOnly filters withoutDefaultFilters img
        Registry registrySrc ->
          context "Analyzing via registry" $
            analyzeFromRegistry systemDepsOnly filters withoutDefaultFilters registrySrc

scanImageNoAnalysis ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Exec sig m
  ) =>
  ImageText ->
  Text ->
  Text ->
  m (Text, Text)
scanImageNoAnalysis imgText dockerHost imageArch = do
  parsedSource <- runDockerEngineApi dockerHost $ parseContainerImageSource (unImageText imgText) imageArch
  circePoweredScan <- withSystemTempDir "fossa-container-export-tmp" $ \dir -> do
    tarball <- runWithCirceReexport imgText dir
    join <$> traverse (recover . analyzeTarball) tarball
  maybe (legacyScan parsedSource) (pure . correctTag parsedSource) circePoweredScan
  where
    correctTag src (tag, digest) = (correctCirceTag src tag, digest)
    analyzeTarball = context "Analyzing docker archive" . revisionFromDockerArchive
    legacyScan src = do
      case src of
        DockerArchive tarball -> context "Retrieving revision information from tarball" $ analyzeTarball tarball
        DockerEngine imgTag ->
          context "Retrieving revision information via Docker engine API" $
            revisionFromDockerEngine dockerHost imgTag
        Podman img ->
          context "Retrieving revision information via podman" $
            revisionFromPodman img
        Registry registrySrc ->
          context "Retrieving revision information via registry" $
            revisionFromRegistry registrySrc

correctCirceSource :: ContainerImageSource -> ContainerScan -> ContainerScan
correctCirceSource src scan =
  ContainerScan
    { imageData = (imageData scan)
    , imageDigest = (imageDigest scan)
    , imageTag = correctCirceTag src $ imageTag scan
    }

correctCirceTag :: ContainerImageSource -> Text -> Text
correctCirceTag src tag = case src of
  Registry RegistryImageSource{..} ->
    registryHost
      <> "/"
      <> registryContainerRepository
      <> showReferenceWithSep registryContainerRepositoryReference
  _ -> tag

parseContainerImageSource ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  , Has Logger sig m
  , Has Exec sig m
  , Has DockerEngineApi sig m
  ) =>
  Text ->
  Text ->
  m ContainerImageSource
parseContainerImageSource src defaultArch =
  parseDockerArchiveSource src
    <||> parseDockerEngineSource src
    <||> parsePodmanSource src
    <||> parseRegistrySource defaultArch src

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

  pure $ DockerEngine imgTag

parseDockerArchiveSource ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  ) =>
  Text ->
  m ContainerImageSource
parseDockerArchiveSource path = do
  cwd <- getCurrentDir
  someTarballFile <- fromEitherShow $ parseSomeFile (toString path)
  resolvedAbsPath <- case someTarballFile of
    Abs absPath -> pure absPath
    Rel relPath -> pure $ cwd </> relPath
  doesFileExist' <- doesFileExist resolvedAbsPath
  unless doesFileExist' $
    fatalText $
      "Could not locate tarball source at filepath: " <> toText resolvedAbsPath
  pure $ DockerArchive resolvedAbsPath

newtype DockerEngineImageNotPresentLocally = DockerEngineImageNotPresentLocally Text

instance ToDiagnostic DockerEngineImageNotPresentLocally where
  renderDiagnostic (DockerEngineImageNotPresentLocally tag) = do
    let header = "Could not find: " <> tag <> " in local repository. Perform: docker pull " <> tag <> ", prior to running fossa."
    Errata (Just header) [] Nothing

parsePodmanSource ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Exec sig m
  , Has ReadFS sig m
  ) =>
  Text ->
  m ContainerImageSource
parsePodmanSource img = execThrow' (podmanInspectImage img) $> Podman img

parseRegistrySource ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  ) =>
  Text ->
  Text ->
  m ContainerImageSource
parseRegistrySource defaultArch tag = case parse (parseImageUrl defaultArch) "" tag of
  Left err -> do
    let structuredError = "\n" <> errorBundlePretty err
    fatal structuredError
  Right registrySource -> pure $ Registry registrySource
