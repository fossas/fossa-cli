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
import Container.Docker.SourceParser (RegistryImageSource (..), parseImageUrl)
import Container.Types (ContainerScan (..))
import Control.Carrier.DockerEngineApi (runDockerEngineApi)
import Control.Effect.Debug (Debug)
import Control.Effect.Diagnostics (Diagnostics, ToDiagnostic, context, errCtx, fatal, fatalText, fromEitherShow, fromMaybeText, recover, warnOnSomeException, (<||>))
import Control.Effect.DockerEngineApi (DockerEngineApi, getDockerImageSize, isDockerEngineAccessible)
import Control.Effect.Exception (SomeException)
import Control.Effect.Lift (Has, Lift, sendIO)
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
  hsep,
  logInfo,
  renderIt,
  viaShow,
  vsep,
 )
import Effect.ReadFS (ReadFS, doesFileExist, getCurrentDir)
import Errata (Errata (..))
import Path (Abs, File, Path, SomeBase (Abs, Rel), filename, parseSomeFile, splitExtension, (</>))
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
    scan <- join <$> traverse (recover . analyzeTarball) tarball
    traverse (correctCirceSource parsedSource) scan
  maybe (legacyScan parsedSource) pure circePoweredScan
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
    meta <- join <$> traverse (recover . analyzeTarball) tarball
    traverse (correctTag parsedSource) meta
  maybe (legacyScan parsedSource) pure circePoweredScan
  where
    correctTag src (tag, digest) = do
      tag' <- correctCirceTag src tag
      pure (tag', digest)
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

-- | Circe re-exports the image as a plain tarball, which means that the typical FOSSA CLI operations for choosing the image tag
-- don't get to run if we use Circe. This function adds them back in.
correctCirceSource :: (Has Diagnostics sig m, Has (Lift IO) sig m) => ContainerImageSource -> ContainerScan -> m ContainerScan
correctCirceSource src scan = do
  imageTag' <- correctCirceTag src $ imageTag scan
  pure
    ContainerScan
      { imageData = (imageData scan)
      , imageDigest = (imageDigest scan)
      , imageTag = imageTag'
      }

correctCirceTag :: (Has Diagnostics sig m, Has (Lift IO) sig m) => ContainerImageSource -> Text -> m Text
correctCirceTag src tag = case src of
  DockerArchive path -> do
    split <-
      warnOnSomeException (DetermineFileNameFailure path) "Determine file name without extension"
        . sendIO
        . splitExtension
        $ filename path
    maybe (pure tag) (pure . toText . fst) split
  Registry RegistryImageSource{..} -> pure $ registryHost <> "/" <> registryContainerRepository
  _ -> pure tag

data DetermineFileNameFailure = DetermineFileNameFailure (Path Abs File) SomeException

instance ToDiagnostic DetermineFileNameFailure where
  renderDiagnostic (DetermineFileNameFailure file exc) = do
    let header = "An error occurred while attempting to determine the name of the file."
        body =
          renderIt $
            vsep
              [ hsep ["File path:", pretty $ toText file]
              , hsep ["Error text:", viaShow exc]
              ]
    Errata (Just header) [] (Just body)

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
