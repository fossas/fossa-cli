{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Container.Sources.DockerArchive (
  analyzeFromDockerArchive,
  listTargetsFromDockerArchive,
) where

import App.Fossa.Analyze (applyFiltersToProject, toProjectResult, updateProgress)
import App.Fossa.Analyze.Debug (diagToDebug)
import App.Fossa.Analyze.Discover (DiscoverFunc (DiscoverFunc))
import App.Fossa.Analyze.Project (mkResult)
import App.Fossa.Analyze.Types (
  AnalyzeProject (analyzeProject),
  AnalyzeStaticTaskEffs,
  AnalyzeTaskEffs,
  DiscoveredProjectIdentifier (..),
  DiscoveredProjectScan (..),
 )
import App.Fossa.Config.Analyze (ExperimentalAnalyzeConfig (ExperimentalAnalyzeConfig))
import Codec.Archive.Tar.Index (TarEntryOffset)
import Container.Docker.Manifest (getImageDigest, getRepoTags)
import Container.OsRelease (OsInfo (nameId, version), getOsInfo)
import Container.Tarball (mkFsFromChangeset, parse)
import Container.TarballReadFs (runTarballReadFSIO)
import Container.Types (
  ContainerImageRaw (..),
  ContainerLayer (layerDigest),
  ContainerScan (ContainerScan),
  ContainerScanImage (ContainerScanImage),
  ContainerScanImageLayer (ContainerScanImageLayer),
  baseLayer,
  hasOtherLayers,
  otherLayersSquashed,
 )
import Control.Applicative ((<|>))
import Control.Carrier.AtomicCounter (runAtomicCounter)
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.Finally (runFinally)
import Control.Carrier.Output.IO (output, runOutput)
import Control.Carrier.Reader (runReader)
import Control.Carrier.Stack.StickyLog (stickyLogStack)
import Control.Carrier.StickyLogger (runStickyLogger)
import Control.Carrier.TaskPool (withTaskPool)
import Control.Concurrent (getNumCapabilities)
import Control.Effect.AtomicCounter (AtomicCounter)
import Control.Effect.Debug (Debug)
import Control.Effect.Diagnostics (Diagnostics, context, fromEither, fromMaybeText)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Output (Output)
import Control.Effect.Reader (Reader)
import Control.Effect.Stack (Stack, withEmptyStack)
import Control.Effect.TaskPool (TaskPool)
import Control.Effect.Telemetry (Telemetry)
import Control.Monad (void)
import Data.ByteString.Lazy qualified as BS
import Data.FileTree.IndexFileTree (SomeFileTree, fixedVfsRoot)
import Data.Foldable (for_, traverse_)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Set.NonEmpty (toSet)
import Data.String.Conversion (ToString (toString))
import Data.Text (Text)
import Data.Text.Extra (breakOnAndRemove, showT)
import Discovery.Filters (AllFilters)
import Discovery.Projects (withDiscoveredProjects)
import Effect.Exec (Exec)
import Effect.Logger (
  Has,
  Logger,
  Pretty (pretty),
  Severity (..),
  logInfo,
  logWarn,
  viaShow,
 )
import Effect.ReadFS (ReadFS)
import Path (Abs, Dir, File, Path, toFilePath)
import Path.IO (makeRelative)
import Path.Internal (Path (Path))
import Srclib.Converter qualified as Srclib
import Srclib.Types (SourceUnit)
import Strategy.ApkDatabase qualified as Apk
import Strategy.Bundler qualified as Bundler
import Strategy.Carthage qualified as Carthage
import Strategy.Composer qualified as Composer
import Strategy.Dpkg qualified as Dpkg
import Strategy.Fpm qualified as Fpm
import Strategy.Glide qualified as Glide
import Strategy.Googlesource.RepoManifest qualified as RepoManifest
import Strategy.Node qualified as Node
import Strategy.NuGet.Nuspec qualified as Nuspec
import Strategy.NuGet.PackageReference qualified as PackageReference
import Strategy.NuGet.PackagesConfig qualified as PackagesConfig
import Strategy.NuGet.Paket qualified as Paket
import Strategy.NuGet.ProjectAssetsJson qualified as ProjectAssetsJson
import Strategy.NuGet.ProjectJson qualified as ProjectJson
import Strategy.Perl qualified as Perl
import Strategy.Python.Poetry qualified as Poetry
import Strategy.Python.Setuptools qualified as Setuptools
import Strategy.RPM qualified as RPM
import Strategy.SwiftPM qualified as SwiftPM
import Types (DiscoveredProject (..), FoundTargets (FoundTargets, ProjectWithoutTargets), unBuildTarget)

-- | Analyzes Docker Image from Exported Tarball Source.
analyzeFromDockerArchive ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has Telemetry sig m
  , Has Debug sig m
  ) =>
  AllFilters ->
  Path Abs File ->
  m ContainerScan
analyzeFromDockerArchive filters tarball = do
  capabilities <- sendIO getNumCapabilities
  containerTarball <- sendIO . BS.readFile $ toString tarball
  image <- fromEither $ parse containerTarball

  -- get Image Digest and Tags
  let manifest = rawManifest image
  let imageDigest = getImageDigest manifest
  imageTag <- extractRepoName (getRepoTags manifest)

  -- Analyze Base Layer
  logInfo "Analyzing Base Layer"
  let baseFs = mkFsFromChangeset $ baseLayer image
  let baseDigest = layerDigest . baseLayer $ image
  osInfo <-
    context "Retrieving Os Information" $
      runTarballReadFSIO baseFs tarball getOsInfo
  baseUnits <-
    context "Analyzing From Base Layer" $
      analyzeLayer filters capabilities osInfo baseFs tarball

  let mkScan :: [ContainerScanImageLayer] -> ContainerScan
      mkScan layers =
        ContainerScan
          ( ContainerScanImage
              (nameId osInfo)
              (version osInfo)
              layers
          )
          imageDigest
          imageTag

  if hasOtherLayers image
    then do
      logInfo "Analyzing Other Layers"
      let squashedDigest = layerDigest . otherLayersSquashed $ image
      otherUnits <-
        context "Squashing all non-base layer for analysis" $
          analyzeLayer
            filters
            capabilities
            osInfo
            (mkFsFromChangeset $ otherLayersSquashed image)
            tarball
      pure $
        mkScan
          [ ContainerScanImageLayer baseDigest baseUnits
          , ContainerScanImageLayer squashedDigest otherUnits
          ]
    else pure $ mkScan [ContainerScanImageLayer baseDigest baseUnits]

analyzeLayer ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has Telemetry sig m
  , Has Debug sig m
  ) =>
  AllFilters ->
  Int ->
  OsInfo ->
  SomeFileTree TarEntryOffset ->
  Path Abs File ->
  m [SourceUnit]
analyzeLayer filters capabilities osInfo layerFs tarball =
  map (Srclib.toSourceUnit noUnusedDeps)
    . mapMaybe toProjectResult
    <$> (runReader noFilters)
      ( do
          (projectResults, ()) <-
            runTarballReadFSIO layerFs tarball
              . runReader noExperimental
              . Diag.context "discovery/analysis tasks"
              . runOutput @DiscoveredProjectScan
              . runStickyLogger SevInfo
              . runFinally
              . withTaskPool capabilities updateProgress
              . runAtomicCounter
              $ do
                runAnalyzers osInfo filters
          pure projectResults
      )
  where
    noExperimental :: ExperimentalAnalyzeConfig
    noExperimental = ExperimentalAnalyzeConfig Nothing

    noUnusedDeps :: Bool
    noUnusedDeps = False

    -- TODO: Implement filters from .fossa.yml file
    noFilters :: AllFilters
    noFilters = mempty

runAnalyzers ::
  ( AnalyzeTaskEffs sig m
  , Has (Output DiscoveredProjectScan) sig m
  , Has TaskPool sig m
  , Has AtomicCounter sig m
  ) =>
  OsInfo ->
  AllFilters ->
  m ()
runAnalyzers osInfo filters = do
  traverse_
    single
    [ DiscoverFunc (Apk.discover osInfo)
    , DiscoverFunc (Dpkg.discover osInfo)
    ]
  where
    single (DiscoverFunc f) = withDiscoveredProjects f basedir (runDependencyAnalysis basedir filters)
    basedir = Path $ toString fixedVfsRoot

runDependencyAnalysis ::
  ( AnalyzeProject proj
  , Has (Lift IO) sig m
  , Has AtomicCounter sig m
  , Has Debug sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has (Output DiscoveredProjectScan) sig m
  , Has (Reader ExperimentalAnalyzeConfig) sig m
  , Has (Reader AllFilters) sig m
  , Has Stack sig m
  , Has Telemetry sig m
  ) =>
  Path Abs Dir ->
  AllFilters ->
  DiscoveredProject proj ->
  m ()
runDependencyAnalysis basedir filters project@DiscoveredProject{..} = do
  let dpi = DiscoveredProjectIdentifier projectPath projectType
  case applyFiltersToProject basedir filters project of
    Nothing -> do
      logInfo $ "Skipping " <> pretty projectType <> " project at " <> viaShow projectPath <> ": no filters matched"
      output $ SkippedDueToProvidedFilter dpi
    Just targets -> do
      logInfo $ "Analyzing " <> pretty projectType <> " project at " <> pretty (toFilePath projectPath)
      let ctxMessage = "Project Analysis: " <> showT projectType
      graphResult <- Diag.runDiagnosticsIO . diagToDebug . stickyLogStack . withEmptyStack . Diag.context ctxMessage $ do
        analyzeProject targets projectData
      Diag.flushLogs SevError SevDebug graphResult
      output $ Scanned dpi (mkResult basedir project <$> graphResult)

-- | Extracts Repository Name.
--
-- >> extractRepoName ["redis:alpine"] = "redis"
-- >> extractRepoName ["redis@someDigest"] = "redis"
-- -
extractRepoName :: Has Diagnostics sig m => [Text] -> m Text
extractRepoName tags = do
  firstTag <- fromMaybeText "No image tags found" $ listToMaybe tags
  tagTuple <-
    fromMaybeText "Image was not in the format name:tag or name@digest" $
      breakOnAndRemove "@" firstTag
        <|> breakOnAndRemove ":" firstTag
  pure $ fst tagTuple

osDepsAnalyzers :: AnalyzeStaticTaskEffs sig m => OsInfo -> [DiscoverFunc m]
osDepsAnalyzers osInfo =
  [ DiscoverFunc (Apk.discover osInfo)
  , DiscoverFunc (Dpkg.discover osInfo)
  ]

staticOnlyManagedDepsAnalyzers :: AnalyzeStaticTaskEffs sig m => [DiscoverFunc m]
staticOnlyManagedDepsAnalyzers =
  [ DiscoverFunc Bundler.discover
  , DiscoverFunc Carthage.discover
  , DiscoverFunc Composer.discover
  , DiscoverFunc Fpm.discover
  , DiscoverFunc Glide.discover
  , DiscoverFunc Node.discover
  , DiscoverFunc Nuspec.discover
  , DiscoverFunc PackageReference.discover
  , DiscoverFunc PackagesConfig.discover
  , DiscoverFunc Paket.discover
  , DiscoverFunc Perl.discover
  , DiscoverFunc Poetry.discover
  , DiscoverFunc ProjectAssetsJson.discover
  , DiscoverFunc ProjectJson.discover
  , DiscoverFunc RPM.discover
  , DiscoverFunc RepoManifest.discover
  , DiscoverFunc Setuptools.discover
  , DiscoverFunc SwiftPM.discover
  ]

listTargetLayer ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  Int ->
  OsInfo ->
  SomeFileTree TarEntryOffset ->
  Path Abs File ->
  Text ->
  m ()
listTargetLayer capabilities osInfo layerFs tarball layerType = do
  ignoreDebug
    . runTarballReadFSIO layerFs tarball
    . runStickyLogger SevInfo
    . runFinally
    . withTaskPool capabilities updateProgress
    . runAtomicCounter
    . runReader (ExperimentalAnalyzeConfig Nothing)
    . runReader (mempty :: AllFilters)
    $ run
  where
    run = traverse_ findTargets [DiscoverFunc (Apk.discover osInfo), DiscoverFunc (Dpkg.discover osInfo)]
    findTargets (DiscoverFunc f) = withDiscoveredProjects f basedir (printSingle basedir layerType)
    basedir = Path $ toString fixedVfsRoot

listTargetsFromDockerArchive ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  Path Abs File ->
  m ()
listTargetsFromDockerArchive tarball = do
  capabilities <- sendIO getNumCapabilities
  containerTarball <- sendIO . BS.readFile $ toString tarball
  image <- fromEither $ parse containerTarball
  logWarn "fossa container list-targets does not apply any filtering, you may see projects which are not present in the final analysis."

  let baseFs = mkFsFromChangeset $ baseLayer image
  osInfo <- runTarballReadFSIO baseFs tarball getOsInfo
  listTargetLayer capabilities osInfo baseFs tarball "Base Layer"

  if hasOtherLayers image
    then do
      void $
        listTargetLayer
          capabilities
          osInfo
          (mkFsFromChangeset $ otherLayersSquashed image)
          tarball
          "Other Layers"
    else pure ()

-- This should be shared
printSingle :: Has Logger sig m => Path Abs Dir -> Text -> DiscoveredProject a -> m ()
printSingle basedir layerLocation project = do
  let maybeRel = makeRelative basedir (projectPath project)

  case maybeRel of
    Nothing -> pure ()
    Just rel -> do
      logInfo $
        "Found project: "
          <> pretty (projectType project)
          <> "@"
          <> pretty (toFilePath rel)
          <> pretty (" (" <> layerLocation <> ")")

      case projectBuildTargets project of
        ProjectWithoutTargets -> do
          logInfo $
            "Found target: "
              <> pretty (projectType project)
              <> "@"
              <> pretty (toFilePath rel)
              <> pretty (" (" <> layerLocation <> ")")
        FoundTargets targets -> for_ (Set.toList $ toSet targets) $ \target -> do
          logInfo $
            "Found target: "
              <> pretty (projectType project)
              <> "@"
              <> pretty (toFilePath rel)
              <> ":"
              <> pretty (unBuildTarget target)
              <> pretty (" (" <> layerLocation <> ")")
