{-# LANGUAGE NamedFieldPuns #-}

module App.Fossa.Container.Sources.DockerArchive (
  analyzeFromDockerArchive,
  listTargetsFromDockerArchive,
  revisionFromDockerArchive,
) where

import App.Fossa.Analyze (applyFiltersToProject, toProjectResult, updateProgress)
import App.Fossa.Analyze.Debug (diagToDebug)
import App.Fossa.Analyze.Discover (DiscoverFunc (DiscoverFunc))
import App.Fossa.Analyze.Project (mkResult)
import App.Fossa.Analyze.Types (
  AnalyzeProject (analyzeProjectStaticOnly),
  AnalyzeStaticTaskEffs,
  DiscoveredProjectIdentifier (..),
  DiscoveredProjectScan (..),
 )
import App.Fossa.Config.Analyze (ExperimentalAnalyzeConfig (ExperimentalAnalyzeConfig), GoDynamicTactic (GoModulesBasedTactic), WithoutDefaultFilters (..))
import App.Fossa.Container.Sources.Discovery (layerAnalyzers, renderLayerTarget)
import App.Fossa.Container.Sources.JarAnalysis (analyzeContainerJars)
import App.Types (Mode (..))
import Codec.Archive.Tar.Index (TarEntryOffset)
import Container.Docker.Manifest (getImageDigest, getRepoTags)
import Container.OsRelease (OsInfo (nameId, version), getOsInfo)
import Container.Tarball (mkFsFromChangeset, parse)
import Container.TarballReadFs (runTarballReadFSIO)
import Container.Types (
  ContainerImageRaw (..),
  ContainerLayer (..),
  ContainerScan (..),
  ContainerScanImage (ContainerScanImage),
  ContainerScanImageLayer (ContainerScanImageLayer),
  baseLayer,
  discoveredJars,
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
import Control.Effect.Diagnostics (Diagnostics, context, fromEither, fromMaybeText, warnThenRecover)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Output (Output)
import Control.Effect.Reader (Reader)
import Control.Effect.Stack (Stack, withEmptyStack)
import Control.Effect.TaskPool (TaskPool)
import Control.Effect.Telemetry (Telemetry)
import Control.Monad (join, void, when)
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy qualified as BS
import Data.FileTree.IndexFileTree (SomeFileTree, fixedVfsRoot)
import Data.Flag (Flag, fromFlag)
import Data.Foldable (traverse_)
import Data.Map qualified as Map
import Data.Maybe (isNothing, listToMaybe, mapMaybe)
import Data.String.Conversion (ToString (toString))
import Data.Text (Text)
import Data.Text.Extra (breakOnEndAndRemove, showT)
import Discovery.Filters (AllFilters, MavenScopeFilters (..), isDefaultNonProductionPath)
import Discovery.Projects (withDiscoveredProjects)
import Effect.Exec (Exec)
import Effect.Logger (
  Has,
  Logger,
  Pretty (pretty),
  Severity (..),
  logDebug,
  logInfo,
  logWarn,
  viaShow,
 )
import Effect.ReadFS (ReadFS)
import Path (Abs, Dir, File, Path, toFilePath)
import Path.Internal (Path (Path))
import Srclib.Converter qualified as Srclib
import Srclib.Types (SourceUnit)
import Types (DiscoveredProject (..))

-- | Analyzes Docker Image from Exported Tarball Source.
analyzeFromDockerArchive ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Exec sig m -- May not exec dynamic strategies. TODO: Remove this and convert the BerkeleyDB driver to FFI.
  , Has Logger sig m
  , Has Telemetry sig m
  , Has Debug sig m
  ) =>
  Bool ->
  AllFilters ->
  Flag WithoutDefaultFilters ->
  Path Abs File ->
  m ContainerScan
analyzeFromDockerArchive systemDepsOnly filters withoutDefaultFilters tarball = do
  capabilities <- sendIO getNumCapabilities
  containerTarball <- sendIO . BS.readFile $ toString tarball

  observations <- warnThenRecover @Text "Error Running JAR analyzer (millhone)" $
    do
      logInfo "Searching for JARs in container image."
      analyzeContainerJars tarball

  image <- fromEither $ parse containerTarball

  -- get Image Digest and Tags
  let manifest = rawManifest image
  let imageDigest = getImageDigest manifest
  imageTag <- extractRepoName (getRepoTags manifest)

  -- Analyze Base Layer
  logInfo "Analyzing Base Layer"
  baseFs <- context "Building base layer FS" $ mkFsFromChangeset $ baseLayer image
  let imageBaseLayer = baseLayer image
      baseDigest = layerDigest imageBaseLayer
  osInfo <-
    context "Retrieving OS Information"
      . warnThenRecover @Text "Could not retrieve OS info"
      $ runTarballReadFSIO baseFs tarball getOsInfo

  when (isNothing osInfo) $
    logInfo "No image system information detected. System dependencies will not be included with this scan."

  baseUnits <-
    context "Analyzing From Base Layer" $
      analyzeLayer systemDepsOnly filters withoutDefaultFilters capabilities osInfo baseFs tarball

  let mkScan :: [ContainerScanImageLayer] -> ContainerScan
      mkScan layers =
        ContainerScan
          { imageData =
              ( ContainerScanImage
                  (nameId <$> osInfo)
                  (version <$> osInfo)
                  layers
              )
          , imageDigest
          , imageTag
          }

  (baseObservations, otherObservations) <-
    case (observations, layerPath imageBaseLayer) of
      (Just observations', baseLayerPath) ->
        pure
          . bimap (join . Map.elems) (join . Map.elems)
          -- If a base layer does not exist not exist, jar observations will appear in "other" layers.
          . Map.partitionWithKey (\layerPath _ -> Just layerPath == baseLayerPath)
          $ (discoveredJars observations')
      _ -> do
        logInfo "Failed to run Jar analyzer."
        logInfo "If you were expecting JAR analysis results, run with '--debug' for more info."
        pure ([], [])

  let baseScanImageLayer = ContainerScanImageLayer baseDigest baseUnits baseObservations

  if hasOtherLayers image
    then do
      logInfo "Analyzing Other Layers"
      let squashedDigest = layerDigest . otherLayersSquashed $ image
      fs <- context "Building squashed FS from other layers" . mkFsFromChangeset $ otherLayersSquashed image
      otherUnits <-
        context "Analyzing from Other Layers" $
          analyzeLayer systemDepsOnly filters withoutDefaultFilters capabilities osInfo fs tarball

      let scan =
            mkScan
              [ baseScanImageLayer
              , ContainerScanImageLayer squashedDigest otherUnits otherObservations
              ]
      pure scan
    else pure $ mkScan [baseScanImageLayer]

analyzeLayer ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Exec sig m -- May not exec dynamic strategies. TODO: Remove this and convert the BerkeleyDB driver to FFI.
  , Has Logger sig m
  , Has Telemetry sig m
  , Has Debug sig m
  ) =>
  Bool ->
  AllFilters ->
  Flag WithoutDefaultFilters ->
  Int ->
  Maybe OsInfo ->
  SomeFileTree TarEntryOffset ->
  Path Abs File ->
  m [SourceUnit]
analyzeLayer systemDepsOnly filters withoutDefaultFilters capabilities osInfo layerFs tarball = do
  toSourceUnit
    <$> (runReader filters)
      ( do
          (projectResults, ()) <-
            runTarballReadFSIO layerFs tarball
              . runReader noExperimental
              . runReader noMavenScopeFilters
              . runReader NonStrict
              . Diag.context "discovery/analysis tasks"
              . runOutput @DiscoveredProjectScan
              . runStickyLogger SevInfo
              . runFinally
              . withTaskPool capabilities updateProgress
              . runAtomicCounter
              $ do
                runAnalyzers systemDepsOnly osInfo filters withoutDefaultFilters
          pure projectResults
      )
  where
    noMavenScopeFilters :: MavenScopeFilters
    noMavenScopeFilters = MavenScopeIncludeFilters mempty
    noExperimental :: ExperimentalAnalyzeConfig
    noExperimental =
      ExperimentalAnalyzeConfig
        Nothing
        GoModulesBasedTactic -- Discovery is the same for both module and package centric analysis.
        False -- Discovery has no consequence from path dependency analysis config
    toSourceUnit :: [DiscoveredProjectScan] -> [SourceUnit]
    toSourceUnit =
      map (Srclib.projectToSourceUnit False)
        . mapMaybe toProjectResult

runAnalyzers ::
  ( AnalyzeStaticTaskEffs sig m
  , Has (Output DiscoveredProjectScan) sig m
  , Has TaskPool sig m
  , Has AtomicCounter sig m
  ) =>
  Bool ->
  Maybe OsInfo ->
  AllFilters ->
  Flag WithoutDefaultFilters ->
  m ()
runAnalyzers systemDepsOnly osInfo filters withoutDefaultFilters = do
  traverse_
    single
    $ layerAnalyzers osInfo systemDepsOnly
  where
    single (DiscoverFunc f) = withDiscoveredProjects f basedir (runDependencyAnalysis basedir filters withoutDefaultFilters)
    basedir = Path $ toString fixedVfsRoot

runDependencyAnalysis ::
  ( AnalyzeProject proj
  , Has (Lift IO) sig m
  , Has Exec sig m -- May not exec dynamic strategies. TODO: Remove this and convert the BerkeleyDB driver to FFI.
  , Has AtomicCounter sig m
  , Has Debug sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has (Output DiscoveredProjectScan) sig m
  , Has (Reader ExperimentalAnalyzeConfig) sig m
  , Has (Reader MavenScopeFilters) sig m
  , Has (Reader Mode) sig m
  , Has (Reader AllFilters) sig m
  , Has Stack sig m
  , Has Telemetry sig m
  ) =>
  Path Abs Dir ->
  AllFilters ->
  Flag WithoutDefaultFilters ->
  DiscoveredProject proj ->
  m ()
runDependencyAnalysis basedir filters withoutDefaultFilters project@DiscoveredProject{projectType, projectPath, projectData} = do
  let dpi = DiscoveredProjectIdentifier projectPath projectType
  let hasNonProductionPath =
        not (fromFlag WithoutDefaultFilters withoutDefaultFilters) && isDefaultNonProductionPath basedir projectPath

  case (applyFiltersToProject basedir filters project, hasNonProductionPath) of
    (Nothing, _) -> do
      logInfo $ "Skipping " <> pretty projectType <> " project at " <> viaShow projectPath <> ": no filters matched"
      output $ SkippedDueToProvidedFilter dpi
    (Just _, True) -> do
      logInfo $ "Skipping " <> pretty projectType <> " project at " <> viaShow projectPath <> " (default filtering)"
      output $ SkippedDueToDefaultFilter dpi
    (Just targets, False) -> do
      logInfo $ "Analyzing " <> pretty projectType <> " project at " <> pretty (toFilePath projectPath)
      let ctxMessage = "Project Analysis: " <> showT projectType
      graphResult <- Diag.runDiagnosticsIO . diagToDebug . stickyLogStack . withEmptyStack $
        Diag.context ctxMessage $ do
          analyzeProjectStaticOnly targets projectData
      Diag.flushLogs SevError SevDebug graphResult
      output $ Scanned dpi (mkResult basedir project Nothing <$> graphResult)

-- | Extracts Repository Name.
--
-- >> extractRepoName ["redis:alpine"] = "redis"
-- >> extractRepoName ["redis@someDigest"] = "redis"
-- -
extractRepoName :: (Has Logger sig m, Has Diagnostics sig m) => [Text] -> m Text
extractRepoName tags = do
  firstTag <- fromMaybeText "No image tags found" $ listToMaybe tags
  tagTuple <-
    fromMaybeText "Image was not in the format name:tag or name@digest" $
      breakOnEndAndRemove "@" firstTag
        <|> breakOnEndAndRemove ":" firstTag
  logDebug . pretty $ "Identified project name: " <> fst tagTuple
  pure $ fst tagTuple

revisionFromDockerArchive ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  Path Abs File ->
  m (Text, Text)
revisionFromDockerArchive tarball = do
  containerTarball <- sendIO . BS.readFile $ toString tarball
  image <- fromEither $ parse containerTarball
  let manifest = rawManifest image
  let imageDigest = getImageDigest manifest
  imageTag <- extractRepoName (getRepoTags manifest)
  pure (imageTag, imageDigest)

listTargetsFromDockerArchive ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Exec sig m -- May not exec dynamic strategies. TODO: Remove this and convert the BerkeleyDB driver to FFI.
  , Has Logger sig m
  , Has Telemetry sig m
  ) =>
  Path Abs File ->
  m ()
listTargetsFromDockerArchive tarball = do
  capabilities <- sendIO getNumCapabilities
  containerTarball <- sendIO . BS.readFile $ toString tarball
  image <- fromEither $ parse containerTarball

  logWarn "fossa container list-targets does not apply any filtering, you may see projects which are not present in the final analysis."
  logWarn "fossa container list-targets only lists targets for experimental-scanner (when analyzed with --experimental-scanner flag)."

  logInfo "Analyzing Base Layer"
  baseFs <- context "Building Base Layer FS" $ mkFsFromChangeset $ baseLayer image
  osInfo <-
    context "Retrieving OS Information"
      . warnThenRecover @Text "Could not retrieve OS info"
      $ runTarballReadFSIO baseFs tarball getOsInfo
  context "Analyzing From Base Layer" $ listTargetLayer capabilities osInfo baseFs tarball "Base Layer"

  when (hasOtherLayers image) $ do
    logInfo "Analyzing Other Layers"
    fs <- context "Building squashed FS from other layers" $ mkFsFromChangeset $ otherLayersSquashed image
    void . context "Analyzing from Other Layers" $ listTargetLayer capabilities osInfo fs tarball "Other Layers"

listTargetLayer ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Exec sig m -- May not exec dynamic strategies. TODO: Remove this and convert the BerkeleyDB driver to FFI.
  , Has Logger sig m
  , Has Telemetry sig m
  ) =>
  Int ->
  Maybe OsInfo ->
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
    . runReader
      ( ExperimentalAnalyzeConfig
          Nothing
          GoModulesBasedTactic -- Targets aren't different between package/module centric analysis for Go.
          False -- Targets are not impacted by path dependencies.
      )
    . runReader (MavenScopeIncludeFilters mempty)
    . runReader (NonStrict :: Mode)
    . runReader (mempty :: AllFilters)
    $ run
  where
    run = traverse_ findTargets $ layerAnalyzers osInfo False
    findTargets (DiscoverFunc f) = withDiscoveredProjects f basedir (renderLayerTarget basedir layerType)
    basedir = Path $ toString fixedVfsRoot
