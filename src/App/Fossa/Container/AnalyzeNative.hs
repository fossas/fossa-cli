{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Container.AnalyzeNative (
  analyzeExperimental,
) where

import App.Fossa.Analyze (applyFiltersToProject, updateProgress)
import App.Fossa.Analyze.Debug (diagToDebug)
import App.Fossa.Analyze.Discover (DiscoverFunc (DiscoverFunc))
import App.Fossa.Analyze.Project (ProjectResult, mkResult)
import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject), AnalyzeTaskEffs)
import App.Fossa.Config.Analyze (ExperimentalAnalyzeConfig (ExperimentalAnalyzeConfig))
import App.Fossa.Config.Container.Analyze (
  ContainerAnalyzeConfig (
    ContainerAnalyzeConfig,
    imageLocator,
    revisionOverride,
    scanDestination
  ),
 )
import App.Fossa.Config.Container.Common (ImageText (unImageText))
import Container.Docker.Source.Tarball (mkFsFromChangeset, parse)
import Container.Os.Release (getOsInfo)
import Container.Types (ContainerImageRaw (layers), layerChangeSets)
import Control.Carrier.AtomicCounter (runAtomicCounter)
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.Finally (runFinally)
import Control.Carrier.Output.IO (output, runOutput)
import Control.Carrier.Reader (runReader)
import Control.Carrier.Stack.StickyLog (stickyLogStack)
import Control.Carrier.StickyLogger (runStickyLogger)
import Control.Carrier.TarballReadFS (runTarballReadFSIO)
import Control.Carrier.TaskPool (withTaskPool)
import Control.Concurrent (getNumCapabilities)
import Control.Effect.AtomicCounter (AtomicCounter)
import Control.Effect.Debug (Debug, debugMetadata)
import Control.Effect.Diagnostics (Diagnostics, fatal, fatalText)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Output (Output)
import Control.Effect.Reader (Reader)
import Control.Effect.Stack (Stack, withEmptyStack)
import Control.Effect.TaskPool (TaskPool)
import Control.Effect.Telemetry (Telemetry)
import Data.Aeson.Types qualified as Aeson
import Data.ByteString.Lazy qualified as BS
import Data.Foldable (Foldable (fold), traverse_)
import Data.String.Conversion (ToString (toString), toStrict)
import Data.Text (Text)
import Discovery.Filters (AllFilters)
import Discovery.Projects (withDiscoveredProjects)
import Effect.Exec (Exec)
import Effect.Logger (
  Has,
  Logger,
  Pretty (pretty),
  Severity (SevInfo, SevWarn),
  logInfo,
  logStdout,
  viaShow,
 )
import Effect.ReadFS (ReadFS)
import Path (Abs, Dir, File, Path, fileExtension, parseAbsFile, toFilePath)
import Path.Internal.Posix (Path (Path))
import Strategy.Apk qualified as Apk
import Strategy.Python.Setuptools qualified as Setuptools
import Text.Pretty.Simple (pShowNoColor)
import Types (DiscoveredProject (projectData, projectPath, projectType))

data ContainerImageSource
  = ContainerExportedTarball (Path Abs File)
  | ContainerDockerImage Text
  | ContainerOCIRegistry Text Text
  deriving (Show, Eq)

analyzeExperimental ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has Telemetry sig m
  ) =>
  ContainerAnalyzeConfig ->
  m ()
analyzeExperimental ContainerAnalyzeConfig{..} = do
  logInfo "Running in fossa experimental mode ...."

  parsedSource <- parseContainerImageSource (unImageText imageLocator)
  case parsedSource of
    ContainerDockerImage _ -> fatalText "container images from daemon are not yet supported!"
    ContainerOCIRegistry _ _ -> fatalText "container images from oci registry are not yet supported!"
    ContainerExportedTarball tarball -> do
      _ <- analyzeContainer tarball
      pure ()

parseContainerImageSource :: (Has (Lift IO) sig m, Has Diagnostics sig m) => Text -> m (ContainerImageSource)
parseContainerImageSource = parseContainerExportedTarball

parseContainerExportedTarball :: (Has (Lift IO) sig m, Has Diagnostics sig m) => Text -> m (ContainerImageSource)
parseContainerExportedTarball path = case parseAbsFile (toString path) of
  Left err -> fatal err
  Right a -> do
    case fileExtension a of
      Left err -> fatal err
      Right ext ->
        if ext == ".tar"
          then pure (ContainerExportedTarball a)
          else fatalText "expected to have .tar extension for tarball source"

analyzeContainer :: (Has Diagnostics sig m, Has Exec sig m, Has (Lift IO) sig m, Has Logger sig m, Has Telemetry sig m) => Path Abs File -> m ([ProjectResult])
analyzeContainer tarball = do
  containerTarball <- sendIO $ BS.readFile (toString tarball)
  fsTree <- case parse containerTarball of
    Left err -> fatal err
    Right cir -> do
      let squashed = fold $! layers cir
      pure (mkFsFromChangeset $! layerChangeSets squashed)

  osInfo <- runTarballReadFSIO fsTree tarball getOsInfo
  logStdout $ toStrict (pShowNoColor osInfo)

  capabilities <- sendIO getNumCapabilities
  runReader withoutAnyExperimentalPreferences
    . runReader (mempty :: AllFilters)
    . ignoreDebug
    $ do
      (projectResults, ()) <-
        Diag.context "discovery/analysis tasks"
          . runTarballReadFSIO (fsTree) tarball
          . runOutput @ProjectResult
          . runStickyLogger SevInfo
          . runFinally
          . withTaskPool capabilities updateProgress
          . runAtomicCounter
          $ do
            runAnalyzers mempty
      pure projectResults
  where
    withoutAnyExperimentalPreferences = ExperimentalAnalyzeConfig Nothing

runAnalyzers ::
  ( AnalyzeTaskEffs sig m
  , Has (Output ProjectResult) sig m
  , Has TaskPool sig m
  , Has AtomicCounter sig m
  ) =>
  AllFilters ->
  m ()
runAnalyzers filters = do
  traverse_
    single
    [ DiscoverFunc Setuptools.discover
    , DiscoverFunc Apk.discover
    ]
  where
    single (DiscoverFunc f) = withDiscoveredProjects f basedir (runDependencyAnalysis basedir filters)
    basedir = Path "vfs-root" -- FIXME

runDependencyAnalysis ::
  ( AnalyzeProject proj
  , Aeson.ToJSON proj
  , Has (Lift IO) sig m
  , Has AtomicCounter sig m
  , Has Debug sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has (Output ProjectResult) sig m
  , Has (Reader ExperimentalAnalyzeConfig) sig m
  , Has (Reader AllFilters) sig m
  , Has Stack sig m
  , Has Telemetry sig m
  ) =>
  -- | Analysis base directory
  Path Abs Dir ->
  AllFilters ->
  DiscoveredProject proj ->
  m ()
runDependencyAnalysis basedir filters project = do
  case applyFiltersToProject basedir filters project of
    Nothing -> logInfo $ "Skipping " <> pretty (projectType project) <> " project at " <> viaShow (projectPath project) <> ": no filters matched"
    Just targets -> do
      logInfo $ "Analyzing " <> pretty (projectType project) <> " project at " <> pretty (toFilePath (projectPath project))
      graphResult <- Diag.runDiagnosticsIO . diagToDebug . stickyLogStack . withEmptyStack . Diag.context "Project Analysis" $ do
        debugMetadata "DiscoveredProject" project
        analyzeProject targets (projectData project)
      Diag.withResult SevWarn SevWarn graphResult (output . mkResult basedir project)
