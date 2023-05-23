{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Analyze (
  analyzeMain,
  updateProgress,
  runAnalyzers,
  runDependencyAnalysis,
  analyzeSubCommand,

  -- * Helpers
  toProjectResult,
  applyFiltersToProject,
) where

import App.Docs (userGuideUrl)
import App.Fossa.Analyze.Debug (collectDebugBundle, diagToDebug)
import App.Fossa.Analyze.Discover (
  DiscoverFunc (..),
  discoverFuncs,
 )
import App.Fossa.Analyze.Filter (
  CountedResult (FilteredAll, FoundSome, NoneDiscovered),
  checkForEmptyUpload,
 )
import App.Fossa.Analyze.GraphMangler (graphingToGraph)
import App.Fossa.Analyze.Project (ProjectResult (..), mkResult)
import App.Fossa.Analyze.ScanSummary (renderScanSummary)
import App.Fossa.Analyze.Types (
  AnalysisScanResult (AnalysisScanResult),
  AnalyzeProject (..),
  AnalyzeTaskEffs,
  DiscoveredProjectIdentifier (..),
  DiscoveredProjectScan (..),
 )
import App.Fossa.Analyze.Upload (uploadSuccessfulAnalysis)
import App.Fossa.BinaryDeps (analyzeBinaryDeps)
import App.Fossa.Config.Analyze (
  AnalyzeCliOpts,
  AnalyzeConfig (..),
  BinaryDiscovery (BinaryDiscovery),
  DynamicLinkInspect (DynamicLinkInspect),
  ExperimentalAnalyzeConfig,
  IATAssertion (IATAssertion),
  IncludeAll (IncludeAll),
  NoDiscoveryExclusion (NoDiscoveryExclusion),
  ScanDestination (..),
  StandardAnalyzeConfig (severity),
  UnpackArchives (UnpackArchives),
 )
import App.Fossa.Config.Analyze qualified as Config
import App.Fossa.ManualDeps (analyzeFossaDepsFile)
import App.Fossa.Monorepo (monorepoScan)
import App.Fossa.Subcommand (SubCommand)
import App.Fossa.VSI.DynLinked (analyzeDynamicLinkedDeps)
import App.Fossa.VSI.IAT.AssertRevisionBinaries (assertRevisionBinaries)
import App.Fossa.VSI.Types qualified as VSI
import App.Fossa.VSIDeps (analyzeVSIDeps)
import App.Types (
  BaseDir (..),
  OverrideDynamicAnalysisBinary,
  ProjectRevision (..),
 )
import Codec.Compression.GZip qualified as GZip
import Control.Carrier.AtomicCounter (AtomicCounter, runAtomicCounter)
import Control.Carrier.Debug (Debug, debugMetadata, ignoreDebug)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.Finally (Finally, Has, runFinally)
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Carrier.Output.IO (Output, output, runOutput)
import Control.Carrier.Reader (Reader, runReader)
import Control.Carrier.Stack.StickyLog (stickyLogStack)
import Control.Carrier.StickyLogger (StickyLogger, logSticky', runStickyLogger)
import Control.Carrier.TaskPool (
  Progress (..),
  TaskPool,
  forkTask,
  withTaskPool,
 )
import Control.Concurrent (getNumCapabilities)
import Control.Effect.Diagnostics (recover)
import Control.Effect.Exception (Lift)
import Control.Effect.FossaApiClient (FossaApiClient, getEndpointVersion)
import Control.Effect.Git (Git)
import Control.Effect.Lift (sendIO)
import Control.Effect.Stack (Stack, withEmptyStack)
import Control.Effect.Telemetry (Telemetry, trackResult, trackTimeSpent)
import Control.Monad (join, unless, void, when)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Flag (Flag, fromFlag)
import Data.Foldable (traverse_)
import Data.Maybe (mapMaybe)
import Data.String.Conversion (decodeUtf8, toText)
import Data.Text.Extra (showT)
import Diag.Result (resultToMaybe)
import Discovery.Archive qualified as Archive
import Discovery.Filters (AllFilters, applyFilters, filterIsVSIOnly, ignoredPaths, isDefaultNonProductionPath)
import Discovery.Projects (withDiscoveredProjects)
import Effect.Exec (Exec)
import Effect.Logger (
  Logger,
  Severity (..),
  logInfo,
  logStdout,
 )
import Effect.ReadFS (ReadFS)
import Path (Abs, Dir, Path, toFilePath)
import Path.IO (makeRelative)
import Prettyprinter (
  Doc,
  Pretty (pretty),
  annotate,
  viaShow,
  vsep,
 )
import Prettyprinter.Render.Terminal (
  Color (Cyan, Green, Yellow),
  color,
 )
import Srclib.Converter qualified as Srclib
import Srclib.Types (Locator, SourceUnit)
import Types (DiscoveredProject (..), FoundTargets)

debugBundlePath :: FilePath
debugBundlePath = "fossa.debug.json.gz"

analyzeSubCommand :: SubCommand AnalyzeCliOpts AnalyzeConfig
analyzeSubCommand = Config.mkSubCommand dispatch

dispatch ::
  ( Has Diag.Diagnostics sig m
  , Has Exec sig m
  , Has Git sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Telemetry sig m
  ) =>
  AnalyzeConfig ->
  m ()
dispatch = \case
  Monorepo cfg -> monorepoScan cfg
  Standard cfg -> void $ analyzeMain cfg

-- This is just a handler for the Debug effect.
-- The real logic is in the inner analyze
analyzeMain ::
  ( Has Diag.Diagnostics sig m
  , Has Exec sig m
  , Has Git sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Telemetry sig m
  ) =>
  StandardAnalyzeConfig ->
  m Aeson.Value
analyzeMain cfg = case Config.severity cfg of
  SevDebug -> do
    (scope, res) <- collectDebugBundle cfg $ Diag.errorBoundaryIO $ analyze cfg
    sendIO . BL.writeFile debugBundlePath . GZip.compress $ Aeson.encode scope
    Diag.rethrow res
  _ -> ignoreDebug $ analyze cfg

runDependencyAnalysis ::
  ( AnalyzeProject proj
  , Aeson.ToJSON proj
  , Has (Lift IO) sig m
  , Has AtomicCounter sig m
  , Has Debug sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has (Output DiscoveredProjectScan) sig m
  , Has Stack sig m
  , Has (Reader ExperimentalAnalyzeConfig) sig m
  , Has (Reader AllFilters) sig m
  , Has (Reader OverrideDynamicAnalysisBinary) sig m
  , Has Telemetry sig m
  ) =>
  -- | Analysis base directory
  Path Abs Dir ->
  AllFilters ->
  DiscoveredProject proj ->
  m ()
runDependencyAnalysis basedir filters project@DiscoveredProject{..} = do
  let dpi = DiscoveredProjectIdentifier projectPath projectType
  let hasNonProductionPath = isDefaultNonProductionPath basedir projectPath

  case (applyFiltersToProject basedir filters project, hasNonProductionPath) of
    (Nothing, _) -> do
      logInfo $ "Skipping " <> pretty projectType <> " project at " <> viaShow projectPath <> ": no filters matched"
      output $ SkippedDueToProvidedFilter dpi
    (Just _, True) -> do
      logInfo $ "Skipping " <> pretty projectType <> " project at " <> viaShow projectPath <> " (default non-production path filtering)"
      output $ SkippedDueToDefaultProductionFilter dpi
    (Just targets, False) -> do
      logInfo $ "Analyzing " <> pretty projectType <> " project at " <> pretty (toFilePath projectPath)
      let ctxMessage = "Project Analysis: " <> showT projectType
      graphResult <- Diag.runDiagnosticsIO . diagToDebug . stickyLogStack . withEmptyStack . Diag.context ctxMessage $ do
        debugMetadata "DiscoveredProject" project
        trackTimeSpent (showT projectType) $ analyzeProject targets projectData
      Diag.flushLogs SevError SevDebug graphResult
      trackResult graphResult
      output $ Scanned dpi (mkResult basedir project <$> graphResult)

applyFiltersToProject :: Path Abs Dir -> AllFilters -> DiscoveredProject n -> Maybe FoundTargets
applyFiltersToProject basedir filters DiscoveredProject{..} =
  case makeRelative basedir projectPath of
    -- FIXME: this is required for --unpack-archives to continue to work.
    -- archives are not unpacked relative to the scan basedir, so "makeRelative"
    -- will always fail
    Nothing -> Just projectBuildTargets
    Just rel -> do
      applyFilters filters (toText projectType) rel projectBuildTargets

runAnalyzers ::
  ( AnalyzeTaskEffs sig m
  , Has (Output DiscoveredProjectScan) sig m
  , Has TaskPool sig m
  , Has AtomicCounter sig m
  ) =>
  Path Abs Dir ->
  AllFilters ->
  m ()
runAnalyzers basedir filters = do
  if filterIsVSIOnly filters
    then do
      logInfo "Running in VSI only mode, skipping other analyzers"
      pure ()
    else traverse_ single discoverFuncs
  where
    single (DiscoverFunc f) = withDiscoveredProjects f basedir (runDependencyAnalysis basedir filters)

analyze ::
  ( Has Debug sig m
  , Has Diag.Diagnostics sig m
  , Has Exec sig m
  , Has Git sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Telemetry sig m
  ) =>
  StandardAnalyzeConfig ->
  m Aeson.Value
analyze cfg = Diag.context "fossa-analyze" $ do
  capabilities <- sendIO getNumCapabilities

  let maybeApiOpts = case destination of
        OutputStdout -> Nothing
        UploadScan opts _ -> Just opts
      BaseDir basedir = Config.baseDir cfg
      destination = Config.scanDestination cfg
      filters = Config.filterSet cfg
      iatAssertion = Config.iatAssertion $ Config.vsiOptions cfg
      includeAll = Config.includeAllDeps cfg
      jsonOutput = Config.jsonOutput cfg
      noDiscoveryExclusion = Config.noDiscoveryExclusion cfg
      revision = Config.projectRevision cfg
      skipResolutionSet = Config.vsiSkipSet $ Config.vsiOptions cfg
      vendoredDepsOptions = Config.vendoredDeps cfg

  -- additional source units are built outside the standard strategy flow, because they either
  -- require additional information (eg API credentials), or they return additional information (eg user deps).
  vsiResults <- Diag.errorBoundaryIO . diagToDebug $
    Diag.context "analyze-vsi" . runStickyLogger SevInfo . runFinally $ do
      let shouldRunVSI = fromFlag Config.VSIAnalysis $ Config.vsiAnalysisEnabled $ Config.vsiOptions cfg
      case (shouldRunVSI, maybeApiOpts) of
        (True, Just apiOpts') ->
          runFossaApiClient apiOpts' $
            analyzeVSI basedir revision filters skipResolutionSet
        _ -> pure Nothing
  dynamicLinkedResults <-
    Diag.errorBoundaryIO
      . diagToDebug
      . runReader filters
      $ Diag.context "discover-dynamic-linking" . doAnalyzeDynamicLinkedBinary basedir . Config.dynamicLinkingTarget
      $ Config.vsiOptions cfg
  binarySearchResults <-
    Diag.errorBoundaryIO . diagToDebug $
      Diag.context "discover-binaries" $
        if (fromFlag BinaryDiscovery $ Config.binaryDiscoveryEnabled $ Config.vsiOptions cfg)
          then analyzeDiscoverBinaries basedir filters
          else pure Nothing
  manualSrcUnits <-
    Diag.errorBoundaryIO . diagToDebug $
      if filterIsVSIOnly filters
        then do
          logInfo "Running in VSI only mode, skipping manual source units"
          pure Nothing
        else Diag.context "fossa-deps" . runStickyLogger SevInfo $ analyzeFossaDepsFile basedir maybeApiOpts vendoredDepsOptions
  let additionalSourceUnits :: [SourceUnit]
      additionalSourceUnits = mapMaybe (join . resultToMaybe) [manualSrcUnits, vsiResults, binarySearchResults, dynamicLinkedResults]
  traverse_ (Diag.flushLogs SevError SevDebug) [vsiResults, binarySearchResults, manualSrcUnits, dynamicLinkedResults]

  let discoveryFilters = if fromFlag NoDiscoveryExclusion noDiscoveryExclusion then mempty else filters
  (projectScans, ()) <-
    Diag.context "discovery/analysis tasks"
      . runOutput @DiscoveredProjectScan
      . runStickyLogger SevInfo
      . runFinally
      . withTaskPool capabilities updateProgress
      . runAtomicCounter
      . runReader (Config.experimental cfg)
      . runReader discoveryFilters
      . runReader (Config.overrideDynamicAnalysis cfg)
      $ do
        runAnalyzers basedir filters
        when (fromFlag UnpackArchives $ Config.unpackArchives cfg) $
          forkTask $ do
            res <- Diag.runDiagnosticsIO . diagToDebug . stickyLogStack . withEmptyStack $ Archive.discover (`runAnalyzers` filters) basedir
            Diag.withResult SevError SevWarn res (const (pure ()))

  let projectResults = mapMaybe toProjectResult projectScans
  let filteredProjects = mapMaybe toProjectResult projectScans

  let analysisResult = AnalysisScanResult projectScans vsiResults binarySearchResults manualSrcUnits dynamicLinkedResults

  maybeEndpointAppVersion <- case destination of
    UploadScan apiOpts _ -> runFossaApiClient apiOpts $ do
      -- Using 'recovery' as API corresponding to 'getEndpointVersion',
      -- seems to be not stable and we sometimes see TimeoutError in telemetry
      version <- recover getEndpointVersion
      debugMetadata "FossaEndpointCoreVersion" version
      pure version
    _ -> pure Nothing

  renderScanSummary (severity cfg) maybeEndpointAppVersion analysisResult $ Config.filterSet cfg

  -- Need to check if vendored is empty as well, even if its a boolean that vendoredDeps exist
  let result = buildResult includeAll additionalSourceUnits filteredProjects
  case checkForEmptyUpload includeAll projectResults filteredProjects additionalSourceUnits of
    NoneDiscovered -> Diag.fatal ErrNoProjectsDiscovered
    FilteredAll -> Diag.fatal ErrFilteredAllProjects
    FoundSome sourceUnits -> case destination of
      OutputStdout -> logStdout . decodeUtf8 $ Aeson.encode result
      UploadScan apiOpts metadata ->
        Diag.context "upload-results"
          . runFossaApiClient apiOpts
          $ do
            locator <- uploadSuccessfulAnalysis (BaseDir basedir) metadata jsonOutput revision sourceUnits
            doAssertRevisionBinaries iatAssertion locator
  pure result

toProjectResult :: DiscoveredProjectScan -> Maybe ProjectResult
toProjectResult (SkippedDueToProvidedFilter _) = Nothing
toProjectResult (SkippedDueToDefaultProductionFilter _) = Nothing
toProjectResult (Scanned _ res) = resultToMaybe res

analyzeVSI ::
  ( Has Diag.Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has StickyLogger sig m
  , Has ReadFS sig m
  , Has Finally sig m
  , Has FossaApiClient sig m
  ) =>
  Path Abs Dir ->
  ProjectRevision ->
  AllFilters ->
  VSI.SkipResolution ->
  m (Maybe SourceUnit)
analyzeVSI dir revision filters skipResolving = do
  logInfo "Running VSI analysis"

  let skippedLocators = VSI.unVSISkipResolution skipResolving
  unless (null skippedLocators) $
    do
      logInfo "Skipping resolution of the following locators:"
      traverse_ (logInfo . pretty . VSI.renderLocator) skippedLocators

  results <- analyzeVSIDeps dir revision filters skipResolving
  pure $ Just results

analyzeDiscoverBinaries ::
  ( Has Diag.Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  ) =>
  Path Abs Dir ->
  AllFilters ->
  m (Maybe SourceUnit)
analyzeDiscoverBinaries dir filters = do
  if filterIsVSIOnly filters
    then do
      logInfo "Running in VSI only mode, skipping binary discovery"
      pure Nothing
    else do
      logInfo "Discovering binary files as dependencies"
      analyzeBinaryDeps dir filters

doAssertRevisionBinaries ::
  ( Has Diag.Diagnostics sig m
  , Has FossaApiClient sig m
  , Has ReadFS sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  IATAssertion ->
  Locator ->
  m ()
doAssertRevisionBinaries (IATAssertion (Just dir)) locator =
  assertRevisionBinaries dir locator
doAssertRevisionBinaries _ _ = pure ()

doAnalyzeDynamicLinkedBinary ::
  ( Has Diag.Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has (Reader AllFilters) sig m
  ) =>
  Path Abs Dir ->
  DynamicLinkInspect ->
  m (Maybe SourceUnit)
doAnalyzeDynamicLinkedBinary root (DynamicLinkInspect (Just target)) = analyzeDynamicLinkedDeps root target
doAnalyzeDynamicLinkedBinary _ _ = pure Nothing

data AnalyzeError
  = ErrNoProjectsDiscovered
  | ErrFilteredAllProjects

instance Diag.ToDiagnostic AnalyzeError where
  renderDiagnostic :: AnalyzeError -> Doc ann
  renderDiagnostic ErrNoProjectsDiscovered =
    vsep
      [ "No analysis targets found in directory."
      , ""
      , "Make sure your project is supported. See the user guide for details:"
      , "    " <> pretty userGuideUrl
      , ""
      ]
  renderDiagnostic (ErrFilteredAllProjects) =
    vsep
      [ "Filtered out all projects. This may be occurring because: "
      , ""
      , " * No manual or vendor dependencies were provided with `fossa-deps` file."
      , " * Exclusion filters were used, filtering out discovered projects. "
      , " * Discovered projects resided in following ignored path by default:"
      , vsep $ map (\i -> pretty $ "    * " <> toText i) ignoredPaths
      , ""
      , "See the user guide for details:"
      , "    " <> pretty userGuideUrl
      , ""
      ]

buildResult :: Flag IncludeAll -> [SourceUnit] -> [ProjectResult] -> Aeson.Value
buildResult includeAll srcUnits projects =
  Aeson.object
    [ "projects" .= map buildProject projects
    , "sourceUnits" .= finalSourceUnits
    ]
  where
    finalSourceUnits = srcUnits ++ scannedUnits
    scannedUnits = map (Srclib.toSourceUnit (fromFlag IncludeAll includeAll)) projects

buildProject :: ProjectResult -> Aeson.Value
buildProject project =
  Aeson.object
    [ "path" .= projectResultPath project
    , "type" .= projectResultType project
    , "graph" .= graphingToGraph (projectResultGraph project)
    ]

updateProgress :: Has StickyLogger sig m => Progress -> m ()
updateProgress Progress{..} =
  logSticky'
    ( "[ "
        <> annotate (color Cyan) (pretty pQueued)
        <> " Waiting / "
        <> annotate (color Yellow) (pretty pRunning)
        <> " Running / "
        <> annotate (color Green) (pretty pCompleted)
        <> " Completed"
        <> " ]"
    )
