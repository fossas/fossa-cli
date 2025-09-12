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
  CountedResult (..),
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
import App.Fossa.Analyze.Upload (ScanUnits (SourceUnitOnly), mergeSourceAndLicenseUnits, uploadSuccessfulAnalysis)
import App.Fossa.BinaryDeps (analyzeBinaryDeps)
import App.Fossa.Config.Analyze (
  AnalysisTacticTypes (..),
  AnalyzeCliOpts,
  AnalyzeConfig (..),
  BinaryDiscovery (BinaryDiscovery),
  DynamicLinkInspect (DynamicLinkInspect),
  ExperimentalAnalyzeConfig (..),
  IATAssertion (IATAssertion),
  IncludeAll (IncludeAll),
  NoDiscoveryExclusion (NoDiscoveryExclusion),
  ScanDestination (..),
  UnpackArchives (UnpackArchives),
  WithoutDefaultFilters (..),
 )
import App.Fossa.Config.Analyze qualified as Config
import App.Fossa.Config.Common (DestinationMeta (..), destinationApiOpts, destinationMetadata)
import App.Fossa.Ficus.Analyze (analyzeWithFicus)
import App.Fossa.FirstPartyScan (runFirstPartyScan)
import App.Fossa.Lernie.Analyze (analyzeWithLernie)
import App.Fossa.Lernie.Types (LernieResults (..))
import App.Fossa.ManualDeps (analyzeFossaDepsFile)
import App.Fossa.PathDependency (enrichPathDependencies, enrichPathDependencies', withPathDependencyNudge)
import App.Fossa.PreflightChecks (PreflightCommandChecks (AnalyzeChecks), preflightChecks)
import App.Fossa.Reachability.Upload (analyzeForReachability, onlyFoundUnits)
import App.Fossa.Subcommand (SubCommand)
import App.Fossa.VSI.DynLinked (analyzeDynamicLinkedDeps)
import App.Fossa.VSI.IAT.AssertRevisionBinaries (assertRevisionBinaries)
import App.Fossa.VSI.Types qualified as VSI
import App.Fossa.VSIDeps (analyzeVSIDeps)
import App.Types (
  BaseDir (..),
  FirstPartyScansFlag (..),
  Mode (..),
  OverrideDynamicAnalysisBinary,
  ProjectRevision (..),
 )
import App.Util (FileAncestry, ancestryDirect)
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
import Data.Error (createBody)
import Data.Flag (Flag, fromFlag)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String.Conversion (decodeUtf8, toText)
import Data.Text.Extra (showT)
import Data.Traversable (for)
import Diag.Diagnostic as DI
import Diag.Result (Result (Success), resultToMaybe)
import Discovery.Archive qualified as Archive
import Discovery.Filters (AllFilters, MavenScopeFilters, applyFilters, filterIsVSIOnly, ignoredPaths, isDefaultNonProductionPath)
import Discovery.Projects (withDiscoveredProjects)
import Effect.Exec (Exec)
import Effect.Logger (
  Logger,
  Severity (..),
  logDebug,
  logInfo,
  logStdout,
  renderIt,
 )
import Effect.ReadFS (ReadFS)
import Errata (Errata (..))
import Fossa.API.Types (Organization (Organization, orgSupportsReachability))
import Path (Abs, Dir, Path, toFilePath)
import Path.IO (makeRelative)
import Prettyprinter (
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
import Srclib.Types (LicenseSourceUnit (..), Locator, SourceUnit, sourceUnitToFullSourceUnit)
import Types (DiscoveredProject (..), FoundTargets)
import Control.Monad.IO.Unlift (MonadUnliftIO)

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
  , Has Telemetry sig m, MonadUnliftIO m
  ) =>
  AnalyzeConfig ->
  m ()
dispatch cfg = void $ analyzeMain cfg

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
  , MonadUnliftIO m
  ) =>
  AnalyzeConfig ->
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
  , Has (Reader MavenScopeFilters) sig m
  , Has (Reader Mode) sig m
  , Has (Reader AllFilters) sig m
  , Has (Reader OverrideDynamicAnalysisBinary) sig m
  , Has Telemetry sig m
  ) =>
  -- | Analysis base directory
  Path Abs Dir ->
  -- | Filters
  AllFilters ->
  Flag WithoutDefaultFilters ->
  -- | An optional path prefix to prepend to paths of discovered manifestFiles
  Maybe FileAncestry ->
  AnalysisTacticTypes ->
  -- | The project to analyze
  DiscoveredProject proj ->
  m ()
runDependencyAnalysis basedir filters withoutDefaultFilters pathPrefix allowedTactics project@DiscoveredProject{..} = do
  let dpi = DiscoveredProjectIdentifier projectPath projectType
  let hasNonProductionPath =
        not (fromFlag Config.WithoutDefaultFilters withoutDefaultFilters)
          && isDefaultNonProductionPath basedir projectPath

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
      graphResult <- Diag.runDiagnosticsIO . diagToDebug . stickyLogStack . withEmptyStack . Diag.context ctxMessage $ do
        debugMetadata "DiscoveredProject" project
        let analyzeFn = case allowedTactics of
              StaticOnly -> analyzeProjectStaticOnly
              Any -> analyzeProject
        trackTimeSpent (showT projectType) $ analyzeFn targets projectData
      Diag.flushLogs SevError SevDebug graphResult
      trackResult graphResult
      output $ Scanned dpi (mkResult basedir project pathPrefix <$> graphResult)

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
  AnalysisTacticTypes ->
  AllFilters ->
  Flag WithoutDefaultFilters ->
  Path Abs Dir ->
  Maybe FileAncestry ->
  m ()
runAnalyzers allowedTactics filters withoutDefaultFilters basedir pathPrefix = do
  if filterIsVSIOnly filters
    then do
      logInfo "Running in VSI only mode, skipping other analyzers"
      pure ()
    else traverse_ single discoverFuncs
  where
    single (DiscoverFunc f) = withDiscoveredProjects f basedir (runDependencyAnalysis basedir filters withoutDefaultFilters pathPrefix allowedTactics)

analyze ::
  ( Has Debug sig m
  , Has Diag.Diagnostics sig m
  , Has Exec sig m
  , Has Git sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Telemetry sig m
  , MonadUnliftIO m
  ) =>
  AnalyzeConfig ->
  m Aeson.Value
analyze cfg = Diag.context "fossa-analyze" $ do
  capabilities <- sendIO getNumCapabilities

  let maybeDestMeta = destinationMetadata destination
      maybeApiOpts = destinationApiOpts <$> maybeDestMeta
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
      grepOptions = Config.grepOptions cfg
      customFossaDepsFile = Config.customFossaDepsFile cfg
      shouldAnalyzePathDependencies = resolvePathDependencies $ Config.experimental cfg
      allowedTactics = Config.allowedTacticTypes cfg
      withoutDefaultFilters = Config.withoutDefaultFilters cfg
      enableSnippetScan = Config.xSnippetScan cfg

  manualSrcUnits <-
    Diag.errorBoundaryIO . diagToDebug $
      if filterIsVSIOnly filters
        then do
          logInfo "Running in VSI only mode, skipping manual source units"
          pure Nothing
        else Diag.context "fossa-deps" . runStickyLogger SevInfo $ analyzeFossaDepsFile basedir customFossaDepsFile maybeApiOpts vendoredDepsOptions

  orgInfo <-
    for
      maybeDestMeta
      ( \(DestinationMeta (apiOpts, metadata)) ->
          runFossaApiClient apiOpts . preflightChecks $ AnalyzeChecks revision metadata
      )

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
  maybeFicusResults <-
    Diag.errorBoundaryIO . diagToDebug $
      if not enableSnippetScan
        then do
          logInfo "Skipping ficus snippet scanning (--x-snippet-scan not set)"
          pure Nothing
        else
          if filterIsVSIOnly filters
            then do
              logInfo "Running in VSI only mode, skipping snippet-scan"
              pure Nothing
            else Diag.context "snippet-scanning" . runStickyLogger SevInfo $ analyzeWithFicus basedir maybeApiOpts revision $ Config.licenseScanPathFilters vendoredDepsOptions
  let ficusResults = join . resultToMaybe $ maybeFicusResults
  maybeLernieResults <-
    Diag.errorBoundaryIO . diagToDebug $
      if filterIsVSIOnly filters
        then do
          logInfo "Running in VSI only mode, skipping keyword search and custom-license search"
          pure Nothing
        else Diag.context "custom-license & keyword search" . runStickyLogger SevInfo $ analyzeWithLernie basedir maybeApiOpts grepOptions $ Config.licenseScanPathFilters vendoredDepsOptions
  let lernieResults = join . resultToMaybe $ maybeLernieResults

  let -- This makes nice with additionalSourceUnits below, but throws out additional Result data.
      -- This is ok because 'resultToMaybe' would do that anyway.
      -- We'll use the original results to output warnings/errors below.
      vsiResults' :: [SourceUnit]
      vsiResults' = fromMaybe [] $ join (resultToMaybe vsiResults)

      additionalSourceUnits :: [SourceUnit]
      additionalSourceUnits = vsiResults' <> mapMaybe (join . resultToMaybe) [manualSrcUnits, binarySearchResults, dynamicLinkedResults]
  traverse_ (Diag.flushLogs SevError SevDebug) [manualSrcUnits, binarySearchResults, dynamicLinkedResults]
  -- Flush logs using the original Result from VSI.
  traverse_ (Diag.flushLogs SevError SevDebug) [vsiResults]
  -- Flush logs from lernie
  traverse_ (Diag.flushLogs SevError SevDebug) [maybeLernieResults]

  maybeFirstPartyScanResults <-
    Diag.errorBoundaryIO . diagToDebug $
      if firstPartyScansFlag cfg == FirstPartyScansOffFromFlag
        then do
          logInfo "first party scans forced off by the --experimental-block-first-party-scans flag. Skipping first party scans"
          pure Nothing
        else Diag.context "first-party-scans" . runStickyLogger SevInfo $ runFirstPartyScan basedir maybeApiOpts cfg
  let firstPartyScanResults = join . resultToMaybe $ maybeFirstPartyScanResults
  let discoveryFilters = if fromFlag NoDiscoveryExclusion noDiscoveryExclusion then mempty else filters
  (projectScans, ()) <-
    Diag.context "discovery/analysis tasks"
      . runOutput @DiscoveredProjectScan
      . runStickyLogger SevInfo
      . runFinally
      . withTaskPool capabilities updateProgress
      . runAtomicCounter
      . runReader (Config.experimental cfg)
      . runReader (Config.mavenScopeFilterSet cfg)
      . runReader discoveryFilters
      . runReader (Config.overrideDynamicAnalysis cfg)
      . runReader (Config.mode cfg)
      $ do
        runAnalyzers allowedTactics filters withoutDefaultFilters basedir Nothing
        when (fromFlag UnpackArchives $ Config.unpackArchives cfg) $
          forkTask $ do
            res <- Diag.runDiagnosticsIO . diagToDebug . stickyLogStack . withEmptyStack $ Archive.discover (runAnalyzers allowedTactics filters withoutDefaultFilters) basedir ancestryDirect
            Diag.withResult SevError SevWarn res (const (pure ()))
  logDebug $ "Unfiltered project scans: " <> pretty (show projectScans)

  let filteredProjects = mapMaybe toProjectResult projectScans
  logDebug $ "Filtered project scans: " <> pretty (show filteredProjects)

  maybeEndpointAppVersion <- fmap join . for maybeApiOpts $
    \apiOpts -> runFossaApiClient apiOpts $ do
      -- Using 'recovery' as API corresponding to 'getEndpointVersion',
      -- seems to be not stable and we sometimes see TimeoutError in telemetry
      version <- recover getEndpointVersion
      debugMetadata "FossaEndpointCoreVersion" version
      pure version

  -- In our graph, we may have unresolved path dependencies
  -- If we are in output mode, do nothing. If we are in upload mode
  -- license scan all path dependencies, and upload findings to Endpoint,
  -- and queue a build for all path+ dependencies
  filteredProjects' <- case (shouldAnalyzePathDependencies, destinationMetadata destination) of
    (True, Just (DestinationMeta (apiOpts, _))) ->
      Diag.context "path-dependencies"
        . runFossaApiClient apiOpts
        $ runStickyLogger SevInfo
        $ traverse (enrichPathDependencies includeAll vendoredDepsOptions revision) filteredProjects
    (True, _) -> pure $ map enrichPathDependencies' filteredProjects
    (False, _) -> traverse (withPathDependencyNudge includeAll) filteredProjects
  logDebug $ "Filtered projects with path dependencies: " <> pretty (show filteredProjects')

  reachabilityUnitsResult <-
    case orgInfo of
      (Just (Organization{orgSupportsReachability = False})) -> pure []
      _ ->
        Diag.context "reachability analysis"
          . runReader (Config.reachabilityConfig cfg)
          . runReader filters
          $ analyzeForReachability projectScans
  let reachabilityUnits = onlyFoundUnits reachabilityUnitsResult

  let analysisResult = AnalysisScanResult projectScans vsiResults binarySearchResults (Success [] Nothing) manualSrcUnits dynamicLinkedResults maybeLernieResults reachabilityUnitsResult
  renderScanSummary (severity cfg) maybeEndpointAppVersion analysisResult cfg

  -- Need to check if vendored is empty as well, even if its a boolean that vendoredDeps exist
  let licenseSourceUnits =
        case (firstPartyScanResults, lernieResultsSourceUnit =<< lernieResults) of
          (Nothing, Nothing) -> Nothing
          (Just firstParty, Just lernie) -> Just $ firstParty <> lernie
          (Nothing, Just lernie) -> Just lernie
          (Just firstParty, Nothing) -> Just firstParty
  let keywordSearchResultsFound = (maybe False (not . null . lernieResultsKeywordSearches) lernieResults)
  let outputResult = buildResult includeAll additionalSourceUnits filteredProjects' licenseSourceUnits

  scanUnits <-
    case (keywordSearchResultsFound, checkForEmptyUpload includeAll projectScans filteredProjects' additionalSourceUnits licenseSourceUnits) of
      (False, NoneDiscovered) -> Diag.warn ErrNoProjectsDiscovered $> emptyScanUnits
      (True, NoneDiscovered) -> Diag.warn ErrOnlyKeywordSearchResultsFound $> emptyScanUnits
      (False, FilteredAll) -> Diag.warn ErrFilteredAllProjects $> emptyScanUnits
      (True, FilteredAll) -> Diag.warn ErrOnlyKeywordSearchResultsFound $> emptyScanUnits
      (_, CountedScanUnits scanUnits) -> pure scanUnits
  sendToDestination outputResult iatAssertion destination basedir jsonOutput revision scanUnits reachabilityUnits ficusResults

  pure outputResult
  where
    sendToDestination result iatAssertion destination basedir jsonOutput revision scanUnits reachabilityUnits ficusResults =
      let doUpload (DestinationMeta (apiOpts, metadata)) =
            Diag.context "upload-results"
              . runFossaApiClient apiOpts
              $ do
                locator <- uploadSuccessfulAnalysis (BaseDir basedir) metadata jsonOutput revision scanUnits reachabilityUnits ficusResults
                doAssertRevisionBinaries iatAssertion locator
       in case destination of
            OutputStdout -> logStdout . decodeUtf8 $ Aeson.encode result
            UploadScan meta -> doUpload meta
            OutputAndUpload meta -> do
              logStdout . decodeUtf8 $ Aeson.encode result
              doUpload meta

    emptyScanUnits :: ScanUnits
    emptyScanUnits = SourceUnitOnly []

toProjectResult :: DiscoveredProjectScan -> Maybe ProjectResult
toProjectResult (SkippedDueToProvidedFilter _) = Nothing
toProjectResult (SkippedDueToDefaultFilter _) = Nothing
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
  m (Maybe [SourceUnit])
analyzeVSI dir revision filters skipResolving = do
  logInfo "Running VSI analysis"

  let skippedLocators = VSI.unVSISkipResolution skipResolving
  unless (null skippedLocators) $
    do
      logInfo "Skipping resolution of the following locators:"
      traverse_ (logInfo . pretty . VSI.renderLocator) skippedLocators

  analyzeVSIDeps dir revision filters skipResolving

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
  | ErrOnlyKeywordSearchResultsFound

instance Diag.ToDiagnostic AnalyzeError where
  renderDiagnostic :: AnalyzeError -> Errata
  renderDiagnostic ErrNoProjectsDiscovered = do
    let help = "Refer to the provided documentation to ensure your project is supported"
    let body = createBody Nothing (Just userGuideUrl) Nothing (Just help) Nothing
    Errata (Just "No analysis targets found in directory") [] (Just body)
  renderDiagnostic ErrFilteredAllProjects = do
    let content =
          renderIt $
            vsep
              [ "This may be occurring because: "
              , ""
              , " * No manual or vendor dependencies were provided with `fossa-deps` file."
              , " * Exclusion filters were used, filtering out discovered projects. "
              , " * Discovered projects resided in following ignored path by default:"
              , vsep $ map (\i -> pretty $ "    * " <> toText i) ignoredPaths
              , ""
              ]
        body = createBody (Just content) (Just userGuideUrl) Nothing Nothing Nothing
    Errata (Just "Filtered out all projects") [] (Just body)
  renderDiagnostic ErrOnlyKeywordSearchResultsFound = do
    let body =
          renderIt $
            vsep
              [ "Matches to your keyword searches were found, but no other analysis targets were found."
              , "This error can be safely ignored if you are only expecting keyword search results."
              ]
    Errata (Just "Only keyword search results found") [] (Just body)

buildResult :: Flag IncludeAll -> [SourceUnit] -> [ProjectResult] -> Maybe LicenseSourceUnit -> Aeson.Value
buildResult includeAll srcUnits projects licenseSourceUnits =
  Aeson.object
    [ "projects" .= map buildProject projects
    , "sourceUnits" .= mergedUnits
    ]
  where
    mergedUnits = case licenseSourceUnits of
      Nothing -> map sourceUnitToFullSourceUnit finalSourceUnits
      Just licenseUnits -> do
        NE.toList $ mergeSourceAndLicenseUnits finalSourceUnits licenseUnits
    finalSourceUnits = srcUnits ++ scannedUnits
    scannedUnits = map (Srclib.projectToSourceUnit (fromFlag IncludeAll includeAll)) projects

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
