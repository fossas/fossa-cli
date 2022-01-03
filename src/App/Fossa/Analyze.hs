{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Analyze (
  analyzeMain,
  ScanDestination (..),
  UnpackArchives (..),
  JsonOutput (..),
  VSIAnalysisMode (..),
  IATAssertionMode (..),
  BinaryDiscoveryMode (..),
  ModeOptions (..),
  DiscoverFunc (..),
  discoverFuncs,
  updateProgress,
  IncludeAll (..),
  runAnalyzers,
  runDependencyAnalysis,
) where

import App.Docs (userGuideUrl)
import App.Fossa.API.BuildLink (getFossaBuildUrl)
import App.Fossa.Analyze.Debug (collectDebugBundle, diagToDebug)
import App.Fossa.Analyze.GraphMangler (graphingToGraph)
import App.Fossa.Analyze.Project (ProjectResult (..), mkResult)
import App.Fossa.Analyze.Types (
  AnalyzeExperimentalPreferences (..),
  AnalyzeProject (..),
  AnalyzeTaskEffs,
 )
import App.Fossa.BinaryDeps (analyzeBinaryDeps)
import App.Fossa.FossaAPIV1 (UploadResponse (..), getProject, projectIsMonorepo, uploadAnalysis, uploadContributors)
import App.Fossa.ManualDeps (analyzeFossaDepsFile)
import App.Fossa.ProjectInference (inferProjectDefault, inferProjectFromVCS, mergeOverride, saveRevision)
import App.Fossa.VSI.IAT.AssertRevisionBinaries (assertRevisionBinaries)
import App.Fossa.VSI.Types qualified as VSI
import App.Fossa.VSIDeps (analyzeVSIDeps)
import App.Types (
  BaseDir (..),
  OverrideProject,
  ProjectMetadata,
  ProjectRevision (projectBranch, projectName, projectRevision),
 )
import App.Util (validateDir)
import Codec.Compression.GZip qualified as GZip
import Control.Carrier.AtomicCounter (AtomicCounter, runAtomicCounter)
import Control.Carrier.Debug (Debug, debugMetadata, debugScope, ignoreDebug)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.Diagnostics.StickyContext (stickyDiag)
import Control.Carrier.Finally (Has, runFinally)
import Control.Carrier.Output.IO (Output, output, runOutput)
import Control.Carrier.Reader (Reader, runReader)
import Control.Carrier.StickyLogger (StickyLogger, logSticky', runStickyLogger)
import Control.Carrier.TaskPool (
  Progress (..),
  TaskPool,
  forkTask,
  withTaskPool,
 )
import Control.Concurrent (getNumCapabilities)
import Control.Effect.Diagnostics (Diagnostics, fatalText, fromMaybeText, recover, (<||>))
import Control.Effect.Exception (Lift)
import Control.Effect.Lift (sendIO)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Flag (Flag, fromFlag)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.List (isInfixOf, stripPrefix)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes, fromMaybe)
import Data.String.Conversion (decodeUtf8)
import Data.Text (Text)
import Discovery.Archive qualified as Archive
import Discovery.Filters (AllFilters, applyFilters, filterIsVSIOnly)
import Discovery.Projects (withDiscoveredProjects)
import Effect.Exec (Exec, runExecIO)
import Effect.Logger (
  Logger,
  Severity (..),
  logDebug,
  logError,
  logInfo,
  logStdout,
  withDefaultLogger,
 )
import Effect.ReadFS (ReadFS, runReadFSIO)
import Fossa.API.Types (ApiOpts (..))
import Path (Abs, Dir, Path, fromAbsDir, toFilePath)
import Path.IO (makeRelative)
import Prettyprinter (
  Doc,
  Pretty (pretty),
  annotate,
  line,
  viaShow,
  vsep,
 )
import Prettyprinter.Render.Terminal (
  Color (Cyan, Green, Yellow),
  color,
 )
import Srclib.Converter qualified as Srclib
import Srclib.Types (Locator (locatorProject, locatorRevision), SourceUnit, parseLocator)
import Strategy.Bundler qualified as Bundler
import Strategy.Cargo qualified as Cargo
import Strategy.Carthage qualified as Carthage
import Strategy.Cocoapods qualified as Cocoapods
import Strategy.Composer qualified as Composer
import Strategy.Conda qualified as Conda
import Strategy.Fpm qualified as Fpm
import Strategy.Glide qualified as Glide
import Strategy.Godep qualified as Godep
import Strategy.Gomodules qualified as Gomodules
import Strategy.Googlesource.RepoManifest qualified as RepoManifest
import Strategy.Gradle qualified as Gradle
import Strategy.Haskell.Cabal qualified as Cabal
import Strategy.Haskell.Stack qualified as Stack
import Strategy.Leiningen qualified as Leiningen
import Strategy.Maven qualified as Maven
import Strategy.Mix qualified as Mix
import Strategy.Nim qualified as Nim
import Strategy.Node qualified as Node
import Strategy.NuGet.Nuspec qualified as Nuspec
import Strategy.NuGet.PackageReference qualified as PackageReference
import Strategy.NuGet.PackagesConfig qualified as PackagesConfig
import Strategy.NuGet.Paket qualified as Paket
import Strategy.NuGet.ProjectAssetsJson qualified as ProjectAssetsJson
import Strategy.NuGet.ProjectJson qualified as ProjectJson
import Strategy.Perl qualified as Perl
import Strategy.Pub qualified as Pub
import Strategy.Python.Pipenv qualified as Pipenv
import Strategy.Python.Poetry qualified as Poetry
import Strategy.Python.Setuptools qualified as Setuptools
import Strategy.RPM qualified as RPM
import Strategy.Rebar3 qualified as Rebar3
import Strategy.Scala qualified as Scala
import Strategy.SwiftPM qualified as SwiftPM
import Types (DiscoveredProject (..), FoundTargets)
import VCS.Git (fetchGitContributors)

data ScanDestination
  = -- | upload to fossa with provided api key and base url
    UploadScan ApiOpts ProjectMetadata
  | OutputStdout

-- CLI flags, for use with 'Data.Flag'
data UnpackArchives = UnpackArchives
data IncludeAll = IncludeAll
data JsonOutput = JsonOutput

-- | Collect analysis modes into a single type for ease of use.
-- These modes are intended to be different options that alter how analysis is performed or what analysis steps are followed.
data ModeOptions = ModeOptions
  { modeVSIAnalysis :: VSIAnalysisMode
  , modeVSISkipResolution :: VSI.SkipResolution
  , modeIATAssertion :: IATAssertionMode
  , modeBinaryDiscovery :: BinaryDiscoveryMode
  }

-- | "VSI analysis" modes
data VSIAnalysisMode
  = -- | enable the VSI analysis strategy
    VSIAnalysisEnabled
  | -- | disable the VSI analysis strategy
    VSIAnalysisDisabled

-- | "IAT Assertion" modes
data IATAssertionMode
  = -- | assertion enabled, reading binaries from this directory
    IATAssertionEnabled (Path Abs Dir)
  | -- | assertion not enabled
    IATAssertionDisabled

-- | "Binary Discovery" modes
data BinaryDiscoveryMode
  = -- | Binary discovery enabled
    BinaryDiscoveryEnabled
  | -- | Binary discovery disabled
    BinaryDiscoveryDisabled

analyzeMain ::
  FilePath ->
  Severity ->
  ScanDestination ->
  OverrideProject ->
  Flag UnpackArchives ->
  Flag JsonOutput ->
  Flag IncludeAll ->
  ModeOptions ->
  AllFilters ->
  AnalyzeExperimentalPreferences ->
  IO ()
analyzeMain workdir logSeverity destination project unpackArchives jsonOutput includeAll modeOptions filters preferences =
  withDefaultLogger logSeverity
    . Diag.logWithExit_
    . runReadFSIO
    . runReader preferences
    . runExecIO
    $ case logSeverity of
      -- In --debug mode, emit a debug bundle to "fossa.debug.json"
      SevDebug -> do
        basedir <- sendIO $ validateDir workdir
        (scope, res) <- collectDebugBundle . Diag.errorBoundaryIO $ doAnalyze basedir
        sendIO . BL.writeFile "fossa.debug.json.gz" . GZip.compress $ Aeson.encode scope
        either Diag.rethrow pure res
      _ -> do
        basedir <- sendIO $ validateDir workdir
        ignoreDebug $ doAnalyze basedir
  where
    doAnalyze basedir = analyze basedir destination project unpackArchives jsonOutput includeAll modeOptions filters

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
  , Has (Reader AnalyzeExperimentalPreferences) sig m
  , MonadIO m
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
      graphResult <- Diag.runDiagnosticsIO . diagToDebug . stickyDiag . Diag.context "Project Analysis" $ do
        debugMetadata "DiscoveredProject" project
        analyzeProject targets (projectData project)
      Diag.withResult SevWarn graphResult (output . mkResult basedir project)

applyFiltersToProject :: Path Abs Dir -> AllFilters -> DiscoveredProject n -> Maybe FoundTargets
applyFiltersToProject basedir filters DiscoveredProject{..} =
  case makeRelative basedir projectPath of
    -- FIXME: this is required for --unpack-archives to continue to work.
    -- archives are not unpacked relative to the scan basedir, so "makeRelative"
    -- will always fail
    Nothing -> Just projectBuildTargets
    Just rel -> do
      applyFilters filters projectType rel projectBuildTargets

runAnalyzers ::
  ( AnalyzeTaskEffs sig m
  , Has (Output ProjectResult) sig m
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

discoverFuncs :: AnalyzeTaskEffs sig m => [DiscoverFunc m]
discoverFuncs =
  [ DiscoverFunc Bundler.discover
  , DiscoverFunc Cabal.discover
  , DiscoverFunc Cargo.discover
  , DiscoverFunc Carthage.discover
  , DiscoverFunc Cocoapods.discover
  , DiscoverFunc Composer.discover
  , DiscoverFunc Conda.discover
  , DiscoverFunc Fpm.discover
  , DiscoverFunc Glide.discover
  , DiscoverFunc Godep.discover
  , DiscoverFunc Gomodules.discover
  , DiscoverFunc Gradle.discover
  , DiscoverFunc Leiningen.discover
  , DiscoverFunc Maven.discover
  , DiscoverFunc Mix.discover
  , DiscoverFunc Nim.discover
  , DiscoverFunc Node.discover
  , DiscoverFunc Nuspec.discover
  , DiscoverFunc PackageReference.discover
  , DiscoverFunc PackagesConfig.discover
  , DiscoverFunc Paket.discover
  , DiscoverFunc Perl.discover
  , DiscoverFunc Pipenv.discover
  , DiscoverFunc Poetry.discover
  , DiscoverFunc ProjectAssetsJson.discover
  , DiscoverFunc ProjectJson.discover
  , DiscoverFunc Pub.discover
  , DiscoverFunc RPM.discover
  , DiscoverFunc Rebar3.discover
  , DiscoverFunc RepoManifest.discover
  , DiscoverFunc Scala.discover
  , DiscoverFunc Setuptools.discover
  , DiscoverFunc Stack.discover
  , DiscoverFunc SwiftPM.discover
  ]

-- DiscoverFunc is a workaround for the lack of impredicative types.
--
-- @discoverFuncs@ is a heterogenous list of discover functions that produce
-- different types of projects we can analyze for dependencies.
--
-- This GADT allows us to say that we don't care about the specific type of
-- projects produced by a discover function; we only care that each project type
-- implements ToJSON and AnalyzeProject
--
-- With impredicative types, we could shift the @forall@ inside the list,
-- avoiding the need for this GADT
--
--     discoverFuncs ::
--       AnalyzeTaskEffs sig m =>
--       [forall a. (AnalyzeProject a, ToJSON a) =>
--          Path Abs Dir -> m [DiscoveredProject a]]
data DiscoverFunc m where
  DiscoverFunc :: (AnalyzeProject a, Aeson.ToJSON a) => (Path Abs Dir -> m [DiscoveredProject a]) -> DiscoverFunc m

analyze ::
  ( Has (Lift IO) sig m
  , Has Logger sig m
  , Has Diag.Diagnostics sig m
  , Has Debug sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has (Reader AnalyzeExperimentalPreferences) sig m
  , MonadIO m
  ) =>
  BaseDir ->
  ScanDestination ->
  OverrideProject ->
  Flag UnpackArchives ->
  Flag JsonOutput ->
  Flag IncludeAll ->
  ModeOptions ->
  AllFilters ->
  m ()
analyze (BaseDir basedir) destination override unpackArchives jsonOutput includeAll ModeOptions{..} filters = Diag.context "fossa-analyze" $ do
  capabilities <- sendIO getNumCapabilities

  let apiOpts = case destination of
        OutputStdout -> Nothing
        UploadScan opts _ -> Just opts

  -- additional source units are built outside the standard strategy flow, because they either
  -- require additional information (eg API credentials), or they return additional information (eg user deps).
  vsiResults <- Diag.context "analyze-vsi" . runStickyLogger SevInfo $ analyzeVSI modeVSIAnalysis apiOpts basedir override filters modeVSISkipResolution
  binarySearchResults <- Diag.context "discover-binaries" $ analyzeDiscoverBinaries modeBinaryDiscovery basedir filters
  manualSrcUnits <-
    if filterIsVSIOnly filters
      then do
        logInfo "Running in VSI only mode, skipping manual source units"
        pure Nothing
      else Diag.context "fossa-deps" . runStickyLogger SevInfo $ analyzeFossaDepsFile basedir apiOpts
  let additionalSourceUnits :: [SourceUnit]
      additionalSourceUnits = catMaybes [manualSrcUnits, vsiResults, binarySearchResults]

  (projectResults, ()) <-
    Diag.context "discovery/analysis tasks"
      . runOutput @ProjectResult
      . runStickyLogger SevInfo
      . runFinally
      . withTaskPool capabilities updateProgress
      . runAtomicCounter
      $ do
        runAnalyzers basedir filters
        when (fromFlag UnpackArchives unpackArchives) $
          forkTask $ do
            res <- Diag.runDiagnosticsIO . diagToDebug . stickyDiag $ Archive.discover (`runAnalyzers` filters) basedir
            Diag.withResult SevError res (const (pure ()))

  let filteredProjects = filterProjects (BaseDir basedir) projectResults

  -- Need to check if vendored is empty as well, even if its a boolean that vendoredDeps exist
  case checkForEmptyUpload includeAll projectResults filteredProjects additionalSourceUnits of
    NoneDiscovered -> Diag.fatal ErrNoProjectsDiscovered
    FilteredAll count -> Diag.fatal (ErrFilteredAllProjects count projectResults)
    FoundSome sourceUnits -> case destination of
      OutputStdout -> do
        debugScope "DEBUG: Project inference" $ do
          inferred <- Diag.context "Inferring project name/revision" (inferProjectFromVCS basedir <||> inferProjectDefault basedir)
          logDebug $ "Inferred revision: " <> viaShow inferred

          let revision = mergeOverride override inferred
          logDebug $ "Merged revision: " <> viaShow revision
        logStdout . decodeUtf8 . Aeson.encode $ buildResult includeAll additionalSourceUnits filteredProjects
      UploadScan opts metadata -> Diag.context "upload-results" $ do
        locator <- uploadSuccessfulAnalysis (BaseDir basedir) opts metadata jsonOutput override sourceUnits
        doAssertRevisionBinaries modeIATAssertion opts locator

analyzeVSI :: (MonadIO m, Has Diag.Diagnostics sig m, Has Exec sig m, Has (Lift IO) sig m, Has Logger sig m, Has StickyLogger sig m, Has ReadFS sig m) => VSIAnalysisMode -> Maybe ApiOpts -> Path Abs Dir -> OverrideProject -> AllFilters -> VSI.SkipResolution -> m (Maybe SourceUnit)
analyzeVSI VSIAnalysisEnabled (Just apiOpts) dir override filters skipResolving = do
  logInfo "Running VSI analysis"

  let skippedLocators = VSI.unVSISkipResolution skipResolving
  if not $ null skippedLocators
    then do
      logInfo "Skipping resolution of the following locators:"
      traverse_ (logInfo . pretty . VSI.renderLocator) skippedLocators
    else pure ()

  revision <- inferProjectRevision dir override
  results <- analyzeVSIDeps dir revision apiOpts filters skipResolving
  pure $ Just results
analyzeVSI _ _ _ _ _ _ = pure Nothing

analyzeDiscoverBinaries :: (MonadIO m, Has Diag.Diagnostics sig m, Has (Lift IO) sig m, Has Logger sig m, Has ReadFS sig m) => BinaryDiscoveryMode -> Path Abs Dir -> AllFilters -> m (Maybe SourceUnit)
analyzeDiscoverBinaries BinaryDiscoveryEnabled dir filters = do
  if filterIsVSIOnly filters
    then do
      logInfo "Running in VSI only mode, skipping binary discovery"
      pure Nothing
    else do
      logInfo "Discovering binary files as dependencies"
      analyzeBinaryDeps dir filters
analyzeDiscoverBinaries _ _ _ = pure Nothing

doAssertRevisionBinaries :: (Has Diag.Diagnostics sig m, Has ReadFS sig m, Has (Lift IO) sig m, Has Logger sig m) => IATAssertionMode -> ApiOpts -> Locator -> m ()
doAssertRevisionBinaries (IATAssertionEnabled dir) apiOpts locator = assertRevisionBinaries dir apiOpts locator
doAssertRevisionBinaries _ _ _ = pure ()

dieOnMonorepoUpload :: (Has Diag.Diagnostics sig m, Has (Lift IO) sig m) => ApiOpts -> ProjectRevision -> m ()
dieOnMonorepoUpload apiOpts revision = do
  project <- recover $ getProject apiOpts revision
  if maybe False projectIsMonorepo project
    then fatalText "This project already exists as a monorepo project. Perhaps you meant to supply '--experimental-enable-monorepo', or meant to run 'fossa vps analyze' instead?"
    else pure ()

data AnalyzeError
  = ErrNoProjectsDiscovered
  | ErrFilteredAllProjects Int [ProjectResult]

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
  renderDiagnostic (ErrFilteredAllProjects count projectResults) =
    "Filtered out all "
      <> pretty count
      <> " projects due to directory name, and no manual deps were found."
      <> line
      <> line
      <> vsep (map renderExcludedProject projectResults)
    where
      renderExcludedProject :: ProjectResult -> Doc ann
      renderExcludedProject project = "Excluded by directory name: " <> pretty (toFilePath $ projectResultPath project)

uploadSuccessfulAnalysis ::
  ( Has Diag.Diagnostics sig m
  , Has Logger sig m
  , Has (Lift IO) sig m
  , Has Exec sig m
  , Has ReadFS sig m
  ) =>
  BaseDir ->
  ApiOpts ->
  ProjectMetadata ->
  Flag JsonOutput ->
  OverrideProject ->
  NE.NonEmpty SourceUnit ->
  m Locator
uploadSuccessfulAnalysis (BaseDir basedir) apiOpts metadata jsonOutput override units = Diag.context "Uploading analysis" $ do
  revision <- inferProjectRevision basedir override
  logDebug $ "Merged revision: " <> viaShow revision

  logInfo ""
  logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
  logInfo ("Using revision: `" <> pretty (projectRevision revision) <> "`")
  let branchText = fromMaybe "No branch (detached HEAD)" $ projectBranch revision
  logInfo ("Using branch: `" <> pretty branchText <> "`")

  dieOnMonorepoUpload apiOpts revision
  saveRevision revision

  uploadResult <- uploadAnalysis apiOpts revision metadata units
  let locator = parseLocator $ uploadLocator uploadResult
  buildUrl <- getFossaBuildUrl revision apiOpts locator
  logInfo $
    vsep
      [ "============================================================"
      , ""
      , "    View FOSSA Report:"
      , "    " <> pretty buildUrl
      , ""
      , "============================================================"
      ]
  traverse_ (\err -> logError $ "FOSSA error: " <> viaShow err) (uploadError uploadResult)
  -- Warn on contributor errors, never fail
  void . Diag.recover . runExecIO $ tryUploadContributors basedir apiOpts (uploadLocator uploadResult)

  if fromFlag JsonOutput jsonOutput
    then do
      summary <-
        Diag.context "Analysis ran successfully, but the server returned invalid metadata" $
          buildProjectSummary revision (uploadLocator uploadResult) buildUrl
      logStdout . decodeUtf8 $ Aeson.encode summary
    else pure ()

  pure locator

inferProjectRevision :: (Has (Lift IO) sig m, Has ReadFS sig m, Has Exec sig m, Has Diagnostics sig m, Has Logger sig m) => Path Abs Dir -> OverrideProject -> m ProjectRevision
inferProjectRevision basedir override = do
  inferred <- Diag.context "Inferring project name/revision" (inferProjectFromVCS basedir <||> inferProjectDefault basedir)
  logDebug $ "Inferred revision: " <> viaShow inferred
  pure $ mergeOverride override inferred

data CountedResult
  = NoneDiscovered
  | FilteredAll Int
  | FoundSome (NE.NonEmpty SourceUnit)

-- | Return some state of the projects found, since we can't upload empty result arrays.
-- Takes a list of all projects analyzed, and the list after filtering.  We assume
-- that the smaller list is the latter, and return that list.  Starting with user-defined deps,
-- we also include a check for an additional source unit from fossa-deps.yml.
checkForEmptyUpload :: Flag IncludeAll -> [ProjectResult] -> [ProjectResult] -> [SourceUnit] -> CountedResult
checkForEmptyUpload includeAll xs ys additionalUnits = do
  if null additionalUnits
    then case (xlen, ylen) of
      -- We didn't discover, so we also didn't filter
      (0, 0) -> NoneDiscovered
      -- If either list is empty, we have nothing to upload
      (0, _) -> FilteredAll filterCount
      (_, 0) -> FilteredAll filterCount
      -- NE.fromList is a partial, but is safe since we confirm the length is > 0.
      _ -> FoundSome $ NE.fromList discoveredUnits
    else -- If we have a additional source units, then there's always something to upload.
      FoundSome $ NE.fromList (additionalUnits ++ discoveredUnits)
  where
    xlen = length xs
    ylen = length ys
    filterCount = abs $ xlen - ylen
    -- The smaller list is the post-filter list, since filtering cannot add projects
    filtered = if xlen > ylen then ys else xs
    discoveredUnits = map (Srclib.toSourceUnit (fromFlag IncludeAll includeAll)) filtered

-- For each of the projects, we need to strip the root directory path from the prefix of the project path.
-- We don't want parent directories of the scan root affecting "production path" filtering -- e.g., if we're
-- running in a directory called "tmp", we still want results.
filterProjects :: BaseDir -> [ProjectResult] -> [ProjectResult]
filterProjects rootDir = filter (isProductionPath . dropPrefix rootPath . fromAbsDir . projectResultPath)
  where
    rootPath = fromAbsDir $ unBaseDir rootDir
    dropPrefix :: String -> String -> String
    dropPrefix prefix str = fromMaybe prefix (stripPrefix prefix str)

isProductionPath :: FilePath -> Bool
isProductionPath path =
  not $
    any
      (`isInfixOf` path)
      [ "doc/"
      , "docs/"
      , "test/"
      , "example/"
      , "examples/"
      , "vendor/"
      , "node_modules/"
      , ".srclib-cache/"
      , "spec/"
      , "Godeps/"
      , ".git/"
      , "bower_components/"
      , "third_party/"
      , "third-party/"
      , "Carthage/"
      , "Checkouts/"
      ]

tryUploadContributors ::
  ( Has Diag.Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  ) =>
  Path Abs Dir ->
  ApiOpts ->
  -- | Locator
  Text ->
  m ()
tryUploadContributors baseDir apiOpts locator = do
  contributors <- fetchGitContributors baseDir
  uploadContributors apiOpts locator contributors

-- | Build project summary JSON to be output to stdout
buildProjectSummary :: Has Diag.Diagnostics sig m => ProjectRevision -> Text -> Text -> m Aeson.Value
buildProjectSummary project projectLocator projectUrl = do
  let locator = parseLocator projectLocator
  revision <- fromMaybeText "Server returned an invalid project revision" $ locatorRevision locator
  pure $
    Aeson.object
      [ "project" .= locatorProject locator
      , "revision" .= revision
      , "branch" .= projectBranch project
      , "url" .= projectUrl
      , "id" .= projectLocator
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
