{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Analyze (
  analyzeMain,
  ScanDestination (..),
  UnpackArchives (..),
  JsonOutput (..),
  VSIAnalysisMode (..),
  IATAssertionMode (..),
  discoverFuncs,
  RecordMode (..),
) where

import App.Docs (userGuideUrl)
import App.Fossa.API.BuildLink (getFossaBuildUrl)
import App.Fossa.Analyze.GraphMangler (graphingToGraph)
import App.Fossa.Analyze.Project (ProjectResult (..), mkResult)
import App.Fossa.Analyze.Record (AnalyzeEffects (..), AnalyzeJournal (..), loadReplayLog, saveReplayLog)
import App.Fossa.FossaAPIV1 (UploadResponse (..), uploadAnalysis, uploadContributors)
import App.Fossa.ManualDeps (analyzeFossaDepsFile)
import App.Fossa.ProjectInference (inferProjectDefault, inferProjectFromVCS, mergeOverride, saveRevision)
import App.Fossa.VSI.IAT.AssertRevisionBinaries (assertRevisionBinaries)
import App.Fossa.VSIDeps (analyzeVSIDeps)
import App.Types (
  BaseDir (..),
  OverrideProject,
  ProjectMetadata,
  ProjectRevision (projectBranch, projectName, projectRevision),
 )
import App.Util (validateDir)
import Control.Carrier.AtomicCounter (AtomicCounter, runAtomicCounter)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.Diagnostics.StickyContext
import Control.Carrier.Finally
import Control.Carrier.Output.IO
import Control.Carrier.StickyLogger (StickyLogger, logSticky', runStickyLogger)
import Control.Carrier.TaskPool
import Control.Concurrent
import Control.Effect.Diagnostics ((<||>))
import Control.Effect.Exception (Lift, SomeException, throwIO, try)
import Control.Effect.Lift (sendIO)
import Control.Effect.Record (runRecord)
import Control.Effect.Replay (runReplay)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Flag (Flag, fromFlag)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.List (isInfixOf, stripPrefix)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes, fromMaybe)
import Data.String.Conversion (decodeUtf8)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Discovery.Filters
import Discovery.Projects (withDiscoveredProjects)
import Effect.Exec (Exec, runExecIO)
import Effect.Logger
import Effect.ReadFS (ReadFS, runReadFSIO)
import Fossa.API.Types (ApiOpts (..))
import Path (Abs, Dir, Path, fromAbsDir, toFilePath)
import Path.IO (makeRelative)
import Path.IO qualified as P
import Srclib.Converter qualified as Srclib
import Srclib.Types (Locator, SourceUnit, parseLocator)
import Strategy.Bundler qualified as Bundler
import Strategy.Cargo qualified as Cargo
import Strategy.Carthage qualified as Carthage
import Strategy.Cocoapods qualified as Cocoapods
import Strategy.Composer qualified as Composer
import Strategy.Conda qualified as Conda
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
import Strategy.Npm qualified as Npm
import Strategy.NuGet.Nuspec qualified as Nuspec
import Strategy.NuGet.PackageReference qualified as PackageReference
import Strategy.NuGet.PackagesConfig qualified as PackagesConfig
import Strategy.NuGet.Paket qualified as Paket
import Strategy.NuGet.ProjectAssetsJson qualified as ProjectAssetsJson
import Strategy.NuGet.ProjectJson qualified as ProjectJson
import Strategy.Pub qualified as Pub
import Strategy.Python.Pipenv qualified as Pipenv
import Strategy.Python.Poetry qualified as Poetry
import Strategy.Python.Setuptools qualified as Setuptools
import Strategy.RPM qualified as RPM
import Strategy.Rebar3 qualified as Rebar3
import Strategy.Scala qualified as Scala
import Strategy.Yarn qualified as Yarn
import System.Exit (die)
import Types (DiscoveredProject (..), FoundTargets)
import VCS.Git (fetchGitContributors)

type TaskEffs sig m =
  ( Has (Lift IO) sig m
  , MonadIO m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has Logger sig m
  , Has Diag.Diagnostics sig m
  )

data ScanDestination
  = -- | upload to fossa with provided api key and base url
    UploadScan ApiOpts ProjectMetadata
  | OutputStdout

-- | UnpackArchives bool flag
data UnpackArchives = UnpackArchives

data JsonOutput = JsonOutput

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

-- | "Replay logging" modes
data RecordMode
  = -- | record effect invocations
    RecordModeRecord
  | -- | replay effect invocations from a file
    RecordModeReplay FilePath
  | -- | don't record or replay
    RecordModeNone

analyzeMain :: FilePath -> RecordMode -> Severity -> ScanDestination -> OverrideProject -> Flag UnpackArchives -> Flag JsonOutput -> VSIAnalysisMode -> IATAssertionMode -> AllFilters -> IO ()
analyzeMain workdir recordMode logSeverity destination project unpackArchives jsonOutput enableVSI assertionMode filters =
  withDefaultLogger logSeverity
    . Diag.logWithExit_
    . runReadFSIO
    . runExecIO
    $ case recordMode of
      RecordModeNone -> do
        basedir <- sendIO $ validateDir workdir
        doAnalyze basedir
      RecordModeRecord -> do
        basedir <- sendIO $ validateDir workdir
        (execLogs, (readFSLogs, res)) <-
          runRecord @Exec . runRecord @ReadFS . try @SomeException $
            doAnalyze basedir
        sendIO $ saveReplayLog readFSLogs execLogs "fossa.debug.json"
        either throwIO pure res
      RecordModeReplay file -> do
        basedir <- BaseDir <$> P.resolveDir' workdir
        maybeJournal <- sendIO $ loadReplayLog file
        case maybeJournal of
          Left err -> sendIO (die $ "Issue loading replay log: " <> err)
          Right journal -> do
            let effects = analyzeEffects journal
            runReplay @ReadFS (effectsReadFS effects)
              . runReplay @Exec (effectsExec effects)
              $ doAnalyze basedir
  where
    doAnalyze basedir = analyze basedir destination project unpackArchives jsonOutput enableVSI assertionMode filters

discoverFuncs :: (TaskEffs sig m, TaskEffs rsig run) => [Path Abs Dir -> m [DiscoveredProject run]]
discoverFuncs =
  [ Bundler.discover
  , Cabal.discover
  , Cargo.discover
  , Carthage.discover
  , Cocoapods.discover
  , Composer.discover
  , Conda.discover
  , Glide.discover
  , Godep.discover
  , Gomodules.discover
  , Gradle.discover
  , Leiningen.discover
  , Maven.discover
  , Mix.discover
  , Npm.discover
  , Nuspec.discover
  , PackageReference.discover
  , PackagesConfig.discover
  , Paket.discover
  , Pipenv.discover
  , Poetry.discover
  , ProjectAssetsJson.discover
  , ProjectJson.discover
  , Pub.discover
  , RPM.discover
  , Rebar3.discover
  , RepoManifest.discover
  , Scala.discover
  , Setuptools.discover
  , Stack.discover
  , Yarn.discover
  ]

runDependencyAnalysis ::
  (Has (Lift IO) sig m, Has AtomicCounter sig m, Has Logger sig m, Has (Output ProjectResult) sig m) =>
  -- | Analysis base directory
  BaseDir ->
  AllFilters ->
  DiscoveredProject (StickyDiagC (Diag.DiagnosticsC m)) ->
  m ()
runDependencyAnalysis (BaseDir basedir) filters project =
  case applyFiltersToProject basedir filters project of
    Nothing -> logInfo $ "Skipping " <> pretty (projectType project) <> " project at " <> viaShow (projectPath project) <> ": no filters matched"
    Just targets -> do
      logInfo $ "Analyzing " <> pretty (projectType project) <> " project at " <> pretty (toFilePath (projectPath project))
      graphResult <- Diag.runDiagnosticsIO . stickyDiag $ projectDependencyResults project targets
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

analyze ::
  ( Has (Lift IO) sig m
  , Has Logger sig m
  , Has Diag.Diagnostics sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , MonadIO m
  ) =>
  BaseDir ->
  ScanDestination ->
  OverrideProject ->
  Flag UnpackArchives ->
  Flag JsonOutput ->
  VSIAnalysisMode ->
  IATAssertionMode ->
  AllFilters ->
  m ()
analyze (BaseDir basedir) destination override unpackArchives jsonOutput enableVSI iatAssertion filters = do
  capabilities <- sendIO getNumCapabilities

  let apiOpts = case destination of
        OutputStdout -> Nothing
        UploadScan opts _ -> Just opts

  manualSrcUnits <- analyzeFossaDepsFile basedir apiOpts
  vsiResults <- analyzeVSI enableVSI apiOpts basedir filters

  (projectResults, ()) <-
    runOutput @ProjectResult
      . runStickyLogger SevInfo
      . runFinally
      . withTaskPool capabilities updateProgress
      . runAtomicCounter
      $ withDiscoveredProjects discoverFuncs (fromFlag UnpackArchives unpackArchives) basedir (runDependencyAnalysis (BaseDir basedir) filters)

  let filteredProjects = filterProjects (BaseDir basedir) projectResults

  -- Need to check if vendored is empty as well, even if its a boolean that vendoredDeps exist
  case checkForEmptyUpload projectResults filteredProjects [manualSrcUnits, vsiResults] of
    NoneDiscovered -> Diag.fatal ErrNoProjectsDiscovered
    FilteredAll count -> Diag.fatal (ErrFilteredAllProjects count projectResults)
    FoundSome sourceUnits -> case destination of
      OutputStdout -> logStdout . decodeUtf8 . Aeson.encode $ buildResult manualSrcUnits filteredProjects
      UploadScan opts metadata -> do
        locator <- uploadSuccessfulAnalysis (BaseDir basedir) opts metadata jsonOutput override sourceUnits
        doAssertRevisionBinaries iatAssertion opts locator

analyzeVSI :: (MonadIO m, Has Diag.Diagnostics sig m, Has Exec sig m, Has (Lift IO) sig m, Has Logger sig m) => VSIAnalysisMode -> Maybe ApiOpts -> Path Abs Dir -> AllFilters -> m (Maybe SourceUnit)
analyzeVSI VSIAnalysisEnabled (Just apiOpts) dir filters = do
  logInfo "Running VSI analysis"
  results <- analyzeVSIDeps dir apiOpts filters
  pure $ Just results
analyzeVSI _ _ _ _ = pure Nothing

doAssertRevisionBinaries :: (Has Diag.Diagnostics sig m, Has ReadFS sig m, Has (Lift IO) sig m, Has Logger sig m) => IATAssertionMode -> ApiOpts -> Locator -> m ()
doAssertRevisionBinaries (IATAssertionEnabled dir) apiOpts locator = assertRevisionBinaries dir apiOpts locator
doAssertRevisionBinaries _ _ _ = pure ()

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
  ) =>
  BaseDir ->
  ApiOpts ->
  ProjectMetadata ->
  Flag JsonOutput ->
  OverrideProject ->
  NE.NonEmpty SourceUnit ->
  m Locator
uploadSuccessfulAnalysis (BaseDir basedir) apiOpts metadata jsonOutput override units = do
  revision <- mergeOverride override <$> (inferProjectFromVCS basedir <||> inferProjectDefault basedir)
  saveRevision revision

  logInfo ""
  logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
  logInfo ("Using revision: `" <> pretty (projectRevision revision) <> "`")
  let branchText = fromMaybe "No branch (detached HEAD)" $ projectBranch revision
  logInfo ("Using branch: `" <> pretty branchText <> "`")

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
    then logStdout . decodeUtf8 . Aeson.encode $ buildProjectSummary revision (uploadLocator uploadResult) buildUrl
    else pure ()

  pure locator

data CountedResult
  = NoneDiscovered
  | FilteredAll Int
  | FoundSome (NE.NonEmpty SourceUnit)

-- | Return some state of the projects found, since we can't upload empty result arrays.
-- Takes a list of all projects analyzed, and the list after filtering.  We assume
-- that the smaller list is the latter, and return that list.  Starting with user-defined deps,
-- we also include a check for an additional source unit from fossa-deps.yml.
checkForEmptyUpload :: [ProjectResult] -> [ProjectResult] -> [Maybe SourceUnit] -> CountedResult
checkForEmptyUpload xs ys potentialAdditionalUnits = do
  let additionalUnits = catMaybes potentialAdditionalUnits
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
    discoveredUnits = map Srclib.toSourceUnit filtered

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
  Path x Dir ->
  ApiOpts ->
  -- | Locator
  Text ->
  m ()
tryUploadContributors baseDir apiOpts locator = do
  contributors <- fetchGitContributors baseDir
  uploadContributors apiOpts locator contributors

-- | Build project summary JSON to be output to stdout
buildProjectSummary :: ProjectRevision -> Text -> Text -> Aeson.Value
buildProjectSummary project projectLocator projectUrl =
  Aeson.object
    [ "project" .= projectName project
    , "revision" .= projectRevision project
    , "branch" .= projectBranch project
    , "url" .= projectUrl
    , "id" .= projectLocator
    ]

buildResult :: Maybe SourceUnit -> [ProjectResult] -> Aeson.Value
buildResult maybeSrcUnit projects =
  Aeson.object
    [ "projects" .= map buildProject projects
    , "sourceUnits" .= finalSourceUnits
    ]
  where
    finalSourceUnits = case maybeSrcUnit of
      Just unit -> unit : scannedUnits
      Nothing -> scannedUnits
    scannedUnits = map Srclib.toSourceUnit projects

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
