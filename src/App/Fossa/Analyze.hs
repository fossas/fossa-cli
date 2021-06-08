{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Analyze
  ( analyzeMain,
    ScanDestination (..),
    UnpackArchives (..),
    VSIAnalysisMode (..),
    discoverFuncs,
    RecordMode (..),
  )
where

import App.Fossa.API.BuildLink (getFossaBuildUrl)
import App.Fossa.Analyze.GraphMangler (graphingToGraph)
import App.Fossa.Analyze.Project (ProjectResult (..), mkResult)
import App.Fossa.Analyze.Record (AnalyzeEffects (..), AnalyzeJournal (..), loadReplayLog, saveReplayLog)
import App.Fossa.FossaAPIV1 (UploadResponse (..), uploadAnalysis, uploadContributors)
import App.Fossa.ProjectInference (inferProjectDefault, inferProjectFromVCS, mergeOverride, saveRevision)
import App.Types
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
import Control.Effect.Exception
import Control.Effect.Lift (sendIO)
import Control.Effect.Record
import Control.Effect.Replay (runReplay)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Flag (Flag, fromFlag)
import Data.Foldable (for_, traverse_)
import Data.Functor (void)
import Data.List (isInfixOf, stripPrefix)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.String.Conversion (decodeUtf8)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Discovery.Filters
import Discovery.Projects (withDiscoveredProjects)
import Effect.Exec
import Effect.Logger
import Effect.ReadFS
import Fossa.API.Types (ApiOpts (..))
import Path
import Path.IO (makeRelative)
import Path.IO qualified as P
import Srclib.Converter qualified as Srclib
import Srclib.Types (parseLocator)
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
import Strategy.Npm qualified as Npm
import Strategy.NuGet.Nuspec qualified as Nuspec
import Strategy.NuGet.PackageReference qualified as PackageReference
import Strategy.NuGet.PackagesConfig qualified as PackagesConfig
import Strategy.NuGet.Paket qualified as Paket
import Strategy.NuGet.ProjectAssetsJson qualified as ProjectAssetsJson
import Strategy.NuGet.ProjectJson qualified as ProjectJson
import Strategy.Python.Pipenv qualified as Pipenv
import Strategy.Python.Setuptools qualified as Setuptools
import Strategy.RPM qualified as RPM
import Strategy.Rebar3 qualified as Rebar3
import Strategy.Scala qualified as Scala
import Strategy.UserSpecified.YamlDependencies qualified as UserYaml
import Strategy.VSI qualified as VSI
import Strategy.Yarn qualified as Yarn
import System.Exit (die, exitFailure)
import Types
import VCS.Git (fetchGitContributors)

type TaskEffs sig m =
  ( Has (Lift IO) sig m,
    MonadIO m,
    Has ReadFS sig m,
    Has Exec sig m,
    Has Logger sig m,
    Has Diag.Diagnostics sig m
  )

data ScanDestination
  = -- | upload to fossa with provided api key and base url
    UploadScan ApiOpts ProjectMetadata
  | OutputStdout

-- | UnpackArchives bool flag
data UnpackArchives = UnpackArchives

-- | "VSI analysis" modes
data VSIAnalysisMode
  = -- | enable the VSI analysis strategy
    VSIAnalysisEnabled
  | -- | disable the VSI analysis strategy
    VSIAnalysisDisabled

-- | "Replay logging" modes
data RecordMode
  = -- | record effect invocations
    RecordModeRecord
  | -- | replay effect invocations from a file
    RecordModeReplay FilePath
  | -- | don't record or replay
    RecordModeNone

analyzeMain :: FilePath -> RecordMode -> Severity -> ScanDestination -> OverrideProject -> Flag UnpackArchives -> VSIAnalysisMode -> [BuildTargetFilter] -> IO ()
analyzeMain workdir recordMode logSeverity destination project unpackArchives enableVSI filters =
  withDefaultLogger logSeverity
    . Diag.logWithExit_
    . runReadFSIO
    . runExecIO
    $ case recordMode of
      RecordModeNone -> do
        basedir <- sendIO $ validateDir workdir
        analyze basedir destination project unpackArchives enableVSI filters
      RecordModeRecord -> do
        basedir <- sendIO $ validateDir workdir
        (execLogs, (readFSLogs, ())) <-
          runRecord @Exec . runRecord @ReadFS $
            analyze basedir destination project unpackArchives enableVSI filters
        sendIO $ saveReplayLog readFSLogs execLogs "fossa.debug.json"
      RecordModeReplay file -> do
        basedir <- BaseDir <$> P.resolveDir' workdir
        maybeJournal <- sendIO $ loadReplayLog file
        case maybeJournal of
          Left err -> sendIO (die $ "Issue loading replay log: " <> err)
          Right journal -> do
            let effects = analyzeEffects journal
            runReplay @ReadFS (effectsReadFS effects)
              . runReplay @Exec (effectsExec effects)
              $ analyze basedir destination project unpackArchives enableVSI filters

-- vsiDiscoverFunc is appended to discoverFuncs during analyze.
-- It's not added to discoverFuncs because it requires more information than other discoverFuncs.
vsiDiscoverFunc :: (TaskEffs sig m, TaskEffs rsig run) => VSIAnalysisMode -> ScanDestination -> Path Abs Dir -> m [DiscoveredProject run]
vsiDiscoverFunc VSIAnalysisEnabled (UploadScan apiOpts _) = VSI.discover apiOpts
vsiDiscoverFunc _ _ = const $ pure []

discoverFuncs :: (TaskEffs sig m, TaskEffs rsig run) => [Path Abs Dir -> m [DiscoveredProject run]]
discoverFuncs =
  [ Bundler.discover,
    Cargo.discover,
    Carthage.discover,
    Cocoapods.discover,
    Gradle.discover,
    Rebar3.discover,
    Gomodules.discover,
    Godep.discover,
    Setuptools.discover,
    Maven.discover,
    Leiningen.discover,
    Composer.discover,
    Cabal.discover,
    Stack.discover,
    Yarn.discover,
    Npm.discover,
    Scala.discover,
    RPM.discover,
    RepoManifest.discover,
    Nuspec.discover,
    PackageReference.discover,
    PackagesConfig.discover,
    Paket.discover,
    ProjectAssetsJson.discover,
    ProjectJson.discover,
    Glide.discover,
    Pipenv.discover,
    Conda.discover,
    UserYaml.discover
  ]

runDependencyAnalysis ::
  (Has (Lift IO) sig m, Has AtomicCounter sig m, Has Logger sig m, Has (Output ProjectResult) sig m) =>
  -- | Analysis base directory
  BaseDir ->
  [BuildTargetFilter] ->
  DiscoveredProject (StickyDiagC (Diag.DiagnosticsC m)) ->
  m ()
runDependencyAnalysis (BaseDir basedir) filters project =
  case applyFiltersToProject basedir filters project of
    Nothing -> logInfo $ "Skipping " <> pretty (projectType project) <> " project at " <> viaShow (projectPath project) <> ": no filters matched"
    Just targets -> do
      logInfo $ "Analyzing " <> pretty (projectType project) <> " project at " <> pretty (toFilePath (projectPath project))
      graphResult <- Diag.runDiagnosticsIO . stickyDiag $ projectDependencyGraph project targets
      Diag.withResult SevWarn graphResult (output . mkResult project)

applyFiltersToProject :: Path Abs Dir -> [BuildTargetFilter] -> DiscoveredProject n -> Maybe (Set BuildTarget)
applyFiltersToProject basedir filters DiscoveredProject {..} =
  case makeRelative basedir projectPath of
    -- FIXME: this is required for --unpack-archives to continue to work.
    -- archives are not unpacked relative to the scan basedir, so "makeRelative"
    -- will always fail
    Nothing -> Just projectBuildTargets
    Just rel -> applyFilters filters projectType rel projectBuildTargets

analyze ::
  ( Has (Lift IO) sig m,
    Has Logger sig m,
    Has Diag.Diagnostics sig m,
    Has Exec sig m,
    Has ReadFS sig m,
    MonadIO m
  ) =>
  BaseDir ->
  ScanDestination ->
  OverrideProject ->
  Flag UnpackArchives ->
  VSIAnalysisMode ->
  [BuildTargetFilter] ->
  m ()
analyze (BaseDir basedir) destination override unpackArchives enableVSI filters = do
  capabilities <- sendIO getNumCapabilities
  -- When running analysis, append the vsi discover function to the end of the discover functions list.
  -- This is done because the VSI discover function requires more information than other discover functions do, and only matters for analysis.
  let discoverFuncs' = discoverFuncs ++ [vsiDiscoverFunc enableVSI destination]

  (projectResults, ()) <-
    runOutput @ProjectResult
      . runStickyLogger SevInfo
      . runFinally
      . withTaskPool capabilities updateProgress
      . runAtomicCounter
      $ withDiscoveredProjects discoverFuncs' (fromFlag UnpackArchives unpackArchives) basedir (runDependencyAnalysis (BaseDir basedir) filters)

  let filteredProjects = filterProjects (BaseDir basedir) projectResults

  case checkForEmptyUpload projectResults filteredProjects of
    NoneDiscovered -> logError "No projects were discovered" >> sendIO exitFailure
    FilteredAll count -> do
      logError ("Filtered out all " <> pretty count <> " projects due to directory name")
      for_ projectResults $ \project -> logDebug ("Excluded by directory name: " <> pretty (toFilePath $ projectResultPath project))
      sendIO exitFailure
    FoundSome someProjects -> case destination of
      OutputStdout -> logStdout . decodeUtf8 . Aeson.encode . buildResult $ NE.toList someProjects
      UploadScan apiOpts metadata -> do
        revision <- mergeOverride override <$> (inferProjectFromVCS basedir <||> inferProjectDefault basedir)
        saveRevision revision

        logInfo ""
        logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
        logInfo ("Using revision: `" <> pretty (projectRevision revision) <> "`")
        let branchText = fromMaybe "No branch (detached HEAD)" $ projectBranch revision
        logInfo ("Using branch: `" <> pretty branchText <> "`")

        uploadResult <- uploadAnalysis apiOpts revision metadata someProjects
        buildUrl <- getFossaBuildUrl revision apiOpts . parseLocator $ uploadLocator uploadResult
        logInfo $
          vsep
            [ "============================================================",
              "",
              "    View FOSSA Report:",
              "    " <> pretty buildUrl,
              "",
              "============================================================"
            ]
        traverse_ (\err -> logError $ "FOSSA error: " <> viaShow err) (uploadError uploadResult)
        -- Warn on contributor errors, never fail
        void . Diag.recover . runExecIO $ tryUploadContributors basedir apiOpts (uploadLocator uploadResult)

data CountedResult
  = NoneDiscovered
  | FilteredAll Int
  | FoundSome (NE.NonEmpty ProjectResult)

-- | Return some state of the projects found, since we can't upload empty result arrays.
-- We accept a list of all projects analyzed, and the list after filtering.  We assume
-- that the smaller list is the latter, and re.
checkForEmptyUpload :: [ProjectResult] -> [ProjectResult] -> CountedResult
checkForEmptyUpload xs ys
  | xlen == 0 && ylen == 0 = NoneDiscovered
  | xlen == 0 || ylen == 0 = FilteredAll filterCount
  -- NE.fromList is a partial, but is safe since we confirm the length is > 0.
  | otherwise = FoundSome $ NE.fromList filtered
  where
    xlen = length xs
    ylen = length ys
    filterCount = abs $ xlen - ylen
    -- Return the smaller list, since filtering cannot add projects

    filtered = if xlen > ylen then ys else xs

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
      [ "doc/",
        "docs/",
        "test/",
        "example/",
        "examples/",
        "vendor/",
        "node_modules/",
        ".srclib-cache/",
        "spec/",
        "Godeps/",
        ".git/",
        "bower_components/",
        "third_party/",
        "third-party/",
        "Carthage/",
        "Checkouts/"
      ]

tryUploadContributors ::
  ( Has Diag.Diagnostics sig m,
    Has Exec sig m,
    Has (Lift IO) sig m
  ) =>
  Path x Dir ->
  ApiOpts ->
  -- | Locator
  Text ->
  m ()
tryUploadContributors baseDir apiOpts locator = do
  contributors <- fetchGitContributors baseDir
  uploadContributors apiOpts locator contributors

buildResult :: [ProjectResult] -> Aeson.Value
buildResult projects =
  Aeson.object
    [ "projects" .= map buildProject projects,
      "sourceUnits" .= map Srclib.toSourceUnit projects
    ]

buildProject :: ProjectResult -> Aeson.Value
buildProject project =
  Aeson.object
    [ "path" .= projectResultPath project,
      "type" .= projectResultType project,
      "graph" .= graphingToGraph (projectResultGraph project)
    ]

updateProgress :: Has StickyLogger sig m => Progress -> m ()
updateProgress Progress {..} =
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
