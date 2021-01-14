{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module App.Fossa.Analyze
  ( analyzeMain
  , ScanDestination(..)
  , UnpackArchives(..)
  , discoverFuncs
  , fossaProjectUrl
  ) where

import App.Fossa.Analyze.GraphMangler (graphingToGraph)
import App.Fossa.Analyze.Project (ProjectResult(..), mkResult)
import App.Fossa.FossaAPIV1 (UploadResponse (..), uploadAnalysis, uploadContributors)
import App.Fossa.ProjectInference (inferProject, mergeOverride)
import App.Types
import qualified Control.Carrier.Diagnostics as Diag
import Control.Carrier.Output.IO
import Control.Carrier.Finally
import Control.Carrier.TaskPool
import Control.Concurrent
import Control.Effect.Exception
import Control.Effect.Lift (sendIO)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.Flag (Flag, fromFlag)
import Data.Foldable (traverse_, for_)
import Data.List (isInfixOf, stripPrefix)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Discovery.Filters
import Discovery.Projects (withDiscoveredProjects)
import Effect.Exec
import Effect.Logger
import Effect.ReadFS
import Fossa.API.Types (ApiOpts(..))
import Network.HTTP.Types (urlEncode)
import Path
import Path.IO (makeRelative)
import qualified Srclib.Converter as Srclib
import Srclib.Types (Locator (..), parseLocator)
import qualified Strategy.Bundler as Bundler
import qualified Strategy.Cargo as Cargo
import qualified Strategy.Carthage as Carthage
import qualified Strategy.Cocoapods as Cocoapods
import qualified Strategy.Composer as Composer
import qualified Strategy.Glide as Glide
import qualified Strategy.Gomodules as Gomodules
import qualified Strategy.Godep as Godep
import qualified Strategy.Googlesource.RepoManifest as RepoManifest
import qualified Strategy.Gradle as Gradle
import qualified Strategy.Haskell.Cabal as Cabal
import qualified Strategy.Haskell.Stack as Stack
import qualified Strategy.Leiningen as Leiningen
import qualified Strategy.Maven as Maven
import qualified Strategy.Npm as Npm
import qualified Strategy.NuGet.Nuspec as Nuspec
import qualified Strategy.NuGet.PackageReference as PackageReference
import qualified Strategy.NuGet.PackagesConfig as PackagesConfig
import qualified Strategy.NuGet.Paket as Paket
import qualified Strategy.NuGet.ProjectAssetsJson as ProjectAssetsJson
import qualified Strategy.NuGet.ProjectJson as ProjectJson
import qualified Strategy.Python.Pipenv as Pipenv
import qualified Strategy.Python.Setuptools as Setuptools
import qualified Strategy.Rebar3 as Rebar3
import qualified Strategy.RPM as RPM
import qualified Strategy.Scala as Scala
import qualified Strategy.Yarn as Yarn
import System.Exit (exitFailure)
import Text.URI (URI)
import qualified Text.URI as URI
import Types
import VCS.Git (fetchGitContributors)

data ScanDestination
  = UploadScan ApiOpts ProjectMetadata -- ^ upload to fossa with provided api key and base url
  | OutputStdout

-- | UnpackArchives bool flag
data UnpackArchives = UnpackArchives

analyzeMain :: BaseDir -> Severity -> ScanDestination -> OverrideProject -> Flag UnpackArchives -> [BuildTargetFilter] -> IO ()
analyzeMain basedir logSeverity destination project unpackArchives filters = withLogger logSeverity $
  analyze basedir destination project unpackArchives filters

discoverFuncs ::
  ( Has (Lift IO) sig m,
    MonadIO m,
    Has ReadFS sig m,
    Has Exec sig m,
    Has Logger sig m,
    Has Diag.Diagnostics sig m
  ) =>
  -- | Discover functions
  [Path Abs Dir -> m [DiscoveredProject]]
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
    Pipenv.discover
  ]

runDependencyAnalysis ::
  (Has (Lift IO) sig m, Has Logger sig m, Has (Output ProjectResult) sig m) =>
  -- | Analysis base directory
  BaseDir ->
  [BuildTargetFilter] ->
  DiscoveredProject ->
  m ()
runDependencyAnalysis (BaseDir basedir) filters project = do
  case applyFiltersToProject basedir filters project of
    Nothing -> logInfo $ "Skipping " <> pretty (projectType project) <> " project at " <> viaShow (projectPath project) <> ": no filters matched"
    Just targets -> do
      logInfo $ "Analyzing " <> pretty (projectType project) <> " project at " <> pretty (toFilePath (projectPath project))
      graphResult <- sendIO . Diag.runDiagnosticsIO $ projectDependencyGraph project targets
      Diag.withResult SevWarn graphResult (output . mkResult project)

applyFiltersToProject :: Path Abs Dir -> [BuildTargetFilter] -> DiscoveredProject -> Maybe (Set BuildTarget)
applyFiltersToProject basedir filters DiscoveredProject{..} =
  case makeRelative basedir projectPath of
    -- FIXME: this is required for --unpack-archives to continue to work.
    -- archives are not unpacked relative to the scan basedir, so "makeRelative"
    -- will always fail
    Nothing -> Just projectBuildTargets
    Just rel -> applyFilters filters projectType rel projectBuildTargets

analyze ::
  ( Has (Lift IO) sig m
  , Has Logger sig m
  , MonadIO m
  )
  => BaseDir
  -> ScanDestination
  -> OverrideProject
  -> Flag UnpackArchives
  -> [BuildTargetFilter]
  -> m ()
analyze basedir destination override unpackArchives filters = do
  capabilities <- sendIO getNumCapabilities

  (projectResults, ()) <-
    runOutput @ProjectResult
      . runExecIO
      . runReadFSIO
      . runFinally
      . withTaskPool capabilities updateProgress
      $ withDiscoveredProjects discoverFuncs (fromFlag UnpackArchives unpackArchives) (unBaseDir basedir) (runDependencyAnalysis basedir filters)

  logSticky ""
  let filteredProjects = filterProjects basedir projectResults

  case checkForEmptyUpload projectResults filteredProjects of
    NoneDiscovered -> logError "No projects were discovered" >> sendIO exitFailure
    FilteredAll count -> do
      logError ("Filtered out all " <> pretty count <> " projects due to directory name")
      for_ projectResults $ \project -> logDebug ("Excluded by directory name: " <> pretty (toFilePath $ projectResultPath project))
      sendIO exitFailure
    FoundSome someProjects -> case destination of
      OutputStdout -> logStdout . pretty . decodeUtf8 . Aeson.encode . buildResult $ NE.toList someProjects
      UploadScan apiOpts metadata -> do
        revision <- mergeOverride override <$> inferProject (unBaseDir basedir)

        logInfo ""
        logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
        logInfo ("Using revision: `" <> pretty (projectRevision revision) <> "`")
        let branchText = fromMaybe "No branch (detached HEAD)" $ projectBranch revision
        logInfo ("Using branch: `" <> pretty branchText <> "`")

        uploadResult <- Diag.runDiagnostics $ uploadAnalysis apiOpts revision metadata someProjects
        case uploadResult of
          Left failure -> logError (Diag.renderFailureBundle failure)
          Right success -> do
            let resp = Diag.resultValue success
            logInfo $ vsep
              [ "============================================================"
              , ""
              , "    View FOSSA Report:"
              , "    " <> pretty (fossaProjectUrl (apiOptsUri apiOpts) (uploadLocator resp) revision)
              , ""
              , "============================================================"
              ]
            traverse_ (\err -> logError $ "FOSSA error: " <> viaShow err) (uploadError resp)

            contribResult <- Diag.runDiagnostics $ runExecIO $ tryUploadContributors (unBaseDir basedir) apiOpts (uploadLocator resp)
            case contribResult of
              Left failure -> logDebug (Diag.renderFailureBundle failure)
              Right _ -> pure ()

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
  | otherwise              = FoundSome $ NE.fromList filtered
  where 
    xlen = length xs
    ylen = length ys
    filterCount = abs $ xlen - ylen
    -- | Return the smaller list, since filtering cannot add projects
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
isProductionPath path = not $ any (`isInfixOf` path)
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

-- This url can have a two forms (Core may allow more, but we don't care here):
--    https://<fossa host>/projects/<project>/
--    https://<fossa host>/projects/<project>/refs/branch/<branch>/<revision>
fossaProjectUrl :: URI -> Text -> ProjectRevision -> Text
fossaProjectUrl baseUrl rawLocator revision = URI.render baseUrl <> "projects/" <> encodedProject <> buildSelector
  where
    Locator{locatorFetcher, locatorProject, locatorRevision} = parseLocator rawLocator

    underBS :: (ByteString -> ByteString) -> Text -> Text
    underBS f = TE.decodeUtf8 . f . TE.encodeUtf8
    urlEncode' = underBS (urlEncode True)

    encodedProject = urlEncode' (locatorFetcher <> "+" <> locatorProject)
    encodedRevision = urlEncode' (fromMaybe "" locatorRevision)
    -- | buildSelector is empty string unless we have a real branch to work with.
    buildSelector = fromMaybe "" $ do
      branch <- projectBranch revision
      Just $ "/refs/branch/" <> urlEncode' branch <> "/" <> encodedRevision

buildResult :: [ProjectResult] -> Aeson.Value
buildResult projects = Aeson.object
  [ "projects" .= map buildProject projects
  , "sourceUnits" .= map Srclib.toSourceUnit projects
  ]

buildProject :: ProjectResult -> Aeson.Value
buildProject project = Aeson.object
  [ "path" .= projectResultPath project
  , "type" .= projectResultType project
  , "graph" .= graphingToGraph (projectResultGraph project)
  ]

updateProgress :: Has Logger sig m => Progress -> m ()
updateProgress Progress{..} =
  logSticky ( "[ "
            <> annotate (color Cyan) (pretty pQueued)
            <> " Waiting / "
            <> annotate (color Yellow) (pretty pRunning)
            <> " Running / "
            <> annotate (color Green) (pretty pCompleted)
            <> " Completed"
            <> " ]" )
