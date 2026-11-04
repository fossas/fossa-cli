{-# LANGUAGE QuasiQuotes #-}

module Strategy.SwiftPM (
  discover,
  mkProject,
  SwiftProject (..),

  -- * for testing
  SwiftPackageProject (..),
  getDepsStatic,
) where

import App.Fossa.Analyze.Types (AnalyzeProject (..))
import App.Types (Mode)
import App.Util (guardStrictMode)
import Control.Carrier.Simple (Has)
import Control.Effect.Diagnostics (Diagnostics, context, errCtx, errDoc, errHelp, fatalText, warnOnErr, (<||>))
import Control.Effect.Reader (Reader, ask)
import Data.Aeson (ToJSON)
import Data.Functor (($>))
import Data.Maybe (listToMaybe)
import DepTypes (Dependency)
import Diag.Common (AllDirectDeps (AllDirectDeps), MissingEdges (MissingEdges))
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue, WalkSkipSome),
  findFileNamed,
  walkWithFilters',
 )
import Effect.Exec (GetDepsEffs)
import Effect.Logger (Logger, Pretty (pretty), logDebug)
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Graphing (Graphing)
import Path (Abs, Dir, File, Path, dirname, reldir)
import Strategy.Swift.Errors (
  MissingPackageResolvedFile (MissingPackageResolvedFile),
  MissingPackageResolvedFileHelp (MissingPackageResolvedFileHelp),
  swiftFossaDocUrl,
  swiftPackageResolvedRef,
  xcodeCoordinatePkgVersion,
 )
import Strategy.Swift.PackageResolved (analyzePackageResolved)
import Strategy.Swift.PackageSwift (analyzePackageSwift)
import Strategy.Swift.ShowDependencies (analyzeShowDependencies)
import Strategy.Swift.Xcode.Pbxproj (analyzeXcodeProjForSwiftPkg, hasSomeSwiftDeps)
import Types (DependencyResults (..), DiscoveredProject (..), DiscoveredProjectType (SwiftProjectType), GraphBreadth (..))

data SwiftProject
  = PackageProject SwiftPackageProject
  | XcodeProject XcodeProjectUsingSwiftPm
  deriving (Show, Eq, Ord, Generic)

data SwiftPackageProject = SwiftPackageProject
  { swiftPkgManifest :: Path Abs File
  , swiftPkgProjectDir :: Path Abs Dir
  , swiftPkgResolved :: Maybe (Path Abs File)
  }
  deriving (Show, Eq, Ord, Generic)

data XcodeProjectUsingSwiftPm = XcodeProjectUsingSwiftPm
  { xCodeProjectFile :: Path Abs File
  , xCodeProjectDir :: Path Abs Dir
  , xCodeResolvedFile :: Maybe (Path Abs File)
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON SwiftPackageProject
instance ToJSON XcodeProjectUsingSwiftPm
instance ToJSON SwiftProject

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has Logger sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [DiscoveredProject SwiftProject]
discover = simpleDiscover findProjects mkProject SwiftProjectType

findProjects ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  , Has (Reader AllFilters) sig m
  ) =>
  Path Abs Dir ->
  m [SwiftProject]
findProjects dir = do
  swiftPackageProjects <- context "Finding swift package projects" $ findSwiftPackageProjects dir
  xCodeProjects <- context "Finding xcode projects using swift package manager" $ findXcodeProjects dir
  pure (swiftPackageProjects <> xCodeProjects)

findSwiftPackageProjects ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Reader AllFilters) sig m
  ) =>
  Path Abs Dir ->
  m [SwiftProject]
findSwiftPackageProjects = walkWithFilters' $ \dir _ files -> do
  let packageManifestFile = findFileNamed "Package.swift" files
  let packageResolvedFile = findFileNamed "Package.resolved" files
  case (packageManifestFile, packageResolvedFile) of
    -- If the Package.swift exists, than it is swift package project.
    -- Use Package.swift as primary source of truth.
    (Just manifestFile, resolvedFile) -> pure ([PackageProject $ SwiftPackageProject manifestFile dir resolvedFile], WalkSkipSome [".build"])
    -- Package.resolved without Package.swift or Xcode project file is not a valid swift project.
    (Nothing, _) -> pure ([], WalkContinue)

findXcodeProjects ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  , Has (Reader AllFilters) sig m
  ) =>
  Path Abs Dir ->
  m [SwiftProject]
findXcodeProjects = walkWithFilters' $ \dir _ files -> do
  let xcodeProjectFile = findFileNamed "project.pbxproj" files
  case xcodeProjectFile of
    Nothing -> pure ([], WalkContinue)
    Just projFile -> do
      resolvedFile <- findFirstResolvedFileRecursively dir
      xCodeProjWithDependencies <- hasSomeSwiftDeps projFile
      if xCodeProjWithDependencies
        then pure ([XcodeProject $ XcodeProjectUsingSwiftPm projFile dir resolvedFile], WalkSkipSome [".build"])
        else debugXCodeWithoutSwiftDeps projFile $> ([], WalkContinue)

-- | Walks directory and finds first file named 'Package.resolved'.
-- XCode projects using swift package manager retain Package.resolved,
-- not in the same directory as project file, but rather in workspace's xcshareddata/swiftpm directory.
-- Reference: https://developer.apple.com/documentation/swift_packages/adding_package_dependencies_to_your_app.
findFirstResolvedFileRecursively ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Reader AllFilters) sig m
  ) =>
  Path Abs Dir ->
  m (Maybe (Path Abs File))
findFirstResolvedFileRecursively baseDir = listToMaybe <$> walkWithFilters' findFile baseDir
  where
    isParentDirSwiftPm :: Path Abs Dir -> Bool
    isParentDirSwiftPm d = (dirname d) == [reldir|swiftpm|]

    findFile :: forall f. Applicative f => Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> f ([Path Abs File], WalkStep)
    findFile dir _ files = do
      let foundFile = findFileNamed "Package.resolved" files
      case (foundFile) of
        (Just ff) ->
          if (isParentDirSwiftPm dir)
            then pure ([ff], WalkSkipSome [".build"])
            else pure ([], WalkContinue)
        _ -> pure ([], WalkContinue)

debugXCodeWithoutSwiftDeps :: Has Logger sig m => Path Abs File -> m ()
debugXCodeWithoutSwiftDeps projFile =
  (logDebug . pretty) $
    "XCode project file ("
      <> show projFile
      <> "), did not have any XCRemoteSwiftPackageReference, ignoring from swift analyses."

mkProject :: SwiftProject -> DiscoveredProject SwiftProject
mkProject project =
  DiscoveredProject
    { projectType = SwiftProjectType
    , projectBuildTargets = mempty
    , projectPath = case project of
        PackageProject p -> swiftPkgProjectDir p
        XcodeProject p -> xCodeProjectDir p
    , projectData = project
    }

instance AnalyzeProject SwiftProject where
  analyzeProject _ = getDeps
  analyzeProjectStaticOnly _ = getDepsStatic

-- | Swift packages are analyzed by letting SwiftPM resolve the package graph, falling back to
-- the static strategies when that fails (no toolchain, no network, ...).
-- Xcode projects are only analyzed statically, because `swift package` cannot read them.
getDeps :: GetDepsEffs sig m => SwiftProject -> m DependencyResults
getDeps project@(XcodeProject _) = getDepsStatic project
getDeps project@(PackageProject prj) = do
  mode <- ask
  context "Swift package" $ warnOnErr MissingEdges dynamicAnalysis <||> guardStrictMode mode (getDepsStatic project)
  where
    -- The warning surfaces why the dynamic analysis was skipped; the static fallback never reports edges.
    dynamicAnalysis = do
      graph <- context "Dynamic analysis using swift package show-dependencies" $ analyzeShowDependencies (swiftPkgProjectDir prj)
      pure $
        DependencyResults
          { dependencyGraph = graph
          , dependencyGraphBreadth = Complete
          , dependencyManifestFiles = [swiftPkgManifest prj]
          }

-- | Swift packages are parsed from Package.swift and Package.resolved, falling back to
-- Package.resolved alone when the manifest cannot be parsed. Xcode projects are parsed from
-- project.pbxproj and Package.resolved.
getDepsStatic :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader Mode) sig m) => SwiftProject -> m DependencyResults
getDepsStatic (XcodeProject prj) =
  staticResults (xCodeProjectFile prj) <$> analyzeXcodeProjForSwiftPkg (xCodeProjectFile prj) (xCodeResolvedFile prj)
getDepsStatic (PackageProject prj) = do
  mode <- ask
  staticResults (swiftPkgManifest prj) <$> (warnOnErr AllDirectDeps manifestAnalysis <||> guardStrictMode mode lockfileAnalysis)
  where
    -- The warning surfaces why the manifest was skipped; the lockfile alone cannot tell direct
    -- dependencies from deep ones.
    manifestAnalysis = context "Static analysis using Package.swift" $ analyzePackageSwift (swiftPkgManifest prj) (swiftPkgResolved prj)
    lockfileAnalysis = context "Static analysis using Package.resolved only" $ case swiftPkgResolved prj of
      Just resolvedFile -> analyzePackageResolved resolvedFile
      Nothing ->
        errCtx (MissingPackageResolvedFile (swiftPkgManifest prj))
          . errHelp MissingPackageResolvedFileHelp
          . errDoc swiftFossaDocUrl
          . errDoc swiftPackageResolvedRef
          . errDoc xcodeCoordinatePkgVersion
          $ fatalText "Package.resolved file was not discovered"

staticResults :: Path Abs File -> Graphing Dependency -> DependencyResults
staticResults manifestFile graph =
  DependencyResults
    { dependencyGraph = graph
    , dependencyGraphBreadth = Partial
    , dependencyManifestFiles = [manifestFile]
    }
