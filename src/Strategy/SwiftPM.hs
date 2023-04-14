{-# LANGUAGE QuasiQuotes #-}

module Strategy.SwiftPM (
  discover,
  mkProject,
  SwiftProject,
) where

import App.Fossa.Analyze.Types (AnalyzeProject (..))
import Control.Carrier.Simple (Has)
import Control.Effect.Diagnostics (Diagnostics, context)
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON)
import Data.Functor (($>))
import Data.Maybe (listToMaybe)
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue, WalkSkipSome),
  findFileNamed,
  walk',
 )
import Effect.Logger (Logger, Pretty (pretty), logDebug)
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path, dirname, reldir)
import Strategy.Swift.PackageSwift (analyzePackageSwift)
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

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has Logger sig m) => Path Abs Dir -> m [SwiftProject]
findProjects dir = do
  swiftPackageProjects <- context "Finding swift package projects" $ findSwiftPackageProjects dir
  xCodeProjects <- context "Finding xcode projects using swift package manager" $ findXcodeProjects dir
  pure (swiftPackageProjects <> xCodeProjects)

-- TODO: determine if walkWithFilters' is safe here
findSwiftPackageProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [SwiftProject]
findSwiftPackageProjects = walk' $ \dir _ files -> do
  let packageManifestFile = findFileNamed "Package.swift" files
  let packageResolvedFile = findFileNamed "Package.resolved" files
  case (packageManifestFile, packageResolvedFile) of
    -- If the Package.swift exists, than it is swift package project.
    -- Use Package.swift as primary source of truth.
    (Just manifestFile, resolvedFile) -> pure ([PackageProject $ SwiftPackageProject manifestFile dir resolvedFile], WalkSkipSome [".build"])
    -- Package.resolved without Package.swift or Xcode project file is not a valid swift project.
    (Nothing, _) -> pure ([], WalkContinue)

-- TODO: determine if walkWithFilters' is safe here
findXcodeProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has Logger sig m) => Path Abs Dir -> m [SwiftProject]
findXcodeProjects = walk' $ \dir _ files -> do
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
findFirstResolvedFileRecursively :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m (Maybe (Path Abs File))
findFirstResolvedFileRecursively baseDir = listToMaybe <$> walk' findFile baseDir
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
  analyzeProject' _ = getDeps

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => SwiftProject -> m DependencyResults
getDeps project = do
  graph <- case project of
    PackageProject prj -> analyzePackageSwift (swiftPkgManifest prj) (swiftPkgResolved prj)
    XcodeProject prj -> analyzeXcodeProjForSwiftPkg (xCodeProjectFile prj) (xCodeResolvedFile prj)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Partial
      , dependencyManifestFiles = manifestFiles
      }
  where
    manifestFiles :: [Path Abs File]
    manifestFiles = case project of
      PackageProject prj -> [swiftPkgManifest prj]
      XcodeProject prj -> [xCodeProjectFile prj]
