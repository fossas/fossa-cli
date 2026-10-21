{-# LANGUAGE TemplateHaskell #-}

module Strategy.NuGet (
  discover,
  findProjects,
  getDeps,
  mkProject,
  NuGetProject (..),
  discoverVisitor,
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProjectStaticOnly), analyzeProject)
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  context,
 )
import Control.Effect.Reader (Reader)
import Data.Aeson (
  ToJSON,
 )
import Data.List qualified as L
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue),
  fileName,
  findFileNamed,
  walkWithFilters',
 )
import Effect.ReadFS (ReadFS, doesFileExist)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path, mkRelFile, parent, (</>))
import Strategy.NuGet.DirectoryPackagesProps qualified as DirectoryPackagesProps
import Strategy.NuGet.PackageReference qualified as PackageReference
import Strategy.NuGet.ProjectAssetsJson qualified as ProjectAssetsJson
import Types (
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (NuGetProjectType),
 )

discover ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Reader AllFilters) sig m
  ) =>
  Path Abs Dir ->
  m [DiscoveredProject NuGetProject]
discover = simpleDiscover findProjects mkProject NuGetProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [NuGetProject]
findProjects = walkWithFilters' discoverVisitor

-- | Per-directory collector for the shared discovery walk, so the tree is traversed once for all strategies.
discoverVisitor :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m ([NuGetProject], WalkStep)
discoverVisitor dir _ files = do
  case findProjectAssetsJsonFile files of
    Just file -> pure ([NuGetProject file], WalkContinue)
    Nothing -> do
      -- project.assets.json (created by `dotnet restore`) is essentially a lockfile.
      -- If this is present in an `obj/` subdirectory beside the project file, then
      -- don't return the project file as well, as this will double-report the project,
      -- and the project.assets.json is more detailed and accurate.
      restored <- doesFileExist (dir </> $(mkRelFile "obj/project.assets.json"))
      let projectFiles = if restored then [] else filter isPackageRefFile files
      pure (map NuGetProject projectFiles, WalkContinue)
  where
    findProjectAssetsJsonFile :: [Path Abs File] -> Maybe (Path Abs File)
    findProjectAssetsJsonFile = findFileNamed "project.assets.json"

    isPackageRefFile :: Path b File -> Bool
    isPackageRefFile file = any (\x -> x `L.isSuffixOf` fileName file) [".csproj", ".xproj", ".vbproj", ".dbproj", ".fsproj"]

mkProject :: NuGetProject -> DiscoveredProject NuGetProject
mkProject project =
  DiscoveredProject
    { projectType = NuGetProjectType
    , projectPath = parent $ nugetProjectFile project
    , projectBuildTargets = mempty
    , projectData = project
    }

newtype NuGetProject = NuGetProject
  { nugetProjectFile :: Path Abs File
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON NuGetProject

instance AnalyzeProject NuGetProject where
  analyzeProject _ = getDeps
  analyzeProjectStaticOnly _ = getDeps

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => NuGetProject -> m DependencyResults
getDeps project =
  context "NuGet" $
    if "project.assets.json" == (fileName . nugetProjectFile) project
      then getAssetsJsonDeps project
      else getPackageReferenceDeps project

getAssetsJsonDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => NuGetProject -> m DependencyResults
getAssetsJsonDeps = context "ProjectAssetsJson" . context "Static analysis" . ProjectAssetsJson.analyze' . nugetProjectFile

getPackageReferenceDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => NuGetProject -> m DependencyResults
getPackageReferenceDeps project = context "PackageReference" . context "Static analysis" $ do
  let file = nugetProjectFile project
  versionMap <- context "Directory.Packages.props" $ DirectoryPackagesProps.findAndParse (parent file)
  PackageReference.analyzeWithCPM versionMap file
