{-# LANGUAGE TemplateHaskell #-}

module Strategy.NuGet (
  discover,
  findProjects,
  getDeps,
  mkProject,
  NuGetProject (..),
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
import Data.Set qualified as Set
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue),
  fileName,
  findFileNamed,
  walkWithFilters',
 )
import Effect.ReadFS (ReadFS)
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
findProjects root = do
  found <- walkWithFilters' findInDir root
  -- project.assets.json (created by `dotnet restore`) is essentially a lockfile.
  -- If one was discovered in the `obj/` subdirectory beside a project file, then
  -- don't return the project file as well, as this will double-report the project,
  -- and the project.assets.json is more detailed and accurate.
  --
  -- This is decided from the files the walk discovered rather than by checking the
  -- filesystem, so that a project file is still reported when path filters keep the
  -- walk out of `obj/`.
  let assetsFiles = Set.fromList $ filter isProjectAssetsJsonFile found
      isRestored file = isPackageRefFile file && Set.member (parent file </> $(mkRelFile "obj/project.assets.json")) assetsFiles
  pure . map NuGetProject $ filter (not . isRestored) found
  where
    findInDir _ _ files =
      case findProjectAssetsJsonFile files of
        Just assetsFile -> pure ([assetsFile], WalkContinue)
        Nothing -> pure (filter isPackageRefFile files, WalkContinue)

    isProjectAssetsJsonFile :: Path b File -> Bool
    isProjectAssetsJsonFile file = fileName file == "project.assets.json"

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
