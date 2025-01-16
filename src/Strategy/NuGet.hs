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
import Data.Foldable (find)
import Data.List qualified as L
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
import Path (Abs, Dir, File, Path, parent)
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
findProjects = walkWithFilters' $ \_ _ files -> do
  case findProjectAssetsJsonFile files of
    Just file -> pure ([NuGetProject file], WalkContinue)
    Nothing -> case find isPackageRefFile files of
      Just file -> pure ([NuGetProject file], WalkContinue)
      Nothing -> pure ([], WalkContinue)
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
getPackageReferenceDeps = context "PackageReference" . context "Static analysis" . PackageReference.analyze' . nugetProjectFile
