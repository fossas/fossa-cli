{-# LANGUAGE RecordWildCards #-}

module Strategy.NuGet.ProjectAssetsJson (
  discover,
  findProjects,
  getDeps,
  mkProject,
  buildGraph,
  ProjectAssetsJson (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject, analyzeProject)
import Control.Effect.Diagnostics
import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes
import Discovery.Walk
import Effect.ReadFS
import GHC.Generics (Generic)
import Graphing (Graphing, unfold)
import Path
import Types

discover :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [DiscoveredProject ProjectAssetsJsonProject]
discover dir = context "ProjectAssetsJson" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [ProjectAssetsJsonProject]
findProjects = walk' $ \_ _ files -> do
  case findFileNamed "project.assets.json" files of
    Nothing -> pure ([], WalkContinue)
    Just file -> pure ([ProjectAssetsJsonProject file], WalkContinue)

newtype ProjectAssetsJsonProject = ProjectAssetsJsonProject
  { projectAssetsJsonFile :: Path Abs File
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ProjectAssetsJsonProject

instance AnalyzeProject ProjectAssetsJsonProject where
  analyzeProject _ = getDeps

mkProject :: ProjectAssetsJsonProject -> DiscoveredProject ProjectAssetsJsonProject
mkProject project =
  DiscoveredProject
    { projectType = "projectassetsjson"
    , projectBuildTargets = mempty
    , projectPath = parent $ projectAssetsJsonFile project
    , projectData = project
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => ProjectAssetsJsonProject -> m DependencyResults
getDeps = context "ProjectAssetsJson" . context "Static analysis" . analyze' . projectAssetsJsonFile

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m DependencyResults
analyze' file = do
  assetsJson <- readContentsJson @ProjectAssetsJson file
  graph <- context "Building dependency graph" $ pure (buildGraph assetsJson)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [file]
      }

newtype ProjectAssetsJson = ProjectAssetsJson
  { targets :: Map.Map Text (Map.Map Text DependencyInfo)
  }
  deriving (Show)

instance FromJSON ProjectAssetsJson where
  parseJSON = withObject "ProjectAssetsJson" $ \obj ->
    ProjectAssetsJson <$> obj .: "targets"

data DependencyInfo = DependencyInfo
  { depType :: Text
  , deepDeps :: Map.Map Text Text
  }
  deriving (Show)

instance FromJSON DependencyInfo where
  parseJSON = withObject "Dependency" $ \obj ->
    DependencyInfo <$> obj .: "type"
      <*> obj .:? "dependencies" .!= Map.empty

data NuGetDep = NuGetDep
  { depName :: Text
  , depVersion :: Text
  , completeDepType :: Text
  , completeDeepDeps :: Map.Map Text Text
  }
  deriving (Show)

buildGraph :: ProjectAssetsJson -> Graphing Dependency
buildGraph project = unfold direct deepList toDependency
  where
    direct :: [NuGetDep]
    direct = concatMap (mapMaybe convertDep . Map.toList) (Map.elems (targets project))

    convertDep :: (Text, DependencyInfo) -> Maybe NuGetDep
    convertDep (depString, dep) = case Text.splitOn "/" depString of
      [name, ver] -> Just $ NuGetDep name ver (depType dep) (deepDeps dep)
      _ -> Nothing

    deepList nugetDep = (\(x, y) -> NuGetDep x y "" Map.empty) <$> Map.toList (completeDeepDeps nugetDep)
    toDependency NuGetDep{..} =
      Dependency
        { dependencyType = NuGetType
        , dependencyName = depName
        , dependencyVersion = Just (CEq depVersion)
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }
