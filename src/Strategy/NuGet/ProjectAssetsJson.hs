{-# LANGUAGE RecordWildCards #-}

module Strategy.NuGet.ProjectAssetsJson (
  discover,
  findProjects,
  getDeps,
  mkProject,
  buildGraph,
  ProjectAssetsJson (..),
) where

import Control.Effect.Diagnostics
import Data.Aeson
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import DepTypes
import Discovery.Walk
import Effect.ReadFS
import Graphing (Graphing, unfold)
import Path
import Types

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has ReadFS rsig run, Has Diagnostics rsig run) => Path Abs Dir -> m [DiscoveredProject run]
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
  deriving (Eq, Ord, Show)

mkProject :: (Has ReadFS sig n, Has Diagnostics sig n) => ProjectAssetsJsonProject -> DiscoveredProject n
mkProject project =
  DiscoveredProject
    { projectType = "projectassetsjson"
    , projectBuildTargets = mempty
    , projectDependencyGraph = const $ getDeps project
    , projectPath = parent $ projectAssetsJsonFile project
    , projectLicenses = pure []
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => ProjectAssetsJsonProject -> m (Graphing Dependency, GraphBreadth)
getDeps = context "ProjectAssetsJson" . context "Static analysis" . analyze' . projectAssetsJsonFile

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency, GraphBreadth)
analyze' file = do
  assetsJson <- readContentsJson @ProjectAssetsJson file
  graph <- context "Building dependency graph" $ pure (buildGraph assetsJson)
  pure (graph, Complete)

newtype ProjectAssetsJson = ProjectAssetsJson
  { targets :: M.Map Text (M.Map Text DependencyInfo)
  }
  deriving (Show)

instance FromJSON ProjectAssetsJson where
  parseJSON = withObject "ProjectAssetsJson" $ \obj ->
    ProjectAssetsJson <$> obj .: "targets"

data DependencyInfo = DependencyInfo
  { depType :: Text
  , deepDeps :: M.Map Text Text
  }
  deriving (Show)

instance FromJSON DependencyInfo where
  parseJSON = withObject "Dependency" $ \obj ->
    DependencyInfo <$> obj .: "type"
      <*> obj .:? "dependencies" .!= M.empty

data NuGetDep = NuGetDep
  { depName :: Text
  , depVersion :: Text
  , completeDepType :: Text
  , completeDeepDeps :: M.Map Text Text
  }
  deriving (Show)

buildGraph :: ProjectAssetsJson -> Graphing Dependency
buildGraph project = unfold direct deepList toDependency
  where
    direct :: [NuGetDep]
    direct = concatMap (mapMaybe convertDep . M.toList) (M.elems (targets project))

    convertDep :: (Text, DependencyInfo) -> Maybe NuGetDep
    convertDep (depString, dep) = case T.splitOn "/" depString of
      [name, ver] -> Just $ NuGetDep name ver (depType dep) (deepDeps dep)
      _ -> Nothing

    deepList nugetDep = (\(x, y) -> NuGetDep x y "" M.empty) <$> M.toList (completeDeepDeps nugetDep)
    toDependency NuGetDep{..} =
      Dependency
        { dependencyType = NuGetType
        , dependencyName = depName
        , dependencyVersion = Just (CEq depVersion)
        , dependencyLocations = []
        , dependencyEnvironments = []
        , dependencyTags = M.empty
        }
