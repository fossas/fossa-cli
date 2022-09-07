{-# LANGUAGE RecordWildCards #-}

module Strategy.NuGet.ProjectJson (
  discover,
  findProjects,
  getDeps,
  mkProject,
  buildGraph,
  ProjectJson (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject'), analyzeProject)
import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, Has)
import Control.Effect.Reader (Reader)
import Data.Aeson.Types (
  FromJSON (parseJSON),
  Parser,
  ToJSON,
  Value,
  withObject,
  withText,
  (.:),
  (.:?),
 )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (
  DepType (NuGetType),
  Dependency (..),
  VerConstraint (CCompatible, CEq),
 )
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue),
  findFileNamed,
  walkWithFilters',
 )
import Effect.ReadFS (ReadFS, readContentsJson)
import GHC.Generics (Generic)
import Graphing (Graphing)
import Graphing qualified
import Path (Abs, Dir, File, Path, parent)
import Types (
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (ProjectJsonProjectType),
  GraphBreadth (Partial),
 )

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [DiscoveredProject ProjectJsonProject]
discover = simpleDiscover findProjects mkProject ProjectJsonProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [ProjectJsonProject]
findProjects = walkWithFilters' $ \_ _ files -> do
  case findFileNamed "project.json" files of
    Nothing -> pure ([], WalkContinue)
    Just file -> pure ([ProjectJsonProject file], WalkContinue)

newtype ProjectJsonProject = ProjectJsonProject
  { projectJsonFile :: Path Abs File
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ProjectJsonProject

instance AnalyzeProject ProjectJsonProject where
  analyzeProject _ = getDeps
  analyzeProject' _ = getDeps

mkProject :: ProjectJsonProject -> DiscoveredProject ProjectJsonProject
mkProject project =
  DiscoveredProject
    { projectType = ProjectJsonProjectType
    , projectBuildTargets = mempty
    , projectPath = parent $ projectJsonFile project
    , projectData = project
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => ProjectJsonProject -> m DependencyResults
getDeps = analyze' . projectJsonFile

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m DependencyResults
analyze' file = do
  graph <- buildGraph <$> readContentsJson @ProjectJson file
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Partial
      , dependencyManifestFiles = [file]
      }

newtype ProjectJson = ProjectJson
  { dependencies :: Map Text DependencyInfo
  }
  deriving (Show)

data DependencyInfo = DependencyInfo
  { depVersion :: Text
  , depType :: Maybe Text
  }
  deriving (Show)

instance FromJSON ProjectJson where
  parseJSON = withObject "ProjectJson" $ \obj ->
    ProjectJson <$> obj .: "dependencies"

instance FromJSON DependencyInfo where
  parseJSON val = parseJSONObject val <|> parseJSONText val
    where
      parseJSONObject :: Value -> Parser DependencyInfo
      parseJSONObject = withObject "DependencyInfo" $ \obj ->
        DependencyInfo
          <$> obj .: "version"
          <*> obj .:? "type"

      parseJSONText :: Value -> Parser DependencyInfo
      parseJSONText = withText "DependencyVersion" $ \text ->
        pure $ DependencyInfo text Nothing

data NuGetDependency = NuGetDependency
  { name :: Text
  , version :: Text
  , dependencyType :: Maybe Text
  }
  deriving (Show)

buildGraph :: ProjectJson -> Graphing Dependency
buildGraph project = Graphing.fromList (map toDependency direct)
  where
    direct = (\(name, dep) -> NuGetDependency name (depVersion dep) (depType dep)) <$> Map.toList (dependencies project)
    toDependency NuGetDependency{..} =
      Dependency
        { dependencyType = NuGetType
        , dependencyName = name
        , dependencyVersion = case Text.find ('*' ==) version of
            Just '*' -> Just (CCompatible version)
            _ -> Just (CEq version)
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = case dependencyType of
            Nothing -> Map.empty
            Just depType -> Map.insert "type" [depType] Map.empty
        }
