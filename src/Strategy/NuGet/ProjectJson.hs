{-# LANGUAGE RecordWildCards #-}

module Strategy.NuGet.ProjectJson (
  discover,
  findProjects,
  getDeps,
  mkProject,
  buildGraph,
  looksLikeNuGetManifest,
  ProjectJson (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProjectStaticOnly), analyzeProject)
import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, Has, recover)
import Control.Effect.Reader (Reader)
import Data.Aeson.Types (
  FromJSON (parseJSON),
  Parser,
  ToJSON,
  Value,
  withObject,
  withText,
  (.!=),
  (.:),
  (.:?),
 )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
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
    Just file -> do
      isManifest <- isNuGetManifest file
      if isManifest
        then pure ([ProjectJsonProject file], WalkContinue)
        else pure ([], WalkContinue)

-- | Other tools also name their configuration file @project.json@ (e.g. Nx),
-- so only claim files that look like a NuGet manifest. A file that cannot be
-- read as a JSON object is still claimed, so that a malformed NuGet manifest
-- surfaces an analysis error rather than silently disappearing.
isNuGetManifest :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m Bool
isNuGetManifest file = do
  probe <- recover $ readContentsJson @(Map Text Value) file
  pure $ maybe True looksLikeNuGetManifest probe

-- | The legacy NuGet @project.json@ schema has no required key, but a manifest
-- always declares its packages under a top-level "dependencies" section, a
-- per-framework one inside "frameworks", or both.
looksLikeNuGetManifest :: Map Text Value -> Bool
looksLikeNuGetManifest obj = Map.member "dependencies" obj || Map.member "frameworks" obj

newtype ProjectJsonProject = ProjectJsonProject
  { projectJsonFile :: Path Abs File
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ProjectJsonProject

instance AnalyzeProject ProjectJsonProject where
  analyzeProject _ = getDeps
  analyzeProjectStaticOnly _ = getDeps

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
  { dependencies :: [(Text, DependencyInfo)]
  }
  deriving (Show)

data DependencyInfo = DependencyInfo
  { depVersion :: Text
  , depType :: Maybe Text
  }
  deriving (Eq, Ord, Show)

-- | Framework-specific settings from the @frameworks@ section; dependencies may
-- be declared per-framework instead of (or in addition to) top-level.
newtype FrameworkInfo = FrameworkInfo
  { frameworkDependencies :: Map Text DependencyInfo
  }
  deriving (Show)

-- The "dependencies" key is optional, both top-level and per-framework: a
-- project.json may declare dependencies in either place, or have none at all.
-- A package may also appear in several places with different versions (e.g. a
-- framework-specific override of a top-level entry); rather than picking one
-- winner, every distinct name/version/type combination is reported.
instance FromJSON ProjectJson where
  parseJSON = withObject "ProjectJson" $ \obj -> do
    topLevelDeps <- obj .:? "dependencies" .!= Map.empty
    frameworks :: Map Text FrameworkInfo <- obj .:? "frameworks" .!= Map.empty
    let frameworkDeps = concatMap (Map.toList . frameworkDependencies) (Map.elems frameworks)
    pure . ProjectJson . Set.toList . Set.fromList $ Map.toList topLevelDeps <> frameworkDeps

instance FromJSON FrameworkInfo where
  parseJSON = withObject "FrameworkInfo" $ \obj ->
    FrameworkInfo <$> obj .:? "dependencies" .!= Map.empty

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
    direct = (\(name, dep) -> NuGetDependency name (depVersion dep) (depType dep)) <$> dependencies project
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
