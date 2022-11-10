{-# LANGUAGE RecordWildCards #-}
module Strategy.Conda (
  discover,
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject'), analyzeProject)
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  fatalText,
  (<||>),
 )
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON)
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue, WalkSkipAll),
  findFileNamed,
  walkWithFilters',
 )
import Effect.Exec (Exec)
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path)
import Strategy.Conda.CondaList qualified as CondaList
import Strategy.Conda.EnvironmentYml qualified as EnvironmentYml
import Types (
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (CondaProjectType),
  GraphBreadth (Complete),
 )

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [DiscoveredProject CondaProject]
discover = simpleDiscover findProjects mkProject CondaProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [CondaProject]
findProjects = walkWithFilters' $ \dir _ files -> do
  case findFileNamed "environment.yml" files of
    Nothing -> pure ([], WalkContinue)
    Just envYml -> do
      let project =
            CondaProject
              { condaDir = dir
              , condaEnvironmentYml = envYml
              }
      pure ([project], WalkSkipAll) -- Once we find an environment.yml file, skip the rest of the walk

data CondaProject = CondaProject
  { condaDir :: Path Abs Dir
  , condaEnvironmentYml :: Path Abs File
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON CondaProject

instance AnalyzeProject CondaProject where
  analyzeProject _ = getDeps
  analyzeProject' _ = const $ fatalText "Cannot analyze Conda project statically."

mkProject :: CondaProject -> DiscoveredProject CondaProject
mkProject project =
  DiscoveredProject
    { projectType = CondaProjectType
    , projectBuildTargets = mempty
    , projectPath = condaDir project
    , projectData = project
    }

-- Prefer analyzeCondaList over analyzeEnvironmentYml, results shoudln't be combined, it's either/or.
-- There might be a dep with a version spec in an environment.yml file: i.e. conda+foo$1.2.*, and perhaps
-- the same dep resolved to a known version in the users virtual environment: i.e. conda+foo$1.2.4 (we get that form conda list).
-- If we combined the results then we would include both of those deps in the result, which is not correct behavior.
getDeps :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => CondaProject -> m DependencyResults
getDeps project = analyzeCondaList project <||> analyzeEnvironmentYml project

analyzeCondaList :: (Has Exec sig m, Has Diagnostics sig m) => CondaProject -> m DependencyResults
analyzeCondaList CondaProject{..} = do
  graph <- CondaList.analyze condaDir condaEnvironmentYml
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [condaEnvironmentYml]
      }

analyzeEnvironmentYml :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => CondaProject -> m DependencyResults
analyzeEnvironmentYml project = do
  graph <- EnvironmentYml.analyze . condaEnvironmentYml $ project
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [condaEnvironmentYml project]
      }
