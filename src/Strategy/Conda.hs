module Strategy.Conda (
  discover,
) where

import Control.Carrier.Diagnostics hiding (fromMaybe)
import Discovery.Walk
import Effect.Exec
import Effect.ReadFS
import Path
import Strategy.Conda.CondaList qualified as CondaList
import Strategy.Conda.EnvironmentYml qualified as EnvironmentYml
import Types

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has ReadFS rsig run, Has Exec rsig run, Has Diagnostics rsig run) => Path Abs Dir -> m [DiscoveredProject run]
discover dir = map mkProject <$> findProjects dir

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [CondaProject]
findProjects = walk' $ \dir _ files -> do
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
  deriving (Eq, Ord, Show)

mkProject :: (Has ReadFS sig m, Has Exec sig m, Has Diagnostics sig m) => CondaProject -> DiscoveredProject m
mkProject project =
  DiscoveredProject
    { projectType = "conda"
    , projectBuildTargets = mempty
    , projectDependencyResults = const $ getDeps project
    , projectPath = condaDir project
    , projectLicenses = pure []
    }

-- Prefer analyzeCondaList over analyzeEnvironmentYml, results shoudln't be combined, it's either/or.
-- There might be a dep with a version spec in an environment.yml file: i.e. conda+foo$1.2.*, and perhaps
-- the same dep resolved to a known version in the users virtual environment: i.e. conda+foo$1.2.4 (we get that form conda list).
-- If we combined the results then we would include both of those deps in the result, which is not correct behavior.
getDeps :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => CondaProject -> m DependencyResults
getDeps project = analyzeCondaList project <||> analyzeEnvironmentYml project

analyzeCondaList :: (Has Exec sig m, Has Diagnostics sig m) => CondaProject -> m DependencyResults
analyzeCondaList project = do
  graph <- CondaList.analyze . condaDir $ project
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [condaEnvironmentYml project]
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
