module Strategy.Nim (
  discover,
  findProjects,
  mkProject,
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject))
import Control.Effect.Diagnostics (Diagnostics, context)
import Data.Aeson (ToJSON)
import Discovery.Walk (
  WalkStep (WalkContinue),
  findFileNamed,
  walk',
 )
import Effect.Exec (Exec)
import Effect.ReadFS (Has, ReadFS)
import GHC.Generics (Generic)
import Path
import Strategy.Nim.NimbleLock (analyze)
import Types (DependencyResults (..), DiscoveredProject (..), DiscoveredProjectType (NimbleProjectType))

data NimbleProject = NimbleProject
  { nimDir :: Path Abs Dir
  , nimbleLockFile :: Path Abs File
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON NimbleProject

instance AnalyzeProject NimbleProject where
  analyzeProject _ = getDeps

discover :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [DiscoveredProject NimbleProject]
discover dir = context "Nimble" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [NimbleProject]
findProjects = walk' $ \dir _ files -> do
  case findFileNamed "nimble.lock" files of
    Nothing -> pure ([], WalkContinue)
    Just file -> pure ([NimbleProject dir file], WalkContinue)

mkProject :: NimbleProject -> DiscoveredProject NimbleProject
mkProject project =
  DiscoveredProject
    { projectType = NimbleProjectType
    , projectBuildTargets = mempty
    , projectPath = nimDir project
    , projectData = project
    }

getDeps :: (Has ReadFS sig m, Has Exec sig m, Has Diagnostics sig m) => NimbleProject -> m DependencyResults
getDeps project = do
  (graph, graphBreadth) <- analyze (nimDir project) (nimbleLockFile project)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = graphBreadth
      , dependencyManifestFiles = [nimbleLockFile project]
      }
