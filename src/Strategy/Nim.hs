module Strategy.Nim (
  mkProject,
  discoverVisitor,
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject, analyzeProjectStaticOnly))
import Control.Effect.Diagnostics (Diagnostics)
import Data.Aeson (ToJSON)
import Discovery.Walk (
  WalkStep (WalkContinue),
  findFileNamed,
 )
import Effect.Exec (Exec)
import Effect.ReadFS (Has, ReadFS)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path)
import Strategy.Nim.NimbleLock (analyze, analyze')
import Types (DependencyResults (..), DiscoveredProject (..), DiscoveredProjectType (NimbleProjectType))

data NimbleProject = NimbleProject
  { nimDir :: Path Abs Dir
  , nimbleLockFile :: Path Abs File
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON NimbleProject

instance AnalyzeProject NimbleProject where
  analyzeProject _ = getDeps
  analyzeProjectStaticOnly _ = getDepsStatically

discoverVisitor :: (Has ReadFS sig m) => Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m ([NimbleProject], WalkStep)
discoverVisitor dir _ files = do
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

getDepsStatically :: (Has ReadFS sig m, Has Diagnostics sig m) => NimbleProject -> m DependencyResults
getDepsStatically project = do
  (graph, graphBreadth) <- analyze' (nimDir project) (nimbleLockFile project)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = graphBreadth
      , dependencyManifestFiles = [nimbleLockFile project]
      }
