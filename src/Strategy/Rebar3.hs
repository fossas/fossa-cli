module Strategy.Rebar3 (
  RebarProject (..),
  discover,
  findProjects,
  getDeps,
  mkProject,
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject'), analyzeProject)
import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON)
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue, WalkSkipAll),
  findFileNamed,
  walkWithFilters',
 )
import Effect.Exec (Exec, Has)
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path)
import Strategy.Erlang.Rebar3Tree qualified as Rebar3Tree
import Types (
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (Rebar3ProjectType),
 )

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [DiscoveredProject RebarProject]
discover = simpleDiscover findProjects mkProject Rebar3ProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [RebarProject]
findProjects = walkWithFilters' $ \dir _ files -> do
  case findFileNamed "rebar.config" files of
    Nothing -> pure ([], WalkContinue)
    Just f -> pure ([RebarProject dir f], WalkSkipAll)

data RebarProject = RebarProject
  { rebarDir :: Path Abs Dir
  , rebarFile :: Path Abs File
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON RebarProject

instance AnalyzeProject RebarProject where
  analyzeProject _ = getDeps
  analyzeProject' _ = const $ fatalText "Cannot analyze Rebar3 project statically"

mkProject :: RebarProject -> DiscoveredProject RebarProject
mkProject project =
  DiscoveredProject
    { projectType = Rebar3ProjectType
    , projectBuildTargets = mempty
    , projectPath = rebarDir project
    , projectData = project
    }

getDeps :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => RebarProject -> m DependencyResults
getDeps project = do
  (graph, graphBreadth) <- Rebar3Tree.analyze' (rebarDir project)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = graphBreadth
      , dependencyManifestFiles = [rebarFile project]
      }
