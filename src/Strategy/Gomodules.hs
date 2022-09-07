module Strategy.Gomodules (
  discover,
  findProjects,
  getDeps,
  mkProject,
  GomodulesProject (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject'), analyzeProject)
import Control.Effect.Diagnostics (Diagnostics, context, fatalText, recover, (<||>))
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON)
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkSkipSome),
  findFileNamed,
  walkWithFilters',
 )
import Effect.Exec (Exec, Has)
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Graphing (Graphing)
import Path (Abs, Dir, File, Path)
import Strategy.Go.GoList qualified as GoList
import Strategy.Go.GoModGraph qualified as GoModGraph
import Strategy.Go.Gomod qualified as Gomod
import Strategy.Go.Gostd (GoStdlibDep, filterGoStdlibPackages, listGoStdlibPackages)
import Types (
  Dependency,
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (GomodProjectType),
  GraphBreadth,
 )

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [DiscoveredProject GomodulesProject]
discover = simpleDiscover findProjects mkProject GomodProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [GomodulesProject]
findProjects = walkWithFilters' $ \dir _ files -> do
  case findFileNamed "go.mod" files of
    Nothing -> pure ([], WalkSkipSome ["vendor"])
    Just gomod -> pure ([GomodulesProject gomod dir], WalkSkipSome ["vendor"])

data GomodulesProject = GomodulesProject
  { gomodulesGomod :: Path Abs File
  , gomodulesDir :: Path Abs Dir
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON GomodulesProject

instance AnalyzeProject GomodulesProject where
  analyzeProject _ = getDeps
  analyzeProject' _ = const $ fatalText "Cannot analyze GoModule project statically"

mkProject :: GomodulesProject -> DiscoveredProject GomodulesProject
mkProject project =
  DiscoveredProject
    { projectType = GomodProjectType
    , projectBuildTargets = mempty
    , projectPath = gomodulesDir project
    , projectData = project
    }

getDeps :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => GomodulesProject -> m DependencyResults
getDeps project = do
  (graph, graphBreadth) <- context "Gomodules" $ dynamicAnalysis <||> staticAnalysis
  stdlib <- recover . context "Collect go standard library information" . listGoStdlibPackages $ gomodulesDir project
  pure $
    DependencyResults
      { dependencyGraph = filterGraph stdlib graph
      , dependencyGraphBreadth = graphBreadth
      , dependencyManifestFiles = [gomodulesGomod project]
      }
  where
    filterGraph :: Maybe [GoStdlibDep] -> Graphing Dependency -> Graphing Dependency
    filterGraph Nothing deps = deps
    filterGraph (Just stdlib) deps = filterGoStdlibPackages stdlib deps

    staticAnalysis :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => m (Graphing Dependency, GraphBreadth)
    staticAnalysis = context "Static analysis" (Gomod.analyze' (gomodulesGomod project))

    dynamicAnalysis :: (Has Exec sig m, Has Diagnostics sig m) => m (Graphing Dependency, GraphBreadth)
    dynamicAnalysis =
      context "Dynamic analysis" $
        context "analysis using go mod graph" (GoModGraph.analyze (gomodulesDir project))
          -- Go List tactic is only kept in consideration, in event go mod graph fails.
          -- In reality, this is highly unlikely scenario, and should almost never happen.
          <||> context "analysis using go list" (GoList.analyze' (gomodulesDir project))
