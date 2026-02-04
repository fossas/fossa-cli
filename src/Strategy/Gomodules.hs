module Strategy.Gomodules (
  discover,
  findProjects,
  getDeps,
  mkProject,
  GomodulesProject (..),
)
where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProjectStaticOnly), analyzeProject)
import App.Util (guardStrictMode)
import Control.Effect.Diagnostics (Diagnostics, context, recover, (<||>))
import Control.Effect.Reader (Reader, ask)
import Data.Aeson (ToJSON)
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkSkipSome),
  findFileNamed,
  walkWithFilters',
 )
import Effect.Exec (Exec, GetDepsEffs, Has)
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Graphing (Graphing)
import Path (Abs, Dir, File, Path)
import Strategy.Go.GoListPackages qualified as GoListPackages
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
  analyzeProject _ proj = getDeps proj
  analyzeProjectStaticOnly _ = staticAnalysis

mkProject :: GomodulesProject -> DiscoveredProject GomodulesProject
mkProject project =
  DiscoveredProject
    { projectType = GomodProjectType
    , projectBuildTargets = mempty
    , projectPath = gomodulesDir project
    , projectData = project
    }

getDeps :: (GetDepsEffs sig m) => GomodulesProject -> m DependencyResults
getDeps project = do
  mode <- ask
  (graph, graphBreadth) <-
    context "Gomodules" $
      dynamicAnalysis <||> guardStrictMode mode goDotModAnalysis
  stdlib <- recover . context "Collect go standard library information" . listGoStdlibPackages $ gomodulesDir project
  pure $
    DependencyResults
      { dependencyGraph = filterGraph stdlib graph
      , dependencyGraphBreadth = graphBreadth
      , dependencyManifestFiles = [gomodulesGomod project]
      }
  where
    -- `go list -json -deps all` marks std lib deps with a boolean, so Strategy.Go.GoListPackages does this filtering itself.
    -- I think this can be removed when `go list -json -deps all` becomes the default.
    filterGraph :: Maybe [GoStdlibDep] -> Graphing Dependency -> Graphing Dependency
    filterGraph Nothing deps = deps
    filterGraph (Just stdlib) deps = filterGoStdlibPackages stdlib deps

    goDotModAnalysis :: (Has ReadFS sig m, Has Exec sig m, Has Diagnostics sig m) => m (Graphing Dependency, GraphBreadth)
    goDotModAnalysis = context "Go.mod analysis" (Gomod.analyze' (gomodulesGomod project))

    dynamicAnalysis :: (Has Exec sig m, Has Diagnostics sig m) => m (Graphing Dependency, GraphBreadth)
    dynamicAnalysis =
      context "Dynamic analysis" $
        context "analysis using go list (V3 Resolver)" (GoListPackages.analyze (gomodulesDir project))

staticAnalysis :: (Has Diagnostics sig m, Has ReadFS sig m) => GomodulesProject -> m DependencyResults
staticAnalysis proj = do
  let projectPath = gomodulesGomod proj
  (graph, breadth) <- context "Go.mod analysis (static)" $ Gomod.analyzeStatic projectPath
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = breadth
      , dependencyManifestFiles = [projectPath]
      }
