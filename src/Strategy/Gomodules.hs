module Strategy.Gomodules (
  discover,
  findProjects,
  getDeps,
  mkProject,
) where

import App.Fossa.Analyze.Types (AnalyzeProject, analyzeProject)
import Control.Effect.Diagnostics (Diagnostics, context, (<||>))
import Data.Aeson (ToJSON)
import Discovery.Walk
import Effect.Exec
import Effect.ReadFS
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path)
import Strategy.Go.GoList qualified as GoList
import Strategy.Go.Gomod qualified as Gomod
import Types

discover :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [DiscoveredProject GomodulesProject]
discover dir = context "Gomodules" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [GomodulesProject]
findProjects = walk' $ \dir _ files -> do
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

mkProject :: GomodulesProject -> DiscoveredProject GomodulesProject
mkProject project =
  DiscoveredProject
    { projectType = "gomod"
    , projectBuildTargets = mempty
    , projectPath = gomodulesDir project
    , projectData = project
    }

getDeps :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => GomodulesProject -> m DependencyResults
getDeps project = do
  (graph, graphBreadth) <-
    context "Gomodules" $
      context "Dynamic analysis" (GoList.analyze' (gomodulesDir project))
        <||> context "Static analysis" (Gomod.analyze' (gomodulesGomod project))
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = graphBreadth
      , dependencyManifestFiles = [gomodulesGomod project]
      }
