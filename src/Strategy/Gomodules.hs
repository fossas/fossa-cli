module Strategy.Gomodules (
  discover,
  findProjects,
  getDeps,
  mkProject,
) where

import Control.Effect.Diagnostics (Diagnostics, context, (<||>))
import Discovery.Walk
import Effect.Exec
import Effect.ReadFS
import Path (Abs, Dir, File, Path)
import Strategy.Go.GoList qualified as GoList
import Strategy.Go.Gomod qualified as Gomod
import Types

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has ReadFS rsig run, Has Exec rsig run, Has Diagnostics rsig run) => Path Abs Dir -> m [DiscoveredProject run]
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

mkProject :: (Has Exec sig n, Has ReadFS sig n, Has Diagnostics sig n) => GomodulesProject -> DiscoveredProject n
mkProject project =
  DiscoveredProject
    { projectType = "gomod"
    , projectBuildTargets = mempty
    , projectDependencyResults = const $ getDeps project
    , projectPath = gomodulesDir project
    , projectLicenses = pure []
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
