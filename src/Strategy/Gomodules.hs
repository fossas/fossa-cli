module Strategy.Gomodules
  ( discover,
    findProjects,
    getDeps,
    mkProject,
  )
where

import Control.Effect.Diagnostics (Diagnostics, (<||>))
import Discovery.Walk
import Effect.Exec
import Effect.ReadFS
import Graphing
import Path
import qualified Strategy.Go.GoList as GoList
import qualified Strategy.Go.Gomod as Gomod
import Types

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has ReadFS rsig run, Has Exec rsig run, Has Diagnostics rsig run) => Path Abs Dir -> m [DiscoveredProject run]
discover dir = map mkProject <$> findProjects dir

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [GomodulesProject]
findProjects = walk' $ \dir _ files -> do
  case findFileNamed "go.mod" files of
    Nothing -> pure ([], WalkSkipSome ["vendor"])
    Just gomod -> pure ([GomodulesProject gomod dir], WalkSkipSome ["vendor"])

data GomodulesProject = GomodulesProject
  { gomodulesGomod :: Path Abs File,
    gomodulesDir :: Path Abs Dir
  }

mkProject :: (Has Exec sig n, Has ReadFS sig n, Has Diagnostics sig n) => GomodulesProject -> DiscoveredProject n
mkProject project =
  DiscoveredProject
    { projectType = "gomod",
      projectBuildTargets = mempty,
      projectDependencyGraph = const $ getDeps project,
      projectPath = gomodulesDir project,
      projectLicenses = pure []
    }

getDeps :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => GomodulesProject -> m (Graphing Dependency)
getDeps project =
  GoList.analyze' (gomodulesDir project)
    <||> Gomod.analyze' (gomodulesGomod project)
