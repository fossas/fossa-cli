module Strategy.Yarn
  ( discover
  ) where

import Control.Effect.Diagnostics
import Discovery.Walk
import Effect.ReadFS
import qualified Graphing as G
import Path
import Types
import Prelude
import qualified Strategy.Node.YarnLock as YarnLock

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has ReadFS rsig run, Has Diagnostics rsig run) => Path Abs Dir -> m [DiscoveredProject run]
discover dir = context "Yarn" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [YarnProject]
findProjects = walk' $ \dir _ files -> do
  case findFileNamed "yarn.lock" files of
    Nothing -> pure ([], WalkSkipSome ["node_modules"])
    Just lock -> do
      let project =
            YarnProject
            { yarnDir = dir
            , yarnLock = lock
            }

      pure ([project], WalkSkipSome ["node_modules"])

mkProject :: (Has ReadFS sig n, Has Diagnostics sig n) => YarnProject -> DiscoveredProject n
mkProject project =
  DiscoveredProject
    { projectType = "yarn",
      projectBuildTargets = mempty,
      projectDependencyGraph = const $ getDeps project,
      projectPath = yarnDir project,
      projectLicenses = pure []
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => YarnProject -> m (G.Graphing Dependency)
getDeps = context "Yarn" . context "Static analysis" . YarnLock.analyze' . yarnLock

data YarnProject = YarnProject
  { yarnDir :: Path Abs Dir
  , yarnLock :: Path Abs File
  } deriving (Eq, Ord, Show)
