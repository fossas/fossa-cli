module Strategy.Yarn (
  discover,
) where

import Control.Effect.Diagnostics
import Discovery.Walk
import Effect.ReadFS
import Graphing qualified as G
import Path
import Strategy.Yarn.V1.YarnLock qualified as V1
import Strategy.Yarn.V2.YarnLock qualified as V2
import Types
import Prelude

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
    { projectType = "yarn"
    , projectBuildTargets = mempty
    , projectDependencyGraph = const $ getDeps project
    , projectPath = yarnDir project
    , projectLicenses = pure []
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => YarnProject -> m (G.Graphing Dependency)
getDeps project = context "Yarn" $ getDepsV1 project <||> getDepsV2 project

getDepsV1 :: (Has ReadFS sig m, Has Diagnostics sig m) => YarnProject -> m (G.Graphing Dependency)
getDepsV1 = V1.analyze . yarnLock

getDepsV2 :: (Has ReadFS sig m, Has Diagnostics sig m) => YarnProject -> m (G.Graphing Dependency)
getDepsV2 = V2.analyze . yarnLock

data YarnProject = YarnProject
  { yarnDir :: Path Abs Dir
  , yarnLock :: Path Abs File
  }
  deriving (Eq, Ord, Show)
