module Strategy.Yarn
  ( discover
  ) where

import Control.Effect.Diagnostics
import Control.Monad.IO.Class (MonadIO)
import Discovery.Walk
import Effect.ReadFS
import qualified Graphing as G
import Path
import Types
import Prelude
import qualified Strategy.Node.YarnLock as YarnLock

discover :: MonadIO m => Path Abs Dir -> m [DiscoveredProject]
discover dir = map mkProject <$> findProjects dir

findProjects :: MonadIO m => Path Abs Dir -> m [YarnProject]
findProjects = walk' $ \dir _ files -> do
  case findFileNamed "yarn.lock" files of
    Nothing -> pure ([], WalkContinue)
    Just lock -> do
      let project =
            YarnProject
            { yarnDir = dir
            , yarnLock = lock
            }

      pure ([project], WalkSkipAll)

mkProject :: YarnProject -> DiscoveredProject
mkProject project =
  DiscoveredProject
    { projectType = "yarn",
      projectBuildTargets = mempty,
      projectDependencyGraph = const . runReadFSIO $ getDeps project,
      projectPath = yarnDir project,
      projectLicenses = pure []
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => YarnProject -> m (G.Graphing Dependency)
getDeps = YarnLock.analyze' . yarnLock

data YarnProject = YarnProject
  { yarnDir :: Path Abs Dir
  , yarnLock :: Path Abs File
  } deriving (Eq, Ord, Show)
