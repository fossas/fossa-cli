module Strategy.Glide
  ( discover,
  )
where

import Control.Effect.Diagnostics (Diagnostics)
import Control.Monad.IO.Class
import Discovery.Walk
import Effect.ReadFS
import Graphing
import Path
import qualified Strategy.Go.GlideLock as GlideLock
import Types

discover :: MonadIO m => Path Abs Dir -> m [DiscoveredProject]
discover dir = map mkProject <$> findProjects dir

findProjects :: MonadIO m => Path Abs Dir -> m [GlideProject]
findProjects = walk' $ \dir _ files -> do
  case findFileNamed "glide.lock" files of
    Nothing -> pure ([], WalkContinue)
    Just lockfile -> pure ([GlideProject lockfile dir], WalkSkipAll)

data GlideProject = GlideProject
  { glideLock :: Path Abs File,
    glideDir :: Path Abs Dir
  }

mkProject :: GlideProject -> DiscoveredProject
mkProject project =
  DiscoveredProject
    { projectType = "glide",
      projectBuildTargets = mempty,
      projectDependencyGraph = const . runReadFSIO $ getDeps project,
      projectPath = glideDir project,
      projectLicenses = pure []
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => GlideProject -> m (Graphing Dependency)
getDeps project = GlideLock.analyze' (glideLock project)
