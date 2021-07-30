module Strategy.Glide (
  discover,
) where

import Control.Effect.Diagnostics (Diagnostics, context)
import Discovery.Walk
import Effect.ReadFS
import Graphing
import Path
import Strategy.Go.GlideLock qualified as GlideLock
import Types

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has ReadFS rsig run, Has Diagnostics rsig run) => Path Abs Dir -> m [DiscoveredProject run]
discover dir = context "Glide" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [GlideProject]
findProjects = walk' $ \dir _ files -> do
  case findFileNamed "glide.lock" files of
    Nothing -> pure ([], WalkContinue)
    Just lockfile -> pure ([GlideProject lockfile dir], WalkSkipAll)

data GlideProject = GlideProject
  { glideLock :: Path Abs File
  , glideDir :: Path Abs Dir
  }

mkProject :: (Has ReadFS sig n, Has Diagnostics sig n) => GlideProject -> DiscoveredProject n
mkProject project =
  DiscoveredProject
    { projectType = "glide"
    , projectBuildTargets = mempty
    , projectDependencyGraph = const $ getDeps project
    , projectPath = glideDir project
    , projectLicenses = pure []
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => GlideProject -> m (Graphing Dependency, GraphBreadth)
getDeps project = context "Glide" (GlideLock.analyze' (glideLock project))
