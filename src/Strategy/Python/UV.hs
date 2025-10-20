module Strategy.Python.UV (
  discover,
  findProjects,
) where

import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  context,
  errCtx,
  errHelp,
  recover,
  run,
  warnOnErr,
 )
import Control.Effect.Reader (Reader)
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue),
  findFileNamed,
  walkWithFilters',
 )
import Effect.ReadFS (ReadFS)
import Path (Abs, Dir, File, Path, parent)
import Types (
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (PipenvProjectType),
  GraphBreadth (Complete),
 )

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [DiscoveredProject UvProject]
discover = simpleDiscover findProjects mkProject PipenvProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [UvProject]
findProjects = walkWithFilters' $ \_ _ files -> do
  case findFileNamed "uv.lock" files of
    Nothing -> pure ([], WalkContinue)
    Just file -> pure ([UvProject file], WalkContinue)

newtype UvProject = UvProject
  { uvLockfile :: Path Abs File
  }
  deriving (Eq, Ord, Show)
