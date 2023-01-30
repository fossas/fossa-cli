-- | Mix, the Elixir dependency manager.
module Strategy.Mix (
  discover,
  findProjects,
  mkProject,
  MixProject (..),
) where

import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Reader (Reader)
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue, WalkSkipSome),
  findFileNamed,
  walkWithFilters',
 )
import Effect.ReadFS (Has, ReadFS)
import Path (Abs, Dir, Path)
import Strategy.Elixir.MixTree (MixProject (..))
import Types (DiscoveredProject (..), DiscoveredProjectType (MixProjectType))

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [DiscoveredProject MixProject]
discover = simpleDiscover findProjects mkProject MixProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [MixProject]
findProjects = walkWithFilters' $ \dir _ files -> do
  case findFileNamed "mix.exs" files of
    Nothing -> pure ([], WalkContinue)
    Just file -> pure ([MixProject dir file], WalkSkipSome ["deps", "_build"])

mkProject :: MixProject -> DiscoveredProject MixProject
mkProject project =
  DiscoveredProject
    { projectType = MixProjectType
    , projectBuildTargets = mempty
    , projectPath = mixDir project
    , projectData = project
    }
