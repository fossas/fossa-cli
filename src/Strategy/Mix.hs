-- | Mix, the Elixir dependency manager.
module Strategy.Mix (
  mkProject,
  MixProject (..),
  discoverVisitor,
) where

import Discovery.Walk (
  WalkStep (WalkContinue, WalkSkipSome),
  findFileNamed,
 )
import Effect.ReadFS (Has, ReadFS)
import Path (Abs, Dir, File, Path)
import Strategy.Elixir.MixTree (MixProject (..))
import Types (DiscoveredProject (..), DiscoveredProjectType (MixProjectType))

discoverVisitor :: (Has ReadFS sig m) => Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m ([MixProject], WalkStep)
discoverVisitor dir _ files = do
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
