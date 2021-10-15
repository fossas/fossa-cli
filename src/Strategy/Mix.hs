module Strategy.Mix (
  discover,
  findProjects,
  mkProject,
) where

import Control.Effect.Diagnostics (Diagnostics, context)
import Discovery.Walk (
  WalkStep (WalkContinue, WalkSkipSome),
  findFileNamed,
  walk',
 )
import Effect.ReadFS (Has, ReadFS)
import Path
import Strategy.Elixir.MixTree (MixProject (..))
import Types (DiscoveredProject (..))

discover :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [DiscoveredProject MixProject]
discover dir = context "Mix" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [MixProject]
findProjects = walk' $ \dir _ files -> do
  case findFileNamed "mix.exs" files of
    Nothing -> pure ([], WalkContinue)
    Just file -> pure ([MixProject dir file], WalkSkipSome ["deps", "_build"])

mkProject :: MixProject -> DiscoveredProject MixProject
mkProject project =
  DiscoveredProject
    { projectType = "mix"
    , projectBuildTargets = mempty
    , projectPath = mixDir project
    , projectData = project
    }
