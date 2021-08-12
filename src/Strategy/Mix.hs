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
import Effect.Exec (Exec, Has)
import Effect.Logger (Logger (..))
import Effect.ReadFS (ReadFS)
import Path
import Strategy.Elixir.MixTree (MixProject (..), analyze)
import Types (DiscoveredProject (..))

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has Logger rsig run, Has Exec rsig run, Has Diagnostics rsig run) => Path Abs Dir -> m [DiscoveredProject run]
discover dir = context "Mix" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [MixProject]
findProjects = walk' $ \dir _ files -> do
  case findFileNamed "mix.exs" files of
    Nothing -> pure ([], WalkContinue)
    Just file -> pure ([MixProject dir file], WalkSkipSome ["deps", "_build"])

mkProject :: (Has Exec sig n, Has Diagnostics sig n, Has Logger sig n) => MixProject -> DiscoveredProject n
mkProject project =
  DiscoveredProject
    { projectType = "mix"
    , projectBuildTargets = mempty
    , projectDependencyResults = const $ analyze project
    , projectPath = mixDir project
    , projectLicenses = pure []
    }
