module Strategy.Cocoapods (
  discover,
  findProjects,
  getDeps,
  mkProject,
) where

import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, context, (<||>))
import Control.Effect.Diagnostics qualified as Diag
import Discovery.Walk
import Effect.ReadFS
import Graphing
import Path
import Strategy.Cocoapods.Podfile qualified as Podfile
import Strategy.Cocoapods.PodfileLock qualified as PodfileLock
import Types

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has ReadFS rsig run, Has Diagnostics rsig run) => Path Abs Dir -> m [DiscoveredProject run]
discover dir = context "Cocoapods" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [CocoapodsProject]
findProjects = walk' $ \dir _ files -> do
  let podfile = findFileNamed "Podfile" files
  let podfileLock = findFileNamed "Podfile.lock" files

  let project =
        CocoapodsProject
          { cocoapodsPodfile = podfile
          , cocoapodsPodfileLock = podfileLock
          , cocoapodsDir = dir
          }

  case podfile <|> podfileLock of
    Nothing -> pure ([], WalkContinue)
    Just _ -> pure ([project], WalkContinue)

data CocoapodsProject = CocoapodsProject
  { cocoapodsPodfile :: Maybe (Path Abs File)
  , cocoapodsPodfileLock :: Maybe (Path Abs File)
  , cocoapodsDir :: Path Abs Dir
  }
  deriving (Eq, Ord, Show)

mkProject :: (Has ReadFS sig n, Has Diagnostics sig n) => CocoapodsProject -> DiscoveredProject n
mkProject project =
  DiscoveredProject
    { projectType = "cocoapods"
    , projectBuildTargets = mempty
    , projectDependencyGraph = const $ getDeps project
    , projectPath = cocoapodsDir project
    , projectLicenses = pure []
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => CocoapodsProject -> m (Graphing Dependency, GraphBreadth)
getDeps project =
  context "Cocoapods" $
    context "Podfile.lock analysis" (analyzePodfileLock project) <||> context "Podfile analysis" (analyzePodfile project)

analyzePodfile :: (Has ReadFS sig m, Has Diagnostics sig m) => CocoapodsProject -> m (Graphing Dependency, GraphBreadth)
analyzePodfile project = do
  graph <- Diag.fromMaybeText "No Podfile present in the project" (cocoapodsPodfile project) >>= Podfile.analyze'
  pure (graph, Partial)

analyzePodfileLock :: (Has ReadFS sig m, Has Diagnostics sig m) => CocoapodsProject -> m (Graphing Dependency, GraphBreadth)
analyzePodfileLock project = do
  graph <- Diag.fromMaybeText "No Podfile.lock present in the project" (cocoapodsPodfileLock project) >>= PodfileLock.analyze'
  pure (graph, Complete)
