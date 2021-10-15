module Strategy.Cocoapods (
  discover,
  findProjects,
  getDeps,
  mkProject,
) where

import App.Fossa.Analyze.Types (AnalyzeProject, analyzeProject)
import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, context, (<||>))
import Control.Effect.Diagnostics qualified as Diag
import Data.Aeson (ToJSON)
import Discovery.Walk
import Effect.ReadFS
import GHC.Generics (Generic)
import Path
import Strategy.Cocoapods.Podfile qualified as Podfile
import Strategy.Cocoapods.PodfileLock qualified as PodfileLock
import Types

discover :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [DiscoveredProject CocoapodsProject]
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
  deriving (Eq, Ord, Show, Generic)

instance ToJSON CocoapodsProject

instance AnalyzeProject CocoapodsProject where
  analyzeProject _ = getDeps

mkProject :: CocoapodsProject -> DiscoveredProject CocoapodsProject
mkProject project =
  DiscoveredProject
    { projectType = "cocoapods"
    , projectBuildTargets = mempty
    , projectPath = cocoapodsDir project
    , projectData = project
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => CocoapodsProject -> m DependencyResults
getDeps project =
  context "Cocoapods" $
    context "Podfile.lock analysis" (analyzePodfileLock project) <||> context "Podfile analysis" (analyzePodfile project)

analyzePodfile :: (Has ReadFS sig m, Has Diagnostics sig m) => CocoapodsProject -> m DependencyResults
analyzePodfile project = do
  podFile <- Diag.fromMaybeText "No Podfile present in the project" (cocoapodsPodfile project)
  graph <- Podfile.analyze' podFile
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Partial
      , dependencyManifestFiles = [podFile]
      }

analyzePodfileLock :: (Has ReadFS sig m, Has Diagnostics sig m) => CocoapodsProject -> m DependencyResults
analyzePodfileLock project = do
  lockFile <- Diag.fromMaybeText "No Podfile.lock present in the project" (cocoapodsPodfileLock project)
  graph <- PodfileLock.analyze' lockFile
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [lockFile]
      }
