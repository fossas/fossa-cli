module Strategy.Yarn (
  discover,
) where

import App.Fossa.Analyze.Types (AnalyzeProject (..))
import Control.Effect.Diagnostics
import Data.Aeson (ToJSON)
import Discovery.Walk
import Effect.ReadFS
import GHC.Generics (Generic)
import Path
import Strategy.Yarn.V1.YarnLock qualified as V1
import Strategy.Yarn.V2.YarnLock qualified as V2
import Types
import Prelude

discover :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [DiscoveredProject YarnProject]
discover dir = context "Yarn" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [YarnProject]
findProjects = walk' $ \dir _ files -> do
  case findFileNamed "yarn.lock" files of
    Nothing -> pure ([], WalkSkipSome ["node_modules"])
    Just lock -> do
      let project =
            YarnProject
              { yarnDir = dir
              , yarnLock = lock
              }

      pure ([project], WalkSkipSome ["node_modules"])

mkProject :: YarnProject -> DiscoveredProject YarnProject
mkProject project =
  DiscoveredProject
    { projectType = "yarn"
    , projectBuildTargets = mempty
    , projectData = project
    , projectPath = yarnDir project
    --, projectLicenses = pure [] -- FIXME
    }

instance AnalyzeProject YarnProject where
  analyzeProject _ = getDeps

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => YarnProject -> m DependencyResults
getDeps project = context "Yarn" $ getDepsV1 project <||> getDepsV2 project

getDepsV1 :: (Has ReadFS sig m, Has Diagnostics sig m) => YarnProject -> m DependencyResults
getDepsV1 project = do
  graph <- V1.analyze . yarnLock $ project
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [yarnLock project]
      }

getDepsV2 :: (Has ReadFS sig m, Has Diagnostics sig m) => YarnProject -> m DependencyResults
getDepsV2 project = do
  graph <- V2.analyze . yarnLock $ project
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [yarnLock project]
      }

data YarnProject = YarnProject
  { yarnDir :: Path Abs Dir
  , yarnLock :: Path Abs File
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON YarnProject
