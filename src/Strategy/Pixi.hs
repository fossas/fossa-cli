module Strategy.Pixi (
  discover,
  findProjects,
  mkProject,
  getDeps,
  PixiProject (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProjectStaticOnly), analyzeProject)
import Control.Effect.Diagnostics (Diagnostics, Has, context)
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON)
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue, WalkSkipAll),
  findFileNamed,
  walkWithFilters',
 )
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path)
import Strategy.Pixi.PixiLock qualified as PixiLock
import Types (
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (PixiProjectType),
  GraphBreadth (Complete),
 )

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [DiscoveredProject PixiProject]
discover = simpleDiscover findProjects mkProject PixiProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [PixiProject]
findProjects = walkWithFilters' $ \dir _ files -> do
  case findFileNamed "pixi.lock" files of
    Nothing -> pure ([], WalkContinue)
    Just lockFile -> do
      let project =
            PixiProject
              { pixiDir = dir
              , pixiLock = lockFile
              }
      pure ([project], WalkSkipAll)

data PixiProject = PixiProject
  { pixiDir :: Path Abs Dir
  , pixiLock :: Path Abs File
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PixiProject

instance AnalyzeProject PixiProject where
  analyzeProject _ = getDeps
  analyzeProjectStaticOnly _ = getDeps

mkProject :: PixiProject -> DiscoveredProject PixiProject
mkProject project =
  DiscoveredProject
    { projectType = PixiProjectType
    , projectBuildTargets = mempty
    , projectPath = pixiDir project
    , projectData = project
    }

-- | pixi.lock pins every package for every environment and platform, so the
-- graph is complete without running pixi — unlike the conda strategy, which
-- needs `conda env create` to resolve anything.
getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => PixiProject -> m DependencyResults
getDeps project = context "Pixi" $ do
  graph <- PixiLock.analyze (pixiLock project)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [pixiLock project]
      }
