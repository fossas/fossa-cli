module Strategy.Glide (
  discover,
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject'), analyzeProject)
import Control.Effect.Diagnostics (Diagnostics, context)
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON)
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue, WalkSkipAll),
  findFileNamed,
  walkWithFilters',
 )
import Effect.ReadFS (Has, ReadFS)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path)
import Strategy.Go.GlideLock qualified as GlideLock
import Types (
  DependencyResults,
  DiscoveredProject (..),
  DiscoveredProjectType (GlideProjectType),
 )

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [DiscoveredProject GlideProject]
discover = simpleDiscover findProjects mkProject GlideProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [GlideProject]
findProjects = walkWithFilters' $ \dir _ files -> do
  case findFileNamed "glide.lock" files of
    Nothing -> pure ([], WalkContinue)
    Just lockfile -> pure ([GlideProject lockfile dir], WalkSkipAll)

data GlideProject = GlideProject
  { glideLock :: Path Abs File
  , glideDir :: Path Abs Dir
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON GlideProject

instance AnalyzeProject GlideProject where
  analyzeProject _ = getDeps
  analyzeProject' _ = getDeps

mkProject :: GlideProject -> DiscoveredProject GlideProject
mkProject project =
  DiscoveredProject
    { projectType = GlideProjectType
    , projectBuildTargets = mempty
    , projectPath = glideDir project
    , projectData = project
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => GlideProject -> m DependencyResults
getDeps project = context "Glide" (GlideLock.analyze' (glideLock project))
