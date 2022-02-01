module Strategy.Glide (
  discover,
) where

import App.Fossa.Analyze.Types (AnalyzeProject, analyzeProject)
import Control.Effect.Diagnostics (Diagnostics, context)
import Data.Aeson (ToJSON)
import Discovery.Walk
import Effect.ReadFS
import GHC.Generics (Generic)
import Path
import Strategy.Go.GlideLock qualified as GlideLock
import Types

discover :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [DiscoveredProject GlideProject]
discover dir = context "Glide" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [GlideProject]
findProjects = walk' $ \dir _ files -> do
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
