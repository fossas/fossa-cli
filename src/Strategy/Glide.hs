module Strategy.Glide (
  discoverVisitor,
  mkProject,
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProjectStaticOnly), analyzeProject)
import Control.Effect.Diagnostics (Diagnostics, context)
import Data.Aeson (ToJSON)
import Discovery.Walk (
  WalkStep (WalkContinue, WalkSkipAll),
  findFileNamed,
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

discoverVisitor :: (Has ReadFS sig m) => Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m ([GlideProject], WalkStep)
discoverVisitor dir _ files = do
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
  analyzeProjectStaticOnly _ = getDeps

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
