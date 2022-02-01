module Strategy.Fpm (discover) where

import App.Fossa.Analyze.Types (AnalyzeProject, analyzeProject)
import Control.Effect.Diagnostics (Diagnostics, context)
import Data.Aeson (ToJSON)
import Discovery.Walk (WalkStep (WalkContinue, WalkSkipSome), findFileNamed, walk')
import Effect.ReadFS (Has, ReadFS)
import GHC.Generics (Generic)
import Path
import Strategy.Fortran.FpmToml (analyzeFpmToml)
import Types (DependencyResults (..), DiscoveredProject (..), DiscoveredProjectType (FpmProjectType), GraphBreadth (Partial))

discover ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  m [DiscoveredProject FpmProject]
discover dir = context "Fpm" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [FpmProject]
findProjects = walk' $ \dir _ files -> do
  let fmpSpecFile = findFileNamed "fpm.toml" files
  case (fmpSpecFile) of
    Just fpmToml -> pure ([FpmProject fpmToml dir], WalkSkipSome ["build"])
    Nothing -> pure ([], WalkContinue)

data FpmProject = FpmProject
  { fpmSpec :: Path Abs File
  , fpmSpecDir :: Path Abs Dir
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON FpmProject

instance AnalyzeProject FpmProject where
  analyzeProject _ = getDeps

mkProject :: FpmProject -> DiscoveredProject FpmProject
mkProject project =
  DiscoveredProject
    { projectType = FpmProjectType
    , projectBuildTargets = mempty
    , projectPath = fpmSpecDir project
    , projectData = project
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => FpmProject -> m DependencyResults
getDeps project = do
  graph <- analyzeFpmToml $ fpmSpec project
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Partial
      , dependencyManifestFiles = [fpmSpec project]
      }
