-- | Fpm, the fortran package manager.
module Strategy.Fpm (discover) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject'), analyzeProject)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON)
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (WalkStep (WalkContinue, WalkSkipSome), findFileNamed, walkWithFilters')
import Effect.ReadFS (Has, ReadFS)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path)
import Strategy.Fortran.FpmToml (analyzeFpmToml)
import Types (DependencyResults (..), DiscoveredProject (..), DiscoveredProjectType (FpmProjectType), GraphBreadth (Partial))

discover ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Reader AllFilters) sig m
  ) =>
  Path Abs Dir ->
  m [DiscoveredProject FpmProject]
discover = simpleDiscover findProjects mkProject FpmProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [FpmProject]
findProjects = walkWithFilters' $ \dir _ files -> do
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
  analyzeProject' _ = getDeps

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
