module Strategy.Pdm (discover) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject'), analyzeProject)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON)
import Data.Maybe (isNothing)
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (WalkStep (WalkContinue, WalkSkipSome), findFileNamed, walkWithFilters')
import Effect.ReadFS (Has, ReadFS)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path)
import Strategy.Python.PDM.Pdm (analyze)
import Types (
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (PdmProjectType),
  GraphBreadth (Complete, Partial),
 )

discover ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Reader AllFilters) sig m
  ) =>
  Path Abs Dir ->
  m [DiscoveredProject PdmProject]
discover = simpleDiscover findProjects mkProject PdmProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [PdmProject]
findProjects = walkWithFilters' $ \dir _ files -> do
  let pyprojectFile = findFileNamed "pyproject.toml" files
  let pdmlockFile = findFileNamed "pdm.lock" files
  case (pyprojectFile) of
    Just pyprojectToml -> pure ([PdmProject pyprojectToml pdmlockFile dir], WalkSkipSome [".venv"])
    Nothing -> pure ([], WalkContinue)

data PdmProject = PdmProject
  { pyproject :: Path Abs File
  , pdmlock :: Maybe (Path Abs File)
  , pdmDir :: Path Abs Dir
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PdmProject

instance AnalyzeProject PdmProject where
  analyzeProject _ = getDeps
  analyzeProject' _ = getDeps

mkProject :: PdmProject -> DiscoveredProject PdmProject
mkProject project =
  DiscoveredProject
    { projectType = PdmProjectType
    , projectBuildTargets = mempty
    , projectPath = pdmDir project
    , projectData = project
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => PdmProject -> m DependencyResults
getDeps project = do
  graph <- analyze (pyproject project) (pdmlock project)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = if isNothing (pdmlock project) then Partial else Complete
      , dependencyManifestFiles = [pyproject project]
      }
