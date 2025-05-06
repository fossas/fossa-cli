module Strategy.Python.PDM.Pdm (
  discover,
  analyze,
) where

import Control.Effect.Diagnostics (Diagnostics)
import Data.Map qualified as Map
import Data.Text (Text)
import DepTypes (
  DepEnvironment (..),
  Dependency (..)
 )
import Effect.ReadFS (Has, ReadFS, readContentsToml)
import Graphing (Graphing, directs)
import Path (Abs, Dir, File, Path)
import Strategy.Python.Dependency (
  fromPDMDependency,
  toDependency
 )
import Strategy.Python.PDM.PdmLock (buildGraph)
import Strategy.Python.Poetry.PyProject (PyProject (..), PyProjectMetadata (..), PyProjectPdm (..), PyProjectTool (..))
import Strategy.Python.Util (Req (..))

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProjectStaticOnly), analyzeProject)
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON)
import Data.Maybe (isJust, isNothing)
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (WalkStep (WalkContinue, WalkSkipSome), findFileNamed, walkWithFilters')
import GHC.Generics (Generic)
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
  
  case (pyprojectFile, pdmlockFile) of
    -- If both pyproject.toml and pdm.lock are present, it's definitely a PDM project
    (Just pyprojectToml, Just pdmlock) -> 
      pure ([PdmProject pyprojectToml (Just pdmlock) dir], WalkSkipSome [".venv"])
    
    -- If only pyproject.toml is present, check if it contains PDM-specific markers
    (Just pyprojectToml, Nothing) -> do
      -- Try to read and parse the pyproject.toml
      pyproject <- readContentsToml pyprojectToml
      -- Only include this project if it has PDM sections
      if isPdmProject pyproject
        then pure ([PdmProject pyprojectToml Nothing dir], WalkSkipSome [".venv"])
        else pure ([], WalkContinue)
    
    -- No pyproject.toml, no PDM project
    _ -> pure ([], WalkContinue)
  
  where
    -- Check if a PyProject contains PDM-specific markers
    isPdmProject :: PyProject -> Bool
    isPdmProject pyproject = case pyprojectTool pyproject of
      Just (PyProjectTool{pyprojectPdm}) -> isJust pyprojectPdm
      Nothing -> False

data PdmProject = PdmProject
  { pyproject :: Path Abs File
  , pdmlock :: Maybe (Path Abs File)
  , pdmDir :: Path Abs Dir
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PdmProject

instance AnalyzeProject PdmProject where
  analyzeProject _ = getDeps
  analyzeProjectStaticOnly _ = getDeps

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

analyze ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs File ->
  Maybe (Path Abs File) ->
  m (Graphing Dependency)
analyze pyProjectToml pdmLockFile = do
  pyproject <- readContentsToml pyProjectToml

  -- According to PDM, optional dependencies are not
  -- prod dependencies, and they are not installed when,
  -- '--prod' flag is used with 'pdm install' command.
  -- https://github.com/pdm-project/pdm/pull/394/files
  --
  -- Moreover, by default, optional dependencies are not
  -- installed.
  let (prodReqs, optsReqs) = reqsFromPyProject pyproject
  let otherReqs = reqsFromPdmMetadata pyproject
  let devReqs = optsReqs <> otherReqs

  case pdmLockFile of
    Nothing ->
      pure . directs $
        (reqToDependency EnvProduction <$> prodReqs)
          ++ (reqToDependency EnvDevelopment <$> devReqs)
    Just pdmLockFile' -> do
      pdmLock <- readContentsToml pdmLockFile'
      pure $ buildGraph prodReqs devReqs pdmLock

reqToDependency :: DepEnvironment -> Req -> Dependency
reqToDependency env req = Strategy.Python.Dependency.toDependency $ fromPDMDependency env req

reqsFromPdmMetadata :: PyProject -> [Req]
reqsFromPdmMetadata pr = case pyprojectTool pr of
  Nothing -> mempty
  Just (PyProjectTool{pyprojectPdm}) -> case pyprojectPdm of
    Nothing -> mempty
    Just (PyProjectPdm{pdmDevDependencies}) -> case pdmDevDependencies of
      Nothing -> mempty
      Just reqs -> concat . Map.elems $ reqs

reqsFromPyProject :: PyProject -> ([Req], [Req])
reqsFromPyProject pr = case pyprojectProject pr of
  Nothing -> mempty
  Just p -> case (pyprojectDependencies p, pyprojectOptionalDependencies p) of
    (Nothing, Nothing) -> mempty
    (Just reqs, Nothing) -> (reqs, [])
    (Just reqs, Just optsReqs) -> (reqs, concat . Map.elems $ optsReqs)
    (Nothing, Just optsReqs) -> ([], concat . Map.elems $ optsReqs)
