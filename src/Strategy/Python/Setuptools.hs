module Strategy.Python.Setuptools (
  discover,
  findProjects,
  getDeps,
  mkProject,
  SetuptoolsProject (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject'), analyzeProject)
import Control.Effect.Diagnostics (Diagnostics, context)
import Control.Effect.Diagnostics qualified as Diag
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON)
import Data.List (isInfixOf, isSuffixOf)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue),
  fileName,
  findFileNamed,
  walkWithFilters',
 )
import Effect.Exec (Exec)
import Effect.ReadFS (Has, ReadFS)
import GHC.Generics (Generic)
import Graphing (Graphing)
import Path (Abs, Dir, File, Path)
import Strategy.Python.Pip (PythonPackage, getPackages)
import Strategy.Python.ReqTxt qualified as ReqTxt
import Strategy.Python.SetupPy qualified as SetupPy
import Types (
  Dependency,
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (SetuptoolsProjectType),
  GraphBreadth (Partial),
 )

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [DiscoveredProject SetuptoolsProject]
discover = simpleDiscover findProjects mkProject SetuptoolsProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [SetuptoolsProject]
findProjects = walkWithFilters' $ \dir _ files -> do
  let reqTxtFiles =
        filter
          (\f -> "req" `isInfixOf` fileName f && ".txt" `isSuffixOf` fileName f)
          files

  let setupPyFile = findFileNamed "setup.py" files
  let setupCfgFile = findFileNamed "setup.cfg" files

  let project =
        SetuptoolsProject
          { setuptoolsReqTxt = reqTxtFiles
          , setuptoolsSetupPy = setupPyFile
          , setuptoolsSetupCfg = setupCfgFile
          , setuptoolsDir = dir
          }

  case (reqTxtFiles, setupPyFile) of
    ([], Nothing) -> pure ([], WalkContinue)
    _ -> pure ([project], WalkContinue)

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m, Has Exec sig m) => SetuptoolsProject -> m DependencyResults
getDeps project = do
  packages <- getPackages (setuptoolsDir project)
  graph <-
    context "Setuptools" $
      Diag.combineSuccessful @Text @Text
        "Analysis failed for all requirements.txt/setup.py in the project"
        "Failed to parse python file"
        [analyzeReqTxts packages project, analyzeSetupPy packages project]
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Partial
      , dependencyManifestFiles = maybeToList (setuptoolsSetupPy project) ++ setuptoolsReqTxt project
      }

getDeps' :: (Has ReadFS sig m, Has Diagnostics sig m) => SetuptoolsProject -> m DependencyResults
getDeps' project = do
  graph <-
    context "Setuptools" $
      Diag.combineSuccessful @Text @Text
        "Analysis failed for all requirements.txt/setup.py in the project"
        "Failed to parse python file"
        [analyzeReqTxts Nothing project, analyzeSetupPy Nothing project]
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Partial
      , dependencyManifestFiles = maybeToList (setuptoolsSetupPy project) ++ setuptoolsReqTxt project
      }

analyzeReqTxts :: (Has ReadFS sig m, Has Diagnostics sig m) => Maybe ([PythonPackage]) -> SetuptoolsProject -> m (Graphing Dependency)
analyzeReqTxts packages project = context "Analyzing requirements.txt files" $ do
  mconcat <$> traverse (ReqTxt.analyze' packages) (setuptoolsReqTxt project)

analyzeSetupPy :: (Has ReadFS sig m, Has Diagnostics sig m) => Maybe ([PythonPackage]) -> SetuptoolsProject -> m (Graphing Dependency)
analyzeSetupPy packages project = context "Analyzing setup.py" $ do
  setupPy <- Diag.fromMaybeText "No setup.py found in this project" (setuptoolsSetupPy project)
  SetupPy.analyze' packages setupPy (setuptoolsSetupCfg project)

data SetuptoolsProject = SetuptoolsProject
  { setuptoolsReqTxt :: [Path Abs File]
  , setuptoolsSetupPy :: Maybe (Path Abs File)
  , setuptoolsSetupCfg :: Maybe (Path Abs File)
  , setuptoolsDir :: Path Abs Dir
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON SetuptoolsProject

instance AnalyzeProject SetuptoolsProject where
  analyzeProject _ = getDeps
  analyzeProject' _ = getDeps'

mkProject :: SetuptoolsProject -> DiscoveredProject SetuptoolsProject
mkProject project =
  DiscoveredProject
    { projectType = SetuptoolsProjectType
    , projectBuildTargets = mempty
    , projectPath = setuptoolsDir project
    , projectData = project
    }
