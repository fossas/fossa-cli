module Strategy.Python.Setuptools (
  discover,
  findProjects,
  getDeps,
  mkProject,
  SetuptoolsProject (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject, analyzeProject)
import Control.Carrier.Output.IO
import Control.Effect.Diagnostics (Diagnostics, context)
import Control.Effect.Diagnostics qualified as Diag
import Data.Aeson (ToJSON)
import Data.List (isInfixOf, isSuffixOf)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Discovery.Walk
import Effect.ReadFS
import GHC.Generics (Generic)
import Graphing (Graphing)
import Path
import Strategy.Python.ReqTxt qualified as ReqTxt
import Strategy.Python.SetupPy qualified as SetupPy
import Types

discover :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [DiscoveredProject SetuptoolsProject]
discover dir = context "Setuptools" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [SetuptoolsProject]
findProjects = walk' $ \dir _ files -> do
  let reqTxtFiles =
        filter
          (\f -> "req" `isInfixOf` fileName f && ".txt" `isSuffixOf` fileName f)
          files

  let setupPyFile = findFileNamed "setup.py" files

  let project =
        SetuptoolsProject
          { setuptoolsReqTxt = reqTxtFiles
          , setuptoolsSetupPy = setupPyFile
          , setuptoolsDir = dir
          }

  case (reqTxtFiles, setupPyFile) of
    ([], Nothing) -> pure ([], WalkContinue)
    _ -> pure ([project], WalkContinue)

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => SetuptoolsProject -> m DependencyResults
getDeps project = do
  graph <-
    context "Setuptools" $
      Diag.combineSuccessful @Text @Text
        "Analysis failed for all requirements.txt/setup.py in the project"
        "Failed to parse python file"
        [analyzeReqTxts project, analyzeSetupPy project]
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Partial
      , dependencyManifestFiles = maybeToList (setuptoolsSetupPy project) ++ setuptoolsReqTxt project
      }

analyzeReqTxts :: (Has ReadFS sig m, Has Diagnostics sig m) => SetuptoolsProject -> m (Graphing Dependency)
analyzeReqTxts = context "Analyzing requirements.txt files" . fmap mconcat . traverse ReqTxt.analyze' . setuptoolsReqTxt

analyzeSetupPy :: (Has ReadFS sig m, Has Diagnostics sig m) => SetuptoolsProject -> m (Graphing Dependency)
analyzeSetupPy project = context "Analyzing setup.py" $ do
  setupPy <- Diag.fromMaybeText "No setup.py found in this project" (setuptoolsSetupPy project)
  SetupPy.analyze' setupPy

data SetuptoolsProject = SetuptoolsProject
  { setuptoolsReqTxt :: [Path Abs File]
  , setuptoolsSetupPy :: Maybe (Path Abs File)
  , setuptoolsDir :: Path Abs Dir
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON SetuptoolsProject

instance AnalyzeProject SetuptoolsProject where
  analyzeProject _ = getDeps

mkProject :: SetuptoolsProject -> DiscoveredProject SetuptoolsProject
mkProject project =
  DiscoveredProject
    { projectType = SetuptoolsProjectType
    , projectBuildTargets = mempty
    , projectPath = setuptoolsDir project
    , projectData = project
    }
