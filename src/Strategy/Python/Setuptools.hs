module Strategy.Python.Setuptools
  ( discover,
    findProjects,
    getDeps,
    mkProject,
  )
where

import Control.Carrier.Output.IO
import Control.Effect.Diagnostics (Diagnostics, context)
import qualified Control.Effect.Diagnostics as Diag
import Data.List (isInfixOf, isSuffixOf)
import Discovery.Walk
import Effect.ReadFS
import Graphing (Graphing)
import Path
import qualified Strategy.Python.ReqTxt as ReqTxt
import qualified Strategy.Python.SetupPy as SetupPy
import Types

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has ReadFS rsig run, Has Diagnostics rsig run) => Path Abs Dir -> m [DiscoveredProject run]
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
          { setuptoolsReqTxt = reqTxtFiles,
            setuptoolsSetupPy = setupPyFile,
            setuptoolsDir = dir
          }

  case (reqTxtFiles, setupPyFile) of
    ([], Nothing) -> pure ([], WalkContinue)
    _ -> pure ([project], WalkContinue)

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => SetuptoolsProject -> m (Graphing Dependency)
getDeps project = context "Setuptools" $
  Diag.combineSuccessful
    "Analysis failed for all requirements.txt/setup.py in the project"
    [analyzeReqTxts project, analyzeSetupPy project]

analyzeReqTxts :: (Has ReadFS sig m, Has Diagnostics sig m) => SetuptoolsProject -> m (Graphing Dependency)
analyzeReqTxts = context "Analyzing requirements.txt files" . fmap mconcat . traverse ReqTxt.analyze' . setuptoolsReqTxt

analyzeSetupPy :: (Has ReadFS sig m, Has Diagnostics sig m) => SetuptoolsProject -> m (Graphing Dependency)
analyzeSetupPy project = context "Analyzing setup.py" (Diag.fromMaybeText "No setup.py found in this project" (setuptoolsSetupPy project)) >>= SetupPy.analyze'

data SetuptoolsProject = SetuptoolsProject
  { setuptoolsReqTxt :: [Path Abs File],
    setuptoolsSetupPy :: Maybe (Path Abs File),
    setuptoolsDir :: Path Abs Dir
  }
  deriving (Eq, Ord, Show)

mkProject :: (Has ReadFS sig n, Has Diagnostics sig n) => SetuptoolsProject -> DiscoveredProject n
mkProject project =
  DiscoveredProject
    { projectType = "setuptools",
      projectBuildTargets = mempty,
      projectDependencyGraph = const $ getDeps project,
      projectPath = setuptoolsDir project,
      projectLicenses = pure []
    }
