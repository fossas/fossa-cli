module Strategy.Python.Setuptools
  ( discover,
  )
where

import Control.Carrier.Output.IO
import Control.Effect.Diagnostics (Diagnostics)
import qualified Control.Effect.Diagnostics as Diag
import Control.Monad.IO.Class
import Data.List (isInfixOf, isSuffixOf)
import Discovery.Walk
import Effect.ReadFS
import Graphing (Graphing)
import Path
import qualified Strategy.Python.ReqTxt as ReqTxt
import qualified Strategy.Python.SetupPy as SetupPy
import Types

discover :: MonadIO m => Path Abs Dir -> m [DiscoveredProject]
discover dir = map mkProject <$> findProjects dir

findProjects :: MonadIO m => Path Abs Dir -> m [SetuptoolsProject]
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
getDeps project =
  Diag.combineSuccessful
    "Analysis failed for all requirements.txt/setup.py in the project"
    [analyzeReqTxts project, analyzeSetupPy project]

analyzeReqTxts :: (Has ReadFS sig m, Has Diagnostics sig m) => SetuptoolsProject -> m (Graphing Dependency)
analyzeReqTxts = fmap mconcat . traverse ReqTxt.analyze' . setuptoolsReqTxt

analyzeSetupPy :: (Has ReadFS sig m, Has Diagnostics sig m) => SetuptoolsProject -> m (Graphing Dependency)
analyzeSetupPy project = Diag.fromMaybeText "No setup.py found in this project" (setuptoolsSetupPy project) >>= SetupPy.analyze'

data SetuptoolsProject = SetuptoolsProject
  { setuptoolsReqTxt :: [Path Abs File],
    setuptoolsSetupPy :: Maybe (Path Abs File),
    setuptoolsDir :: Path Abs Dir
  }
  deriving (Eq, Ord, Show)

mkProject :: SetuptoolsProject -> DiscoveredProject
mkProject project =
  DiscoveredProject
    { projectType = "setuptools",
      projectBuildTargets = mempty,
      projectDependencyGraph = const . runReadFSIO $ getDeps project,
      projectPath = setuptoolsDir project,
      projectLicenses = pure []
    }
