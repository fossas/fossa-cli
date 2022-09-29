module Strategy.R (
  discover,
  findProjects,
  mkProject,
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject, analyzeProject'))
import Control.Effect.Diagnostics (Diagnostics, errCtx, fatalText, recover, warnOnErr)
import Control.Effect.Reader (Reader)
import Control.Monad (void)
import Data.Aeson (ToJSON)
import Diag.Common (
  MissingDeepDeps (MissingDeepDeps),
  MissingEdges (MissingEdges),
 )
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue, WalkSkipSome),
  findFileNamed,
  walkWithFilters',
 )
import Effect.ReadFS (Has, ReadFS)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path)
import Strategy.R.Errors (
  MissingRenvLockFile (MissingRenvLockFile),
  VersionConstraintsIgnored (VersionConstraintsIgnored),
 )
import Strategy.R.Renv (analyze)
import Types (
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (RProjectType),
 )

newtype RDescriptionFile = RDescriptionFile (Path Abs File)
  deriving (Show, Eq, Ord, Generic)

data RProject
  = Renv (Path Abs Dir) RDescriptionFile (Path Abs File)
  | ManifestOnly (Path Abs Dir) RDescriptionFile
  deriving (Show, Eq, Ord, Generic)

instance ToJSON RDescriptionFile
instance ToJSON RProject

instance AnalyzeProject RProject where
  analyzeProject _ = getDeps
  analyzeProject' _ = getDeps

discover ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Reader AllFilters) sig m
  ) =>
  Path Abs Dir ->
  m [DiscoveredProject RProject]
discover = simpleDiscover findProjects mkProject RProjectType

findProjects ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Reader AllFilters) sig m
  ) =>
  Path Abs Dir ->
  m [RProject]
findProjects = walkWithFilters' $ \dir _ files -> do
  let lockFile = findFileNamed "renv.lock" files
  let descriptionFile = findFileNamed "DESCRIPTION" files
  let skipRenvLibs = WalkSkipSome ["renv"]

  case (lockFile, descriptionFile) of
    (Just lockFile', Just description') -> do
      let project' = Renv dir (RDescriptionFile description') lockFile'
      pure ([project'], skipRenvLibs)
    (Nothing, Just description') -> do
      let project = ManifestOnly dir (RDescriptionFile description')
      pure ([project], skipRenvLibs)
    _ -> pure ([], WalkContinue)

mkProject :: RProject -> DiscoveredProject RProject
mkProject project =
  DiscoveredProject
    { projectType = RProjectType
    , projectBuildTargets = mempty
    , projectPath = projectDir project
    , projectData = project
    }
  where
    projectDir :: RProject -> Path Abs Dir
    projectDir (Renv dir _ _) = dir
    projectDir (ManifestOnly dir _) = dir

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => RProject -> m DependencyResults
getDeps (Renv _ (RDescriptionFile manifestFile) lockFile) = do
  (graph, graphBreadth) <- analyze manifestFile (Just lockFile)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = graphBreadth
      , dependencyManifestFiles = [manifestFile]
      }
getDeps (ManifestOnly _ (RDescriptionFile manifestFile)) = do
  void
    . recover
    . warnOnErr MissingEdges
    . warnOnErr MissingDeepDeps
    . warnOnErr VersionConstraintsIgnored
    . errCtx MissingRenvLockFile
    $ fatalText "renv.lock file is missing."

  (graph, graphBreadth) <- analyze manifestFile Nothing
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = graphBreadth
      , dependencyManifestFiles = [manifestFile]
      }
