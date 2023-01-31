-- | Pub, the Dart dependency manager.
module Strategy.Pub (discover) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject'), analyzeProject)
import Control.Effect.Diagnostics (Diagnostics, errCtx, fatalText, recover, warnOnErr, (<||>))
import Control.Effect.Reader (Reader)
import Control.Monad (void)
import Data.Aeson (ToJSON)
import Diag.Common (
  MissingDeepDeps (MissingDeepDeps),
  MissingEdges (MissingEdges),
 )
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (WalkStep (WalkContinue), findFileNamed, walkWithFilters')
import Effect.Exec (Exec, Has)
import Effect.Logger (Logger)
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path)
import Strategy.Dart.Errors (PubspecLimitation (..))
import Strategy.Dart.PubDeps (analyzeDepsCmd)
import Strategy.Dart.PubSpec (analyzePubSpecFile)
import Strategy.Dart.PubSpecLock (analyzePubLockFile)
import Types (DependencyResults (..), DiscoveredProject (..), DiscoveredProjectType (PubProjectType))

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [DiscoveredProject PubProject]
discover = simpleDiscover findProjects mkProject PubProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [PubProject]
findProjects = walkWithFilters' $ \dir _ files -> do
  -- Note: pub does not support pubspec.yml naming - it must be pubspec.yaml.
  let pubSpecFile = findFileNamed "pubspec.yaml" files
  let pubSpecLockFile = findFileNamed "pubspec.lock" files

  case (pubSpecFile, pubSpecLockFile) of
    (Just specFile, Just lockFile) -> pure ([PubProject specFile (Just lockFile) dir], WalkContinue)
    (Just specFile, Nothing) -> pure ([PubProject specFile Nothing dir], WalkContinue)
    -- lockfile without manifest (pubspec.yaml) is not a dart project
    -- ref: https://dart.dev/guides/packages
    (Nothing, Just _) -> pure ([], WalkContinue)
    (Nothing, Nothing) -> pure ([], WalkContinue)

data PubProject = PubProject
  { pubSpec :: Path Abs File
  , pubLock :: Maybe (Path Abs File)
  , pubSpecDir :: Path Abs Dir
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PubProject

instance AnalyzeProject PubProject where
  analyzeProject _ = getDeps
  analyzeProject' _ = getDeps'

mkProject :: PubProject -> DiscoveredProject PubProject
mkProject project =
  DiscoveredProject
    { projectType = PubProjectType
    , projectBuildTargets = mempty
    , projectPath = pubSpecDir project
    , projectData = project
    }

getDeps :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m, Has Logger sig m) => PubProject -> m DependencyResults
getDeps project = do
  (graph, graphBreadth) <- case pubLock project of
    Just lockFile -> analyzeDepsCmd lockFile (pubSpecDir project) <||> analyzePubLockFile lockFile
    Nothing -> do
      void . recover
        $ warnOnErr MissingDeepDeps
          . warnOnErr MissingEdges
        $ errCtx PubspecLimitation (fatalText "Missing pubspec.lock file")
      analyzePubSpecFile $ pubSpec project
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = graphBreadth
      , dependencyManifestFiles = [pubSpec project]
      }

getDeps' :: (Has ReadFS sig m, Has Diagnostics sig m, Has Logger sig m) => PubProject -> m DependencyResults
getDeps' project = do
  (graph, graphBreadth) <- case pubLock project of
    Just lockFile -> analyzePubLockFile lockFile
    Nothing -> do
      void . recover
        $ warnOnErr MissingDeepDeps
          . warnOnErr MissingEdges
        $ errCtx PubspecLimitation (fatalText "Missing pubspec.lock file")
      analyzePubSpecFile $ pubSpec project
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = graphBreadth
      , dependencyManifestFiles = [pubSpec project]
      }
