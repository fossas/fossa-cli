module Strategy.Pub (discover) where

import App.Fossa.Analyze.Types (AnalyzeProject, analyzeProject)
import Control.Effect.Diagnostics (Diagnostics, context, (<||>))
import Data.Aeson (ToJSON)
import Discovery.Walk (WalkStep (WalkContinue), findFileNamed, walk')
import Effect.Exec (Exec, Has)
import Effect.Logger (Logger)
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Path
import Strategy.Dart.PubDeps (analyzeDepsCmd)
import Strategy.Dart.PubSpec (analyzePubSpecFile)
import Strategy.Dart.PubSpecLock (analyzePubLockFile)
import Types (DependencyResults (..), DiscoveredProject (..), DiscoveredProjectType (PubProjectType))

discover :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [DiscoveredProject PubProject]
discover dir = context "Pub" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [PubProject]
findProjects = walk' $ \dir _ files -> do
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
    Nothing -> analyzePubSpecFile $ pubSpec project
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = graphBreadth
      , dependencyManifestFiles = [pubSpec project]
      }
