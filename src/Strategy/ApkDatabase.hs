module Strategy.ApkDatabase (
  discover,
  findProjects,
  mkProject,
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject, analyzeProject'))
import Container.OsRelease (OsInfo)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON)
import Data.String.Conversion (toText)
import Data.Text qualified as Text
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue),
  findFileNamed,
  walkWithFilters',
 )
import Effect.ReadFS (Has, ReadFS)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path)
import Strategy.AlpineLinux.Apk (analyze)
import Types (
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (AlpineDatabaseProjectType),
 )

data AlpineDatabase = AlpineDatabase
  { dbDir :: Path Abs Dir
  , dbFile :: Path Abs File
  , osInfo :: OsInfo
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON AlpineDatabase

instance AnalyzeProject AlpineDatabase where
  analyzeProject _ = getDeps
  analyzeProject' _ = getDeps

discover ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Reader AllFilters) sig m
  ) =>
  OsInfo ->
  Path Abs Dir ->
  m [DiscoveredProject AlpineDatabase]
discover osInfo = simpleDiscover (findProjects osInfo) mkProject AlpineDatabaseProjectType

findProjects ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Reader AllFilters) sig m
  ) =>
  OsInfo ->
  Path Abs Dir ->
  m [AlpineDatabase]
findProjects osInfo = walkWithFilters' $ \dir _ files -> do
  case findFileNamed "installed" files of
    Nothing -> pure ([], WalkContinue)
    Just file ->
      if (Text.isInfixOf "apk/" $ toText file)
        then pure ([AlpineDatabase dir file osInfo], WalkContinue)
        else pure ([], WalkContinue)

mkProject :: AlpineDatabase -> DiscoveredProject AlpineDatabase
mkProject project =
  DiscoveredProject
    { projectType = AlpineDatabaseProjectType
    , projectBuildTargets = mempty
    , projectPath = dbDir project
    , projectData = project
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => AlpineDatabase -> m DependencyResults
getDeps project = do
  (graph, graphBreadth) <- analyze (dbDir project) (dbFile project) (osInfo project)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = graphBreadth
      , dependencyManifestFiles = [dbFile project]
      }
