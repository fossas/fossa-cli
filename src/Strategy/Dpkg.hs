module Strategy.Dpkg (
  discover,
  findProjects,
  mkProject,
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject, analyzeProjectStaticOnly))
import Container.OsRelease (OsInfo)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON)
import Data.Foldable (find)
import Data.String.Conversion (toText)
import Data.Text qualified as Text
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue),
  findFileNamed,
  walkWithFilters',
 )
import Effect.ReadFS (Has, ReadFS, listDir)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path, toFilePath)
import Strategy.Dpkg.Database (analyze)
import Types (
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (DpkgDatabaseProjectType),
 )

data DpkgDatabase = DpkgDatabase
  { dbDir :: Path Abs Dir
  , dbFile :: Path Abs File
  , osInfo :: OsInfo
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON DpkgDatabase

instance AnalyzeProject DpkgDatabase where
  analyzeProject _ = getDeps
  analyzeProjectStaticOnly _ = getDeps

discover ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Reader AllFilters) sig m
  ) =>
  OsInfo ->
  Path Abs Dir ->
  m [DiscoveredProject DpkgDatabase]
discover osInfo = simpleDiscover (findProjects osInfo) mkProject DpkgDatabaseProjectType

findProjects ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Reader AllFilters) sig m
  ) =>
  OsInfo ->
  Path Abs Dir ->
  m [DpkgDatabase]
findProjects osInfo = walkWithFilters' $ \dir dirs files -> do
  let standardDBs = case findFileNamed "status" files of
        Just file ->
          if Text.isInfixOf "var/lib/dpkg/" (toText . toFilePath $ file)
            then [DpkgDatabase dir file osInfo]
            else []
        Nothing -> []
  statusD_DBs <- case find (\f -> toFilePath f == "var/lib/dpkg/status.d/") dirs of
    Just dir' -> do
      (_, filesInDir) <- listDir dir'
      pure $ map (\file -> DpkgDatabase dir' file osInfo) (filter (not . Text.isSuffixOf ".md5sums" . toText) filesInDir)
    Nothing -> pure []
  pure (standardDBs ++ statusD_DBs, WalkContinue)

mkProject :: DpkgDatabase -> DiscoveredProject DpkgDatabase
mkProject project =
  DiscoveredProject
    { projectType = DpkgDatabaseProjectType
    , projectBuildTargets = mempty
    , projectPath = dbDir project
    , projectData = project
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => DpkgDatabase -> m DependencyResults
getDeps project = do
  (graph, graphBreadth) <- analyze (dbDir project) (dbFile project) (osInfo project)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = graphBreadth
      , dependencyManifestFiles = [dbFile project]
      }
