module Strategy.BerkeleyDB (
  discover,
  findProjects,
  mkProject,
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject), analyzeProject')
import Container.OsRelease (OsInfo (..))
import Control.Effect.Diagnostics (Diagnostics, context)
import Control.Effect.Lift (Lift)
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue),
  findFileNamed,
  walkWithFilters',
 )
import Effect.Exec (Exec)
import Effect.Logger (Logger)
import Effect.ReadFS (Has, ReadFS)
import GHC.Generics (Generic)
import Graphing (Graphing, directs)
import Path (Abs, Dir, File, Path, toFilePath)
import Strategy.BerkeleyDB.Internal (BdbEntry (..), readBerkeleyDB)
import Types (
  DepType (LinuxRPM),
  Dependency (..),
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (BerkeleyDBProjectType),
  GraphBreadth (Complete),
  VerConstraint (CEq),
 )

data BerkeleyDatabase = BerkeleyDatabase
  { dbDir :: Path Abs Dir
  , dbFile :: Path Abs File
  , osInfo :: OsInfo
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON BerkeleyDatabase

instance AnalyzeProject BerkeleyDatabase where
  analyzeProject _ = getDeps
  analyzeProject' _ = getDeps

discover ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Reader AllFilters) sig m
  ) =>
  OsInfo ->
  Path Abs Dir ->
  m [DiscoveredProject BerkeleyDatabase]
discover osInfo = simpleDiscover (findProjects osInfo) mkProject BerkeleyDBProjectType

findProjects ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Reader AllFilters) sig m
  ) =>
  OsInfo ->
  Path Abs Dir ->
  m [BerkeleyDatabase]
findProjects osInfo = walkWithFilters' $ \dir _ files -> do
  case findFileNamed "Packages" files of
    Nothing -> pure ([], WalkContinue)
    Just file -> do
      if (Text.isInfixOf "var/lib/rpm/" $ toText . toFilePath $ file)
        then pure ([BerkeleyDatabase dir file osInfo], WalkContinue)
        else pure ([], WalkContinue)

mkProject :: BerkeleyDatabase -> DiscoveredProject BerkeleyDatabase
mkProject project =
  DiscoveredProject
    { projectType = BerkeleyDBProjectType
    , projectBuildTargets = mempty
    , projectPath = dbDir project
    , projectData = project
    }

getDeps ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has Logger sig m
  ) =>
  BerkeleyDatabase ->
  m DependencyResults
getDeps project = do
  (graph, graphBreadth) <- analyze (dbDir project) (dbFile project) (osInfo project)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = graphBreadth
      , dependencyManifestFiles = [dbFile project]
      }

analyze ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has Logger sig m
  ) =>
  Path Abs Dir ->
  Path Abs File ->
  OsInfo ->
  m (Graphing Dependency, GraphBreadth)
analyze _ file osInfo = do
  installed <- context ("read berkeleydb database file: " <> toText file) $ readBerkeleyDB file
  context "building graph of packages" $ pure (buildGraph osInfo installed, Complete)

buildGraph :: OsInfo -> [BdbEntry] -> Graphing Dependency
buildGraph (OsInfo os osVersion) = directs . map toDependency
  where
    toDependency :: BdbEntry -> Dependency
    toDependency pkg =
      Dependency
        LinuxRPM
        (bdbEntryPackage pkg <> "#" <> os <> "#" <> osVersion)
        (Just $ version pkg)
        mempty
        mempty
        mempty

    version :: BdbEntry -> VerConstraint
    version pkg = CEq $ (bdbEntryArch pkg) <> "#" <> epoch pkg <> (bdbEntryVersion pkg)

    epoch :: BdbEntry -> Text
    epoch BdbEntry{bdbEntryEpoch} = maybe "" (<> ":") bdbEntryEpoch
