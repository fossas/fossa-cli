module Strategy.BerkeleyDB (
  discover,
  findProjects,
  mkProject,
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject), analyzeProject')
import Container.OsRelease (OsInfo (..))
import Control.Carrier.Diagnostics (fatalText)
import Control.Effect.Diagnostics (Diagnostics, ToDiagnostic, context, warnOnErr)
import Control.Effect.Lift (Lift)
import Control.Effect.Reader (Reader)
import Control.Monad (unless, void)
import Data.Aeson (ToJSON)
import Data.Either (partitionEithers)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Diag.Diagnostic (ToDiagnostic (renderDiagnostic))
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue),
  findFileNamed,
  walkWithFilters',
 )
import Effect.Exec (Exec)
import Effect.ReadFS (Has, ReadFS)
import GHC.Generics (Generic)
import Graphing (Graphing, directs)
import Path (Abs, Dir, File, Path, toFilePath)
import Prettyprinter (Pretty (..))
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

newtype BdbParsingFailed = BdbParsingFailed Int
instance ToDiagnostic BdbParsingFailed where
  renderDiagnostic (BdbParsingFailed numPkgs) = pretty $ "Could not parse " <> show numPkgs <> " packages from berkleydb store!"

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
  ) =>
  Path Abs Dir ->
  Path Abs File ->
  OsInfo ->
  m (Graphing Dependency, GraphBreadth)
analyze _ file osInfo = do
  (parserErrs, installed) <- context ("read berkeleydb database file: " <> toText file) $ partitionEithers <$> readBerkeleyDB file

  -- show warning for all parsing errors
  unless (null parserErrs)
    $ void
      . warnOnErr (BdbParsingFailed $ length parserErrs)
    $ fatalText
    $ Text.unlines parserErrs

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
