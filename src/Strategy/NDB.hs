module Strategy.NDB (
  discover,
  findProjects,
  mkProject,
) where

import App.Fossa.Analyze.Types (AnalyzeProject (..))
import Container.OsRelease (OsInfo (..))
import Control.Effect.Diagnostics (Diagnostics, context)
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
import Effect.ReadFS (Has, ReadFS)
import GHC.Generics (Generic)
import Graphing (Graphing, directs)
import Path (Abs, Dir, File, Path, toFilePath)
import Strategy.NDB.Internal (NdbEntry (..), readNDB)
import Types (
  DepType (LinuxRPM),
  Dependency (..),
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (..),
  GraphBreadth (Complete),
  VerConstraint (CEq),
  insertTag,
 )

data NdbLocation = NdbLocation
  { dbDir :: Path Abs Dir
  , dbFile :: Path Abs File
  , osInfo :: OsInfo
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON NdbLocation

instance AnalyzeProject NdbLocation where
  analyzeProject _ = getDeps
  analyzeProjectStaticOnly _ = getDeps

discover ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Reader AllFilters) sig m
  ) =>
  OsInfo ->
  Path Abs Dir ->
  m [DiscoveredProject NdbLocation]
discover osInfo = simpleDiscover (findProjects osInfo) mkProject NDBProjectType

findProjects ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Reader AllFilters) sig m
  ) =>
  OsInfo ->
  Path Abs Dir ->
  m [NdbLocation]
findProjects osInfo = walkWithFilters' $ \dir _ files -> do
  case findFileNamed "Packages.db" files of
    Nothing -> pure ([], WalkContinue)
    Just file -> do
      if isSupportedPath file
        then pure ([NdbLocation dir file osInfo], WalkContinue)
        else pure ([], WalkContinue)
  where
    -- The standard location for this is '/var/lib/rpm/',
    -- but some distros in some versions (e.g. openSUSE) symlink this elsewhere
    -- (the example seen for openSUSE is 'usr/lib/sysimage/rpm/').
    --
    -- For maximal compatibility while still being reasonably confident that this is the package database
    -- for the system RPM install, this function just checks whether the file is a child of any directory that
    -- contains the word 'rpm'.
    --
    -- We may want to consider making walk work with symlinks (so that we are confident we're using the right file)
    -- or unconditionally trying any matching named file.
    isSupportedPath :: Path Abs File -> Bool
    isSupportedPath = Text.isInfixOf "rpm" . toText . toFilePath

mkProject :: NdbLocation -> DiscoveredProject NdbLocation
mkProject project =
  DiscoveredProject
    { projectType = NDBProjectType
    , projectBuildTargets = mempty
    , projectPath = dbDir project
    , projectData = project
    }

getDeps ::
  ( Has Diagnostics sig m
  , Has ReadFS sig m
  ) =>
  NdbLocation ->
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
  , Has ReadFS sig m
  ) =>
  Path Abs Dir ->
  Path Abs File ->
  OsInfo ->
  m (Graphing Dependency, GraphBreadth)
analyze _ file osInfo = do
  installed <- context ("read database file: " <> toText file) $ readNDB file
  context "building graph of packages" $ pure (buildGraph osInfo installed, Complete)

buildGraph :: OsInfo -> [NdbEntry] -> Graphing Dependency
buildGraph (OsInfo os osVersion) = directs . map toDependency
  where
    toDependency :: NdbEntry -> Dependency
    toDependency pkg =
      let baseDep =
            Dependency
              LinuxRPM
              (ndbEntryPackage pkg <> "#" <> os <> "#" <> osVersion)
              (Just $ version pkg)
              mempty
              mempty
              mempty
          withLicense = case ndbEntryLicense pkg of
            Just license -> insertTag "license" license baseDep
            Nothing -> baseDep
       in withLicense

    version :: NdbEntry -> VerConstraint
    version pkg = CEq $ (ndbEntryArch pkg) <> "#" <> epoch pkg <> (ndbEntryVersion pkg)

    epoch :: NdbEntry -> Text
    epoch NdbEntry{ndbEntryEpoch} = maybe "" (<> ":") ndbEntryEpoch
