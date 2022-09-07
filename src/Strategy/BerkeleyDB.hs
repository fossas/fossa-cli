module Strategy.BerkeleyDB (
  discover,
  findProjects,
  mkProject,
  -- | For testing
  readBerkeleyDB,
  BdbEntry (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject), analyzeProject')
import App.Fossa.EmbeddedBinary (BinaryPaths, toPath, withBerkeleyBinary)
import Container.OsRelease (OsInfo (..))
import Control.Effect.Diagnostics (Diagnostics, context, fatalOnSomeException, fatalText, fromEitherShow)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON)
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as BSL
import Data.Rpm.DbHeaderBlob (PkgInfo (..), readPackageInfo)
import Data.String.Conversion (ConvertUtf8 (encodeUtf8), decodeUtf8, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue),
  findFileNamed,
  walkWithFilters',
 )
import Effect.Exec (AllowErr (Never), Command (..), Exec, execJson')
import Effect.ReadFS (Has, ReadFS, readContentsBS)
import GHC.Generics (Generic)
import Graphing (Graphing, directs)
import Path (Abs, Dir, File, Path, parseAbsDir, toFilePath)
import System.Directory (getCurrentDirectory)
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

-- | An entry in the database, consisting of the architecture, package, and version.
data BdbEntry = BdbEntry
  { bdbEntryArch :: Text
  , bdbEntryPackage :: Text
  , bdbEntryVersion :: Text
  , bdbEntryEpoch :: Maybe Text
  }
  deriving (Eq, Ord, Show)

-- | FOSSA _requires_ that architecture is provided: https://github.com/fossas/FOSSA/blob/e61713dec1ef80dc6b6114f79622c14df5278235/modules/fetchers/README.md#locators-for-linux-packages
parsePkgInfo :: (Has Diagnostics sig m) => PkgInfo -> m BdbEntry
parsePkgInfo (PkgInfo (Just pkgName) (Just pkgVersion) (Just pkgRelease) (Just pkgArch) pkgEpoch) = pure $ BdbEntry pkgArch pkgName (pkgVersion <> "-" <> pkgRelease) (fmap (toText . show) pkgEpoch)
parsePkgInfo pkg = fatalText . toText $ "package '" <> show pkg <> "' is missing one or more fields; all fields are required"

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

-- | Packages are read as a JSON array of base64 strings.
readBerkeleyDB ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Exec sig m
  ) =>
  Path Abs File ->
  m [BdbEntry]
readBerkeleyDB file = withBerkeleyBinary $ \bin -> do
  -- Get the process working directory, not the one that 'ReadFS' reports, because 'ReadFS' is inside of the tarball.
  cwd <- getSystemCwd

  -- Read the file content from disk and send it to the parser via stdin.
  -- This is necessary because the parser doesn't have access to the file system being read.
  fileContent <- context "read file content" $ readContentsBS file

  -- Handle the JSON response.
  (bdbJsonOutput :: [Text]) <- context "read raw blobs" . execJson' cwd (bdbCommand bin) . decodeUtf8 $ B64.encode fileContent
  bdbByteOutput <- context "decode base64" . traverse fromEitherShow $ B64.decode <$> fmap encodeUtf8 bdbJsonOutput
  entries <- context "parse blobs" . traverse fromEitherShow $ readPackageInfo <$> fmap BSL.fromStrict bdbByteOutput
  context "parse package info" $ traverse parsePkgInfo entries

bdbCommand :: BinaryPaths -> Command
bdbCommand bin =
  Command
    { cmdName = toText $ toPath bin
    , cmdArgs = []
    , cmdAllowErr = Never
    }

getSystemCwd :: (Has (Lift IO) sig m, Has Diagnostics sig m) => m (Path Abs Dir)
getSystemCwd = do
  cwd <- fatalOnSomeException "get process working directory" $ sendIO getCurrentDirectory
  fatalOnSomeException "parse cwd as path" . sendIO $ parseAbsDir cwd
