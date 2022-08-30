{-# LANGUAGE RecordWildCards #-}

module Strategy.BerkeleyDB (
  discover,
  findProjects,
  mkProject,
  -- | For testing
  readBerkeleyDB,
  BdbEntry (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject))
import App.Fossa.EmbeddedBinary (BinaryPaths, toPath, withBerkeleyBinary)
import Container.OsRelease (OsInfo (..))
import Control.Effect.Diagnostics (Diagnostics, context, fatalText, fromEitherShow)
import Control.Effect.Lift (Lift)
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON)
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as BSL
import Data.Rpm.DbHeaderBlob (PkgInfo (..), readPackageInfo)
import Data.String.Conversion (ConvertUtf8 (encodeUtf8), toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue),
  findFileNamed,
  walkWithFilters',
 )
import Effect.Exec (AllowErr (Never), Command (..), Exec, execJson)
import Effect.ReadFS (Has, ReadFS)
import GHC.Generics (Generic)
import Graphing (Graphing, directs)
import Path (Abs, Dir, File, Path, toFilePath)
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
  }
  deriving (Eq, Ord, Show)

-- | FOSSA _requires_ that architecture is provided: https://github.com/fossas/FOSSA/blob/e61713dec1ef80dc6b6114f79622c14df5278235/modules/fetchers/README.md#locators-for-linux-packages
parsePkgInfo :: (Has Diagnostics sig m) => PkgInfo -> m BdbEntry
parsePkgInfo PkgInfo{pkgArch = Just (pkgArch), ..} = pure $ BdbEntry pkgArch pkgName (pkgVersion <> "-" <> pkgRelease)
parsePkgInfo pkg = fatalText . toText $ "package '" <> show pkg <> "' does not have an architecture, which is required"

analyze ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Exec sig m
  ) =>
  Path Abs Dir ->
  Path Abs File ->
  OsInfo ->
  m (Graphing Dependency, GraphBreadth)
analyze dir file osInfo = do
  installed <- context ("read berkeleydb database file: " <> toText file) $ readBerkeleyDB dir file
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
    version pkg = CEq $ (bdbEntryArch pkg) <> "#" <> (bdbEntryVersion pkg)

-- | Packages are read as a JSON array of base64 strings.
readBerkeleyDB ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Exec sig m
  ) =>
  Path Abs Dir ->
  Path Abs File ->
  m [BdbEntry]
readBerkeleyDB dir file = withBerkeleyBinary $ \bdb -> do
  (bdbJsonOutput :: [Text]) <- context "read raw blobs" . execJson dir $ bdbCommand bdb file
  bdbByteOutput <- context "decode base64" . traverse fromEitherShow $ B64.decode <$> fmap encodeUtf8 bdbJsonOutput
  entries <- context "parse blobs" . traverse fromEitherShow $ readPackageInfo <$> fmap BSL.fromStrict bdbByteOutput
  context "parse package info" $ traverse parsePkgInfo entries

bdbCommand :: BinaryPaths -> Path Abs File -> Command
bdbCommand bin file =
  Command
    { cmdName = toText $ toPath bin
    , cmdArgs = ["--target", toText file]
    , cmdAllowErr = Never
    }
