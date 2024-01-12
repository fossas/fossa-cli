{-# LANGUAGE TupleSections #-}

-- Description: Discovery and analysis functions for Sqlite3 backed RPM databases
module Strategy.Sqlite (
  SqliteDB (..),
  readSqliteDBPackages,
  discover,
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject, analyzeProjectStaticOnly))
import Container.OsRelease (OsInfo (..))
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, context, warn)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BLS
import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.Rpm.DbHeaderBlob (PkgConversionError, PkgInfo (..), pkgInfoToDependency, readPackageInfo)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Database.SQLite3 (ColumnIndex (ColumnIndex), StepResult (..), close, columnBlob, columnInt64, finalize, prepare, step)
import Database.SQLite3 qualified as SQLite
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (WalkStep (WalkContinue), findFirstMatchingFile, walkWithFilters')
import Effect.ReadFS (ReadFS, readContentsBS)
import GHC.Generics (Generic)
import Graphing (directs)
import Path (Abs, Dir, File, Path, parent, toFilePath)
import Path.IO (withSystemTempFile)
import Types (DependencyResults (..), DiscoveredProject (..), DiscoveredProjectType (SqliteDBProjectType), GraphBreadth (Complete))

data SqliteDB = SqliteDB
  { dbDir :: Path Abs Dir
  , dbFile :: Path Abs File
  , osInfo :: OsInfo
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON SqliteDB

discover ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Reader AllFilters) sig m
  ) =>
  OsInfo ->
  Path Abs Dir ->
  m [DiscoveredProject SqliteDB]
discover osInfo = simpleDiscover (findProjects osInfo) mkProject SqliteDBProjectType

findProjects ::
  ( Has ReadFS sig m
  , Has (Reader AllFilters) sig m
  , Has Diagnostics sig m
  ) =>
  OsInfo ->
  Path Abs Dir ->
  m [SqliteDB]
findProjects osInfo = walkWithFilters' $ \dir _ files -> do
  case findFirstMatchingFile ["Packages.sqlite", "rpmdb.sqlite"] files of
    Nothing -> pure ([], WalkContinue)
    Just file -> do
      if (Text.isInfixOf "var/lib/rpm/" $ toText . toFilePath $ file)
        then pure ([SqliteDB{dbDir = dir, dbFile = file, osInfo = osInfo}], WalkContinue)
        else pure ([], WalkContinue)

mkProject :: SqliteDB -> DiscoveredProject SqliteDB
mkProject db =
  DiscoveredProject
    { projectType = SqliteDBProjectType
    , projectBuildTargets = mempty
    , projectPath = parent (dbFile db)
    , projectData = db
    }

instance AnalyzeProject SqliteDB where
  analyzeProject _ = analyze
  analyzeProjectStaticOnly _ = analyze

data SqliteDBEntry = SqliteDBEntry
  { pkgName :: Text
  , pkgVersion :: Text
  , pkgRelease :: Text
  , pkgEpoch :: Maybe Text
  , pkgArch :: Text
  }
  deriving (Show)

analyze :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Lift IO) sig m) => SqliteDB -> m DependencyResults
analyze
  SqliteDB
    { osInfo = osInfo
    , dbFile = dbFile
    } =
    do
      context ("Reading packages from SqliteDB at " <> toText dbFile) $ do
        (conversionFailures, dependencies) <- (partitionEithers . map (pkgInfoToDependency osInfo)) <$> readSqliteDBPackages dbFile
        traverse_ mkConversionErrMsg conversionFailures
        context "Building graph of packages" $
          pure
            DependencyResults
              { dependencyGraph = directs dependencies
              , dependencyGraphBreadth = Complete
              , dependencyManifestFiles = [dbFile]
              }
    where
      mkConversionErrMsg :: Has Diagnostics sig m => PkgConversionError -> m ()
      mkConversionErrMsg e = warn $ "Discovered package is missing one of name, version, or architecture: " <> show e

readSqliteDBPackages :: (Has ReadFS sig m, Has (Lift IO) sig m, Has Diagnostics sig m) => Path Abs File -> m [PkgInfo]
readSqliteDBPackages sqlDbFile =
  do
    (parseFailures, packages) <- (partitionEithers . map parsePackageInfos) <$> (writeTempFileAndFetchPkgRows =<< readContentsBS sqlDbFile)
    traverse_ reportParseError parseFailures
    pure packages
  where
    parsePackageInfos :: (Int64, BS.ByteString) -> Either (Int64, String) PkgInfo
    parsePackageInfos (hnum, blob) = first (hnum,) (readPackageInfo . BLS.fromStrict $ blob)

    reportParseError :: Has Diagnostics sig m => (Int64, String) -> m ()
    reportParseError (index, errString) =
      warn
        ( "Couldn't read package number "
            <> show index
            <> " with error "
            <> errString
        )

writeTempFileAndFetchPkgRows ::
  Has (Lift IO) sig m =>
  -- | Bytestring for a sqlite package database
  BS.ByteString ->
  m [(Int64, BS.ByteString)]
writeTempFileAndFetchPkgRows sqliteBlob =
  sendIO $
    withSystemTempFile "sqlite-bs.db" $
      \path handle ->
        do
          BS.hPut handle sqliteBlob
          dbConn <- SQLite.open $ toText path
          statement <- prepare dbConn "select hnum, blob from Packages;"
          blobs <- retrieveBlobs statement
          finalize statement
          close dbConn
          pure blobs
  where
    retrieveBlob :: Has (Lift IO) sig m => SQLite.Statement -> m (Maybe (Int64, BS.ByteString))
    retrieveBlob statement =
      do
        res <- sendIO $ step statement
        case res of
          Row ->
            -- Per the docs: column* functions don't throw errors except when reading text.
            do
              hnum <- sendIO (columnInt64 statement (ColumnIndex 0))
              blob <- sendIO (columnBlob statement (ColumnIndex 1))
              pure . Just $ (hnum, blob)
          Done -> pure Nothing

    retrieveBlobs :: Has (Lift IO) sig m => SQLite.Statement -> m [(Int64, BS.ByteString)]
    retrieveBlobs statement = do
      maybeRow <- retrieveBlob statement
      case maybeRow of
        Nothing -> pure []
        Just row -> do
          rest <- retrieveBlobs statement
          pure $ row : rest
