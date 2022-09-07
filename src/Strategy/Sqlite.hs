{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- Description: Discovery and analysis functions for Sqlite3 backed RPM databases
module Strategy.Sqlite
  (-- for testing
    SqliteDB(..)
  , readDBPackages) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject))
import Container.OsRelease (OsInfo (..))
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, context, warn)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Reader (Reader)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BLS
import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import Data.Rpm.DbHeaderBlob.Internal (PkgInfo, readPackageInfo)
import Data.String.Conversion (toText)
import Database.SQLite3 (ColumnIndex (ColumnIndex), StepResult (..), close, columnBlob, finalize, prepare, step)
import Database.SQLite3 qualified as SQLite
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Effect.ReadFS (ReadFS, doesFileExist, readContentsBS)
import Path (Abs, Dir, File, Path, mkAbsFile, parent)
import Path.IO (withSystemTempFile)
import System.IO (hFlush)
import Types (DependencyResults, DiscoveredProject (..), DiscoveredProjectType (SqliteRpmLinuxProjectType))

newtype SqliteDB = SqliteDB
  {dbFile :: Path Abs File}

rpmSqliteDbPath :: Path Abs File
rpmSqliteDbPath = $(mkAbsFile "/var/lib/rpm/Packages.sqlite")

discover ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Reader AllFilters) sig m
  ) =>
  OsInfo ->
  Path Abs Dir ->
  m [DiscoveredProject SqliteDB]
discover osInfo = simpleDiscover (findProject osInfo) mkProject SqliteRpmLinuxProjectType

findProject :: (Has ReadFS sig m) => OsInfo -> Path Abs Dir -> m [SqliteDB]
findProject _ _ =
  do
    exists <- doesFileExist rpmSqliteDbPath
    pure $
      if exists
        then [SqliteDB{dbFile = rpmSqliteDbPath}]
        else []

mkProject :: SqliteDB -> DiscoveredProject SqliteDB
mkProject db =
  DiscoveredProject
    { projectType = SqliteRpmLinuxProjectType
    , projectBuildTargets = mempty
    , projectPath = parent (dbFile db)
    , projectData = db
    }

instance AnalyzeProject SqliteDB where
  analyzeProject _ = getDeps

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Lift IO) sig m) => SqliteDB -> m DependencyResults
getDeps sqlDb@SqliteDB{..} =
  do
    context ("Reading package database found at " <> toText dbFile) $ readDBPackages sqlDb
    undefined

readDBPackages :: (Has ReadFS sig m, Has (Lift IO) sig m, Has Diagnostics sig m) => SqliteDB -> m [PkgInfo]
readDBPackages SqliteDB{..} =
  do
    (parseFailures, packages) <- (partitionEithers . readPackageInfos) <$> (writeTempFileAndFetchPkgRows =<< readContentsBS dbFile)
    traverse_ (warn . ("Failed reading package with " <>)) parseFailures
    pure packages
  where
    readPackageInfos :: [BS.ByteString] -> [Either String PkgInfo]
    readPackageInfos = map (readPackageInfo . BLS.fromStrict)

-- TODO: Report hnum in the database of packages that failed to read
writeTempFileAndFetchPkgRows ::
  Has (Lift IO) sig m =>
  -- |Bytestring for a sqlite package database
  BS.ByteString ->
  m [BS.ByteString]
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
    retrieveBlob :: Has (Lift IO) sig m => SQLite.Statement -> m (Maybe BS.ByteString)
    retrieveBlob statement =
      do
        res <- sendIO $ step statement
        case res of
          -- todo: error handling, report blob index
          Row -> Just <$> sendIO (columnBlob statement (ColumnIndex 1))
          Done -> pure Nothing

    retrieveBlobs :: Has (Lift IO) sig m => SQLite.Statement -> m [BS.ByteString]
    retrieveBlobs statement = do
      maybeRow <- retrieveBlob statement
      case maybeRow of
        Nothing -> pure []
        Just row -> do
          rest <- retrieveBlobs statement
          pure $ row : rest
