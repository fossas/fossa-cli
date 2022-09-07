{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- Description: Discovery and analysis functions for Sqlite3 backed RPM databases
module Strategy.Sqlite (
  -- for testing
  SqliteDB (..),
  readDBPackages,
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject))
import Container.OsRelease (OsInfo (..))
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, context, warn)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Reader (Reader)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BLS
import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.Rpm.DbHeaderBlob.Internal (PkgInfo, readPackageInfo)
import Data.String.Conversion (toText)
import Database.SQLite3 (ColumnIndex (ColumnIndex), StepResult (..), close, columnBlob, columnInt64, finalize, prepare, step)
import Database.SQLite3 qualified as SQLite
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Effect.ReadFS (ReadFS, doesFileExist, readContentsBS)
import Path (Abs, Dir, File, Path, mkAbsFile, parent)
import Path.IO (withSystemTempFile)
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
    (parseFailures, packages) <- (partitionEithers . map parsePackageInfos) <$> (writeTempFileAndFetchPkgRows =<< readContentsBS dbFile)
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

-- TODO: Report hnum in the database of packages that failed to read
writeTempFileAndFetchPkgRows ::
  Has (Lift IO) sig m =>
  -- |Bytestring for a sqlite package database
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
