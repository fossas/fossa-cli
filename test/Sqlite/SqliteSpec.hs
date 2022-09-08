{-# LANGUAGE TemplateHaskell #-}

module Sqlite.SqliteSpec (spec) where

import Data.Rpm.DbHeaderBlob (PkgInfo (..))
import Path (File, Path, Rel, mkRelFile, (</>))
import Path.IO (getCurrentDir)
import Strategy.Sqlite (readSqliteDBPackages)
import Test.Effect (it', shouldBe')
import Test.Hspec (Spec, context, describe, runIO)

spec :: Spec
spec = context "Sqlite DB" $
  do readDBPackagesSpec

singlePackageDB :: Path Rel File
singlePackageDB = $(mkRelFile "test/Sqlite/test_data/good_pkg_db.sqlite")

expectedPackage :: [PkgInfo]
expectedPackage =
  [ PkgInfo
      { pkgName = Just "libgcc"
      , pkgVersion = Just "11.2.1"
      , pkgRelease = Just "1.fc35"
      , pkgArch = Just "x86_64"
      , pkgEpoch = Nothing
      }
  , PkgInfo
      { pkgName = Just "gmp"
      , pkgVersion = Just "6.2.0"
      , pkgRelease = Just "7.fc35"
      , pkgArch = Just "x86_64"
      , pkgEpoch = Just 1
      }
  ]

badPackageDB :: Path Rel File
badPackageDB = $(mkRelFile "test/Sqlite/test_data/bad_db.sqlite")

readDBPackagesSpec :: Spec
readDBPackagesSpec = do
  currDir <- runIO getCurrentDir
  describe "readDBPackages" $ do
    it' "Reads from a package db with only a single package header" $
      do
        packages <- readSqliteDBPackages (currDir </> singlePackageDB)
        packages
          `shouldBe'` expectedPackage

    it' "Successfully reads remaining packages in a db with a bad blob" $
      do
        packages <- readSqliteDBPackages (currDir </> badPackageDB)
        packages `shouldBe'` expectedPackage
