{-# LANGUAGE TemplateHaskell #-}

module Sqlite.SqliteSpec (spec) where

import Data.Rpm.DbHeaderBlob (PkgInfo (..))
import Path (File, Path, Rel, mkRelFile, (</>))
import Path.IO (getCurrentDir)
import Strategy.Sqlite (SqliteDB (SqliteDB), readDBPackages)
import Test.Effect (it', shouldBe')
import Test.Hspec (Spec, context, describe, fcontext, runIO)

spec :: Spec
spec = fcontext "Sqlite DB" $
  do readDBPackagesSpec

singlePackageDB :: Path Rel File
singlePackageDB = $(mkRelFile "test/Sqlite/test_data/single_pkg_db.sqlite")

expectedPackage :: [PkgInfo]
expectedPackage =
  [ PkgInfo
      { pkgName = Just "libgcc"
      , pkgVersion = Just "11.2.1"
      , pkgRelease = Just "1.fc35"
      , pkgArch = Just "x86_64"
      , pkgEpoch = Nothing
      }
  ]

badPackageDB :: Path Rel File
badPackageDB = $(mkRelFile "test/Sqlite/test_data/bad_db.sqlite")

-- include discovery in the future by storing these pkg dbs in a tar file
readDBPackagesSpec :: Spec
readDBPackagesSpec = do
  currDir <- runIO getCurrentDir
  describe "readDBPackages" $ do
    it' "Reads from a package db with only a single package header" $
      do
        packages <- readDBPackages (SqliteDB (currDir </> singlePackageDB))
        packages
          `shouldBe'` expectedPackage

    it' "Successfully reads remaining packages in a db with a bad blob" $
      do
        packages <- readDBPackages (SqliteDB (currDir </> badPackageDB))
        packages `shouldBe'` expectedPackage
