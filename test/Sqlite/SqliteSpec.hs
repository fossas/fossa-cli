{-# LANGUAGE TemplateHaskell #-}

module Sqlite.SqliteSpec (spec) where

import Data.Rpm.DbHeaderBlob (PkgInfo (..))
import Path (File, Path, Rel, mkRelFile, (</>))
import Strategy.Sqlite (SqliteDB (SqliteDB), readDBPackages)
import Test.Effect (it', shouldBe')
import Test.Hspec (Spec, context, describe, runIO, fcontext)
import Path.IO (getCurrentDir)

spec :: Spec
spec = fcontext "Sqlite DB" $
  do readDBPackagesSpec

singlePackageDB :: Path Rel File
singlePackageDB = $(mkRelFile "test/Sqlite/test_data/single_pkg_db.sqlite")

readDBPackagesSpec :: Spec
readDBPackagesSpec = do
  currDir <- runIO getCurrentDir
  describe "readDBPackages" $ do
    it' "Reads from a package db with only a single package header" $
      do
        packages <- readDBPackages (SqliteDB (currDir </> singlePackageDB))
        packages
          `shouldBe'` [ PkgInfo
                          { pkgName = Just "libgcc"
                          , pkgVersion = Just "11.2.1"
                          , pkgRelease = Just "1.fc35"
                          , pkgArch = Just "x86_64"
                          , pkgEpoch = Nothing
                          }
                      ]
