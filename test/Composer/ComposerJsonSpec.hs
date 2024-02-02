{-# LANGUAGE TemplateHaskell #-}

module Composer.ComposerJsonSpec (spec) where

import App.Fossa.Analyze.LicenseAnalyze (LicenseAnalyzeProject (licenseAnalyzeProject))
import Control.Effect.Diagnostics qualified as Diagnostics
import Path (
  Abs,
  Dir,
  Path,
  mkRelDir,
  mkRelFile,
  toFilePath,
  (</>),
 )
import Path.IO (getCurrentDir)
import Strategy.Composer (ComposerProject (..))
import Test.Effect (it', shouldBe')
import Test.Hspec (Spec, describe, runIO)
import Types (License (..), LicenseResult (..), LicenseType (LicenseSPDX))

oneLicense :: [License]
oneLicense =
  [ License
      { licenseType = LicenseSPDX
      , licenseValue = "MIT"
      }
  ]

manyLicenses :: [License]
manyLicenses =
  [ License
      { licenseType = LicenseSPDX
      , licenseValue = "MIT"
      }
  , License
      { licenseType = LicenseSPDX
      , licenseValue = "GPL"
      }
  , License
      { licenseType = LicenseSPDX
      , licenseValue = "BSD"
      }
  ]

licenseResult :: [License] -> Path a Dir -> [LicenseResult]
licenseResult licenses baseDir =
  [ LicenseResult
      { licenseFile = toFilePath $ baseDir </> $(mkRelFile "composer.json")
      , licensesFound = licenses
      }
  ]

composerProject :: Path Abs Dir -> ComposerProject
composerProject baseDir =
  ComposerProject
    { composerDir = baseDir
    , composerLock = (baseDir </> $(mkRelFile "not_tested_here"))
    , composerJson = Just $ baseDir </> $(mkRelFile "composer.json")
    }

spec :: Spec
spec = do
  currentDir <- runIO getCurrentDir
  describe "composer.json LicenseAnalyzeProject" $ do
    it' "Reads a composer.json with a single license string" $ do
      let baseDir = currentDir </> $(mkRelDir "test/Composer/testdata/composer_json_single")
      licenses <- licenseAnalyzeProject (composerProject baseDir)
      licenses `shouldBe'` licenseResult oneLicense baseDir

    it' "Reads a composer.json with multiple license strings" $ do
      let baseDir = currentDir </> $(mkRelDir "test/Composer/testdata/composer_json_many")
      licenses <- licenseAnalyzeProject (composerProject baseDir)
      licenses `shouldBe'` licenseResult manyLicenses baseDir

    it' "Reads a composer.json with no license key" $ do
      let baseDir = currentDir </> $(mkRelDir "test/Composer/testdata/composer_json_no_license")
      licenses <- licenseAnalyzeProject (composerProject baseDir)
      licenses `shouldBe'` licenseResult [] baseDir

    it' "Fails to read a composer.json which is out of schema" $ do
      let baseDir = currentDir </> $(mkRelDir "test/Composer/testdata/composer_json_bad_schema")
      maybeLicenses <- Diagnostics.recover . licenseAnalyzeProject $ composerProject baseDir
      maybeLicenses `shouldBe'` Nothing

    it' "Returns [] if no composer.json exists" $ do
      let baseDir = currentDir </> $(mkRelDir "test/Composer/testdata/")
      let missingJsonProject = (composerProject baseDir){composerJson = Nothing}
      licenses <- licenseAnalyzeProject missingJsonProject
      licenses `shouldBe'` []
