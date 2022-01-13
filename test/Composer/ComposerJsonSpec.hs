{-# LANGUAGE TemplateHaskell #-}

module Composer.ComposerJsonSpec (spec) where

import App.Pathfinder.Types (LicenseAnalyzeProject (licenseAnalyzeProject))
import Control.Effect.Diagnostics qualified as Diagnostics
import Path (mkRelDir, mkRelFile, toFilePath, (</>))
import Path.IO (getCurrentDir)
import Path.Posix (Dir, Path)
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

spec :: Spec
spec = do
  currentDir <- runIO getCurrentDir
  describe "composer.json LicenseAnalyzeProject" $ do
    it' "Reads a composer.json with a single license string" $ do
        let baseDir = currentDir </> $(mkRelDir "test/Composer/testdata/composer_json_single")
        let project = ComposerProject baseDir $ currentDir </> $(mkRelFile "not_tested_here")
        licenses <- licenseAnalyzeProject project
        licenses `shouldBe'` licenseResult oneLicense baseDir

    it' "Reads a composer.json with multiple license strings" $ do
        let baseDir = currentDir </> $(mkRelDir "test/Composer/testdata/composer_json_many")
        let project = ComposerProject baseDir $ currentDir </> $(mkRelFile "not_tested_here")
        licenses <- licenseAnalyzeProject project
        licenses `shouldBe'` licenseResult manyLicenses baseDir

    it' "Reads a composer.json with no license key" $ do
        let baseDir = currentDir </> $(mkRelDir "test/Composer/testdata/composer_json_no_license")
        let project = ComposerProject baseDir $ currentDir </> $(mkRelFile "not_tested_here")
        licenses <- licenseAnalyzeProject project
        licenses `shouldBe'` []

    it' "Fails to read a composer.json which is out of schema" $ do
        let baseDir = currentDir </> $(mkRelDir "test/Composer/testdata/composer_json_bad_schema")
        let project = ComposerProject baseDir $ currentDir </> $(mkRelFile "not_tested_here")
        maybeLicenses <- Diagnostics.recover $ licenseAnalyzeProject project
        maybeLicenses `shouldBe'` Nothing
