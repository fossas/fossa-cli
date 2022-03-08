{-# LANGUAGE TemplateHaskell #-}

module Cocoapods.CocoapodsSpec (spec) where

import App.Pathfinder.Types (LicenseAnalyzeProject (licenseAnalyzeProject))
import Data.Text (Text)
import Path (Abs, Dir, File, Path, mkRelDir, mkRelFile, toFilePath, (</>))
import Path.IO (getCurrentDir)
import Strategy.Cocoapods (CocoapodsProject (..))
import Test.Effect (it', shouldBe')
import Test.Hspec (Spec, describe, runIO)
import Types (
  License (License),
  LicenseResult (..),
  LicenseType (UnknownType),
 )

mkCocoapodsProject :: Path Abs Dir -> Path Abs File -> CocoapodsProject
mkCocoapodsProject baseDir path =
  CocoapodsProject
    { cocoapodsPodfile = Nothing
    , cocoapodsPodfileLock = Nothing
    , cocoapodsDir = baseDir
    , cocoaPodsSpecFiles = [path]
    }

expectedLicenseResult :: Path Abs File -> Text -> LicenseResult
expectedLicenseResult specFile licenseName =
  LicenseResult
    { licenseFile = toFilePath specFile
    , licensesFound = [License UnknownType licenseName]
    }

licenseScanSpec :: Spec
licenseScanSpec = do
  currDir <- runIO getCurrentDir
  let specDir = currDir </> $(mkRelDir "test/cocoapods/testdata")
  describe "Cocoapods podspec file declared license scanning" $ do
    it' "Can extract a string literal license" $ do
      let specFile = specDir </> $(mkRelFile "stringLicense.podspec")
          project = mkCocoapodsProject specDir specFile
      res <- licenseAnalyzeProject project
      res `shouldBe'` [expectedLicenseResult specFile "MIT"]
    it' "Can extract a license from a dictionary" $ do
      let specFile = specDir </> $(mkRelFile "dictLicense.podspec")
          project = mkCocoapodsProject specDir specFile
      res <- licenseAnalyzeProject project
      res `shouldBe'` [expectedLicenseResult specFile "GPL"]

spec :: Spec
spec = licenseScanSpec
