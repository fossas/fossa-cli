{-# LANGUAGE TemplateHaskell #-}

module Cocoapods.CocoapodsSpec (spec) where

import App.Fossa.Analyze.LicenseAnalyze (LicenseAnalyzeProject (licenseAnalyzeProject))
import Data.Text (Text)
import Path (Abs, Dir, File, Path, Rel, mkRelDir, mkRelFile, toFilePath, (</>))
import Path.IO (getCurrentDir)
import Strategy.Cocoapods (CocoapodsProject (..))
import Strategy.Cocoapods qualified as Cocoapods
import Test.Effect (it', shouldBe', shouldMatchList')
import Test.Hspec (Spec, describe, runIO)
import Types (
  DiscoveredProject (projectData),
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
    , cocoapodsSpecFiles = [path]
    }

expectedLicenseResult :: Path Abs File -> Text -> LicenseResult
expectedLicenseResult specFile licenseName =
  LicenseResult
    { licenseFile = toFilePath specFile
    , licensesFound = [License UnknownType licenseName]
    }

strLicensePodspec :: Path Rel File
strLicensePodspec = $(mkRelFile "stringLicense.podspec")

dictLicensePodspec :: Path Rel File
dictLicensePodspec = $(mkRelFile "dictLicense.podspec")

licenseScanSpec :: Spec
licenseScanSpec = do
  currDir <- runIO getCurrentDir
  let specDir = currDir </> $(mkRelDir "test/Cocoapods/testdata")
  describe "Cocoapods podspec file declared license scanning" $ do
    it' "Can extract a string literal license" $ do
      let specFile = specDir </> strLicensePodspec
          project = mkCocoapodsProject specDir specFile
      res <- licenseAnalyzeProject project
      res `shouldBe'` [expectedLicenseResult specFile "MIT"]

    it' "Can extract a license from a dictionary" $ do
      let specFile = specDir </> dictLicensePodspec
          project = mkCocoapodsProject specDir specFile
      res <- licenseAnalyzeProject project
      res `shouldBe'` [expectedLicenseResult specFile "GPL"]

podspecDiscoverySpec :: Spec
podspecDiscoverySpec = do
  currDir <- runIO getCurrentDir
  let projectDir = currDir </> $(mkRelDir "test/Cocoapods/testdata")
      strLicense = projectDir </> strLicensePodspec
      dictLicense = projectDir </> dictLicensePodspec
  describe "Cocoapods podspec discovery" $
    it' "Finds podspec files during discovery" $ do
      specFiles <- foldMap (cocoapodsSpecFiles . projectData) <$> Cocoapods.discover projectDir
      specFiles
        `shouldMatchList'` [ strLicense
                           , dictLicense
                           ]

spec :: Spec
spec =
  do
    licenseScanSpec
    podspecDiscoverySpec
