{-# LANGUAGE TemplateHaskell #-}

module BundlerSpec (spec) where

import App.Fossa.Analyze.LicenseAnalyze (LicenseAnalyzeProject (licenseAnalyzeProject))
import Path (Abs, Dir, File, Path, Rel, mkRelDir, mkRelFile, toFilePath, (</>))
import Path.IO (getCurrentDir)
import Strategy.Bundler (BundlerProject (..), discover, findLicenses)
import Test.Effect (it', shouldBe', shouldMatchList')
import Test.Hspec (Spec, describe, runIO)
import Types (DiscoveredProject (projectData), License (License), LicenseResult (LicenseResult), LicenseType (UnknownType))

specFilenames :: [Path Rel File]
specFilenames =
  [ $(mkRelFile "single_license.gemspec")
  , $(mkRelFile "licenses.gemspec")
  , $(mkRelFile "licenses_word_array.gemspec")
  ]

mkLicensesResult :: Path Abs File -> [LicenseResult]
mkLicensesResult specPath =
  [ LicenseResult
      (toFilePath specPath)
      [ License UnknownType "AGPL"
      , License UnknownType "BSD"
      , License UnknownType "MIT"
      , License UnknownType "Apache"
      ]
  ]

wordLicensesResult :: Path Abs File -> [LicenseResult]
wordLicensesResult specPath =
  [ LicenseResult
      (toFilePath specPath)
      [ License UnknownType "AGPL"
      , License UnknownType "BSD"
      , License UnknownType "MIT"
      ]
  ]

multiLicenseResult :: Path Abs File -> Path Abs File -> [LicenseResult]
multiLicenseResult specPath1 specPath2 =
  [ LicenseResult
      (toFilePath specPath1)
      [License UnknownType "Ruby"]
  , LicenseResult
      (toFilePath specPath2)
      [ License UnknownType "AGPL"
      , License UnknownType "BSD"
      , License UnknownType "MIT"
      , License UnknownType "Apache"
      ]
  ]

specDir :: Path Rel Dir
specDir = $(mkRelDir "test/Ruby/testdata/gemspecs")

singleLicenseSpecPath :: Path Rel File
singleLicenseSpecPath = specDir </> $(mkRelFile "single_license.gemspec")

licensesSpecPath :: Path Rel File
licensesSpecPath = specDir </> $(mkRelFile "licenses.gemspec")

wordArraySpecPath :: Path Rel File
wordArraySpecPath = specDir </> $(mkRelFile "licenses_word_array.gemspec")

gemspecLicenseAnalyzeSpec :: Spec
gemspecLicenseAnalyzeSpec =
  describe "License analysis from gemspec files" $ do
    currDir <- runIO getCurrentDir
    it' "Can extract licenses from the 'license' key out of a more complicated gemspec" $ do
      let specPath = currDir </> singleLicenseSpecPath
      licenses <- findLicenses specPath
      licenses `shouldBe'` [LicenseResult (toFilePath specPath) [License UnknownType "Ruby"]]
    it' "Can extract licenses from the 'licenses' key" $ do
      let specPath = currDir </> licensesSpecPath
      licenses <- findLicenses specPath
      licenses `shouldBe'` mkLicensesResult specPath
    it' "Can extract licenses from the 'licenses' key with a word array" $ do
      let specPath = currDir </> wordArraySpecPath
      licenses <- findLicenses specPath
      licenses `shouldBe'` wordLicensesResult specPath

gemspecProjectLicenseScanningSpec :: Spec
gemspecProjectLicenseScanningSpec = do
  currDir <- runIO getCurrentDir
  let specPath1 = currDir </> singleLicenseSpecPath
      specPath2 = currDir </> licensesSpecPath
  describe "Project license discovery from a project" $
    it' "Discovers licenses from a project with multiple gemspec files" $ do
      let proj =
            BundlerProject
              { bundlerGemfile = currDir </> $(mkRelFile "not-tested")
              , bundlerGemfileLock = Nothing
              , bundlerDir = currDir </> specDir
              , bundlerGemSpec =
                  [ specPath1
                  , specPath2
                  ]
              }
      licenses <- licenseAnalyzeProject proj
      licenses `shouldBe'` multiLicenseResult specPath1 specPath2

gemspecDiscovery :: Spec
gemspecDiscovery =
  do
    currDir <- runIO getCurrentDir
    describe "GemSpec discovery" $ do
      let absSpecDir = currDir </> $(mkRelDir "test/Ruby/testdata/gemspecs")
          expectedSpecFiles = map (absSpecDir </>) specFilenames
      it' "Finds all *.gemspec files in a dir" $ do
        projects <- map projectData <$> discover absSpecDir
        let specFiles = mconcat $ map bundlerGemSpec projects
        specFiles `shouldMatchList'` expectedSpecFiles

spec :: Spec
spec = do
  gemspecDiscovery
  gemspecProjectLicenseScanningSpec
  gemspecLicenseAnalyzeSpec
