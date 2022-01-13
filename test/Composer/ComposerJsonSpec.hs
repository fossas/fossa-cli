{-# LANGUAGE TemplateHaskell #-}

module Composer.ComposerJsonSpec (spec) where

import App.Pathfinder.Types (LicenseAnalyzeProject (licenseAnalyzeProject))
import Path (mkRelDir, mkRelFile, toFilePath, (</>))
import Path.IO (getCurrentDir)
import Strategy.Composer (ComposerProject (..))
import Test.Effect
import Test.Hspec (Spec, anyException, describe, it, runIO, shouldThrow)
import Types (License (..), LicenseResult (..), LicenseType (LicenseSPDX))

spec :: Spec
spec = do
  currentDir <- runIO getCurrentDir
  describe "composer.json LicenseAnalyzeProject" $ do
    it' "Reads a composer.json with a single license string" $
      let baseDir = currentDir </> $(mkRelDir "test/Composer/testdata/composer_json_single")
          project =
            ComposerProject
              { composerDir = baseDir
              , composerLock = currentDir </> $(mkRelFile "not_tested_here")
              }
       in do
            licenses <- licenseAnalyzeProject project
            licenses
              `shouldBe'` [ LicenseResult
                              { licenseFile = toFilePath $ baseDir </> $(mkRelFile "composer.json")
                              , licensesFound =
                                  [ License
                                      { licenseType = LicenseSPDX
                                      , licenseValue = "MIT"
                                      }
                                  ]
                              }
                          ]

    it' "Reads a composer.json with multiple license strings" $
      let baseDir = currentDir </> $(mkRelDir "test/Composer/testdata/composer_json_many")
          project =
            ComposerProject
              { composerDir = baseDir
              , composerLock = currentDir </> $(mkRelFile "not_tested_here")
              }
       in do
            licenses <- licenseAnalyzeProject project
            licenses
              `shouldBe'` [ LicenseResult
                              { licenseFile = toFilePath $ baseDir </> $(mkRelFile "composer.json")
                              , licensesFound =
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
                              }
                          ]

    it "Fails to read a composer.json which is out of schema" $
      let baseDir = currentDir </> $(mkRelDir "test/Composer/testdata/composer_json_bad_schema")
          project =
            ComposerProject
              { composerDir = baseDir
              , composerLock = currentDir </> $(mkRelFile "not_tested_here")
              }
       in do
            shouldThrow (runTestEffects $ licenseAnalyzeProject project >> pure ()) anyException
