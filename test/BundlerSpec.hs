{-# LANGUAGE TemplateHaskell #-}

module BundlerSpec (spec) where

import Path (File, Path, Rel, mkRelDir, mkRelFile, (</>))
import Path.IO (getCurrentDir)
import Strategy.Bundler (BundlerProject (bundlerGemSpec), discover)
import Test.Effect (it', shouldMatchList')
import Test.Hspec (Spec, describe, runIO)
import Types (DiscoveredProject (projectData))

specFilenames :: [Path Rel File]
specFilenames =
  [ $(mkRelFile "simple_spec.gemspec")
  , $(mkRelFile "complex.gemspec")
  , $(mkRelFile "licenses.gemspec")
  , $(mkRelFile "licenses_word_array.gemspec")
  ]

spec :: Spec
spec = do
  currDir <- runIO getCurrentDir
  describe "Detects multiple gemspecs in a directory" $ do
    let specDir = currDir </> $(mkRelDir "test/Ruby/testdata/gemspecs")
        expectedSpecFiles = map (specDir </>) specFilenames
    it' "Finds all *.gemspec files in a dir" $ do
      projects <- map projectData <$> discover specDir
      let specFiles = mconcat $ map bundlerGemSpec projects
      specFiles `shouldMatchList'` expectedSpecFiles
