{-# LANGUAGE TemplateHaskell #-}

module BundlerSpec (spec) where

import Path (mkRelDir, (</>))
import Path.IO (getCurrentDir)
import Strategy.Bundler (genGemspecFilename)
import Test.Hspec (Spec, it, runIO, shouldBe, describe)

spec :: Spec
spec = do
  currDir <- runIO getCurrentDir
  describe "Generating gemspec filename from dir" $ do
    -- this test is to make sure this behavior works on both POSIX and windows
    -- when run in CI environments.
    it "Appends .gemspec to the end of a dir" $
      genGemspecFilename (currDir </> $(mkRelDir "foo")) `shouldBe` "foo.gemspec"

