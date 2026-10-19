{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.VSI.AnalyzeSpec (spec) where

import App.Fossa.VSI.Analyze (DirAction (..), dirAction, withDefaultFilters)
import Data.Glob qualified as Glob
import Discovery.Filters (AllFilters (..), comboExclude, comboExcludeWithGlobs, comboInclude, comboIncludeWithGlobs)
import Path (Abs, Dir, Path, mkAbsDir, mkRelDir, (</>))
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

#ifdef mingw32_HOST_OS
root :: Path Abs Dir
root = $(mkAbsDir "C:/tmp/root")
#else
root :: Path Abs Dir
root = $(mkAbsDir "/tmp/root")
#endif

actionFor :: AllFilters -> Path Abs Dir -> DirAction
actionFor filters = dirAction (withDefaultFilters filters) root

-- | `paths.only: [src]`, as a path or a glob.
onlySrc :: AllFilters -> Expectation
onlySrc filters = do
  actionFor filters root `shouldBe` WalkDir
  actionFor filters (root </> $(mkRelDir "src")) `shouldBe` ScanDir
  actionFor filters (root </> $(mkRelDir "src/lib")) `shouldBe` ScanDir
  actionFor filters (root </> $(mkRelDir "docs")) `shouldBe` SkipDir

spec :: Spec
spec = describe "dirAction" $ do
  it "skips a dir an exclude glob matches, and scans its parent" $ do
    let filters = AllFilters mempty $ comboExcludeWithGlobs [] [] [Glob.unsafeGlobRel "**/.svn"]
    actionFor filters (root </> $(mkRelDir "a/b/.svn")) `shouldBe` SkipDir
    actionFor filters (root </> $(mkRelDir "a/b")) `shouldBe` ScanDir

  it "skips only the root's dir for a plain exclude path" $ do
    let filters = AllFilters mempty $ comboExclude [] [$(mkRelDir ".svn")]
    actionFor filters (root </> $(mkRelDir ".svn")) `shouldBe` SkipDir
    actionFor filters (root </> $(mkRelDir "a/.svn")) `shouldBe` ScanDir

  it "walks the parents of a paths.only entry without scanning them" $
    onlySrc $
      AllFilters (comboInclude [] [$(mkRelDir "src")]) mempty

  it "treats a paths.only glob like the path" $
    onlySrc $
      AllFilters (comboIncludeWithGlobs [] [] [Glob.unsafeGlobRel "src/**"]) mempty

  it "skips the root's .git with no user filters" $ do
    actionFor mempty (root </> $(mkRelDir ".git")) `shouldBe` SkipDir
    actionFor mempty root `shouldBe` ScanDir
    actionFor mempty (root </> $(mkRelDir "a/.git")) `shouldBe` ScanDir
