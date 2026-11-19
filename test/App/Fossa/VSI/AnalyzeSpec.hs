{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.VSI.AnalyzeSpec (spec) where

import App.Fossa.VSI.Analyze (scanDir, withDefaultFilters)
import Data.Glob qualified as Glob
import Discovery.Filters (AllFilters (..), comboExcludeWithGlobs, comboInclude, comboIncludeWithGlobs)
import Path (Abs, Dir, Path, mkAbsDir, mkRelDir, (</>))
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

#ifdef mingw32_HOST_OS
root :: Path Abs Dir
root = $(mkAbsDir "C:/tmp/root")
#else
root :: Path Abs Dir
root = $(mkAbsDir "/tmp/root")
#endif

scanned :: AllFilters -> Path Abs Dir -> Bool
scanned filters = scanDir (withDefaultFilters filters) root

-- | `paths.only: [src]`, as a path or a glob.
onlySrc :: AllFilters -> Expectation
onlySrc filters = do
  scanned filters root `shouldBe` False
  scanned filters (root </> $(mkRelDir "src")) `shouldBe` True
  scanned filters (root </> $(mkRelDir "src/lib")) `shouldBe` True
  scanned filters (root </> $(mkRelDir "docs")) `shouldBe` False

spec :: Spec
spec = describe "scanDir" $ do
  it "scans a dir an exclude glob doesn't match" $
    scanned (AllFilters mempty $ comboExcludeWithGlobs [] [] [Glob.unsafeGlobRel "**/.svn"]) (root </> $(mkRelDir "a/b")) `shouldBe` True

  it "scans under a paths.only entry, but not its parents" $
    onlySrc $
      AllFilters (comboInclude [] [$(mkRelDir "src")]) mempty

  it "treats a paths.only glob like the path" $
    onlySrc $
      AllFilters (comboIncludeWithGlobs [] [] [Glob.unsafeGlobRel "src/**"]) mempty

  it "scans the root with no user filters" $
    scanned mempty root `shouldBe` True
