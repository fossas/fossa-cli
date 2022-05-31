{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.VendoredDependencySpec (
  spec,
) where

import App.Fossa.VendoredDependency (
  NeedScanningDeps (NeedScanningDeps),
  SkippableDeps (SkippableDeps),
  SkippedDepsLogMsg (..),
  VendoredDependencyScanMode (..),
  compressFile,
  skippedDepsDebugLog,
 )
import Control.Carrier.Lift (sendIO)
import Control.Effect.Path (withSystemTempDir)
import Path (Abs, Dir, Path, mkRelDir, (</>))
import Path.IO (getCurrentDir)
import Test.Effect (it', shouldContain')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

flippedCompressFile :: Path Abs Dir -> FilePath -> Path Abs Dir -> IO FilePath
flippedCompressFile directory fileToTar outputDir = compressFile outputDir directory fileToTar

spec :: Spec
spec = do
  currDir <- runIO getCurrentDir
  describe "compressFile" $ do
    it' "should compress a file" $
      do
        let fileToTar = "foo"
        let specDir = currDir </> $(mkRelDir "test/ArchiveUploader/normal")
        compressedFilePath <- sendIO $ withSystemTempDir "fossa-temp" (flippedCompressFile specDir fileToTar)
        compressedFilePath `shouldContain'` fileToTar

    -- We are mostly testing that this does not raise an exception
    it' "should compress a directory called '.' without throwing" $
      do
        let fileToTar = "."
        let specDir = currDir </> $(mkRelDir "test/ArchiveUploader/normal")
        compressedFilePath <- sendIO $ withSystemTempDir "fossa-temp" (flippedCompressFile specDir fileToTar)
        compressedFilePath `shouldContain'` fileToTar

  describe "skippedDepsDebugLog" $ do
    it "should return SkippingUnsupportedMsg when skipping is not supported" $
      skippedDepsDebugLog (NeedScanningDeps []) (SkippableDeps []) SkippingNotSupported `shouldBe` SkippingUnsupportedMsg
    it "should return SkippingDisabledViaFlagMsg when skipping is disabled" $
      skippedDepsDebugLog (NeedScanningDeps []) (SkippableDeps []) SkippingDisabledViaFlag `shouldBe` SkippingDisabledViaFlagMsg
    it "should return AllDepsPreviouslyScannedMsg when skipping is supported and nothing needs scanning" $
      skippedDepsDebugLog (NeedScanningDeps []) (SkippableDeps [Fixtures.firstVendoredDep]) SkipPreviouslyScanned `shouldBe` AllDepsPreviouslyScannedMsg
    it "should return AllDepsNeedScanningMsg when skipping is supported and all deps need to be scanned" $
      skippedDepsDebugLog (NeedScanningDeps [Fixtures.firstVendoredDep]) (SkippableDeps []) SkipPreviouslyScanned `shouldBe` AllDepsNeedScanningMsg
    it "should return SomeDepsNeedScanningMsg when skipping is supported and some deps need scanning but some deps do not need scanning" $
      skippedDepsDebugLog (NeedScanningDeps [Fixtures.firstVendoredDep]) (SkippableDeps [Fixtures.secondVendoredDep]) SkipPreviouslyScanned
        `shouldBe` SomeDepsNeedScanningMsg (SkippableDeps [Fixtures.secondVendoredDep])
