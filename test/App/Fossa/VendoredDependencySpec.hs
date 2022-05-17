{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.VendoredDependencySpec (
  spec,
) where

import App.Fossa.VendoredDependency (
  compressFile,
 )
import Control.Carrier.Lift (sendIO)
import Control.Effect.Path (withSystemTempDir)
import Path (Abs, Dir, Path, mkRelDir, (</>))
import Path.IO (getCurrentDir)
import Test.Effect (it', shouldContain')
import Test.Hspec (Spec, describe, runIO)

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
  where
    flippedCompressFile :: Path Abs Dir -> FilePath -> Path Abs Dir -> IO FilePath
    flippedCompressFile directory fileToTar outputDir = compressFile outputDir directory fileToTar
