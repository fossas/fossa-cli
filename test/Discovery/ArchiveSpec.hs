{-# LANGUAGE TemplateHaskell #-}

module Discovery.ArchiveSpec (spec) where

import Control.Carrier.Finally (runFinally)
import Control.Effect.Lift (sendIO)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Discovery.Archive (extractTar, extractTarGz, extractTarXz, extractZip, withArchive)
import Path (Abs, File, Path, mkRelDir, mkRelFile, toFilePath, (</>))
import Path.IO qualified as PIO
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

spec :: Spec
spec = do
  describe "extract zip archive to a temporary location" $ do
    target <- runIO simpleZipPath
    (extractedDir, extractedContentA, extractedContentB) <- runIO $
      runFinally . withArchive extractZip target $ \dir -> do
        contentA <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "a.txt")
        contentB <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "b.txt")
        pure (dir, contentA, contentB)
    tempDirExists <- runIO $ PIO.doesDirExist extractedDir

    it "should have extracted the correct contents" $ do
      extractedContentB `shouldBe` expectedContentB
      extractedContentA `shouldBe` expectedContentA

    it "should have cleaned up the temporary directory" $ do
      tempDirExists `shouldBe` False

  describe "extract tar archive to a temporary location" $ do
    target <- runIO simpleTarPath
    (extractedDir, extractedContentA, extractedContentB) <- runIO $
      runFinally . withArchive extractTar target $ \dir -> do
        contentA <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "a.txt")
        contentB <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "b.txt")
        pure (dir, contentA, contentB)
    tempDirExists <- runIO $ PIO.doesDirExist extractedDir

    it "should have extracted the correct contents" $ do
      extractedContentB `shouldBe` expectedContentB
      extractedContentA `shouldBe` expectedContentA

    it "should have cleaned up the temporary directory" $ do
      tempDirExists `shouldBe` False

  describe "extract tar.gz archive to a temporary location" $ do
    target <- runIO simpleTarGzPath
    (extractedDir, extractedContentA, extractedContentB) <- runIO $
      runFinally . withArchive extractTarGz target $ \dir -> do
        contentA <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "a.txt")
        contentB <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "b.txt")
        pure (dir, contentA, contentB)
    tempDirExists <- runIO $ PIO.doesDirExist extractedDir

    it "should have extracted the correct contents" $ do
      extractedContentB `shouldBe` expectedContentB
      extractedContentA `shouldBe` expectedContentA

    it "should have cleaned up the temporary directory" $ do
      tempDirExists `shouldBe` False

  describe "extract tar.xz archive to a temporary location" $ do
    target <- runIO simpleTarXzPath
    (extractedDir, extractedContentA, extractedContentB) <- runIO $
      runFinally . withArchive extractTarXz target $ \dir -> do
        contentA <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "a.txt")
        contentB <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "b.txt")
        pure (dir, contentA, contentB)
    tempDirExists <- runIO $ PIO.doesDirExist extractedDir

    it "should have extracted the correct contents" $ do
      extractedContentB `shouldBe` expectedContentB
      extractedContentA `shouldBe` expectedContentA

    it "should have cleaned up the temporary directory" $ do
      tempDirExists `shouldBe` False

simpleZipPath :: IO (Path Abs File)
simpleZipPath = PIO.resolveFile' "test/Discovery/testdata/simple.zip"

simpleTarPath :: IO (Path Abs File)
simpleTarPath = PIO.resolveFile' "test/Discovery/testdata/simple.tar"

simpleTarGzPath :: IO (Path Abs File)
simpleTarGzPath = PIO.resolveFile' "test/Discovery/testdata/simple.tar.gz"

simpleTarXzPath :: IO (Path Abs File)
simpleTarXzPath = PIO.resolveFile' "test/Discovery/testdata/simple.tar.xz"

expectedContentA :: Text
expectedContentA = "6b5effe3-215a-49ec-9286-f0702f7eb529"

expectedContentB :: Text
expectedContentB = "8dea86e4-4365-4711-872b-6f652b02c8d9"
