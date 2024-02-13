{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Reachability.JarSpec (spec) where

import App.Fossa.Reachability.Jar (callGraphFromJar, isValidJar)
import Control.Effect.Lift (sendIO)

import Path (Abs, File, Path, mkRelFile, (</>))
import Path.IO qualified as PIO
import Test.Effect
import Test.Hspec (Spec, describe)

spec :: Spec
spec = do
  describe "callGraphFromJar" $ do
    it' "should get not get call graph from corruped jar file" $ do
      jarFile <- sendIO malformedJarFile
      resp <- callGraphFromJar jarFile

      resp `shouldBe'` Nothing

  describe "isValidJar" $ do
    it' "should return False when jar does not exist" $ do
      jarFile <- sendIO missingJar
      result <- isValidJar jarFile
      result `shouldBe'` False

    it' "should return True when jar exists" $ do
      jarFile <- sendIO sampleJar
      result <- isValidJar jarFile
      result `shouldBe'` True

    it' "should return False when jar exists, but is test jar" $ do
      jarFile <- sendIO sampleTestJar
      result <- isValidJar jarFile
      result `shouldBe'` False

-- | Jar is produced from following projects:
-- https://github.com/fossas/example-projects/tree/main/reachability/java/vulnerable-function-used
sampleJar :: IO (Path Abs File)
sampleJar = do
  cwd <- PIO.getCurrentDir
  pure (cwd </> $(mkRelFile "test/Reachability/testdata/sample.jar"))

sampleTestJar :: IO (Path Abs File)
sampleTestJar = do
  cwd <- PIO.getCurrentDir
  -- Jar has same content as @sampleJar@
  pure (cwd </> $(mkRelFile "test/Reachability/testdata/sample-test.jar"))

missingJar :: IO (Path Abs File)
missingJar = do
  cwd <- PIO.getCurrentDir
  pure (cwd </> $(mkRelFile "test/Reachability/testdata/missing.jar"))

malformedJarFile :: IO (Path Abs File)
malformedJarFile = do
  cwd <- PIO.getCurrentDir
  pure (cwd </> $(mkRelFile "test/Reachability/testdata/malformed.jar"))
