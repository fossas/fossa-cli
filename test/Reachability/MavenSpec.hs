{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Reachability.MavenSpec (spec) where

import App.Fossa.Reachability.Maven (getJarsByBuild, isValidJar)
import Control.Effect.Lift (sendIO)
import Path (Abs, Dir, File, Path, mkRelDir, mkRelFile, (</>))
import Path.IO qualified as PIO
import Test.Effect (it', shouldBe')
import Test.Hspec (Spec, describe)

spec :: Spec
spec = describe "Maven" $ do
  describe "getJarPathFromPom" $ do
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

  describe "getJarsByBuild" $ do
    it' "should get jar from defaults" $ do
      p <- sendIO defaultMavenProject
      jar <- sendIO defaultMavenProjectJar
      result <- getJarsByBuild p
      result `shouldBe'` [jar]

    it' "should get jar with modified name and modified directory" $ do
      p <- sendIO mavenProjectWithCustomName
      jar <- sendIO mavenProjectWithCustomDistJar
      result <- getJarsByBuild p
      result `shouldBe'` [jar]

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

defaultMavenProject :: IO (Path Abs Dir)
defaultMavenProject = do
  cwd <- PIO.getCurrentDir
  pure (cwd </> $(mkRelDir "test/Reachability/testdata/maven-default/"))

defaultMavenProjectJar :: IO (Path Abs File)
defaultMavenProjectJar = do
  cwd <- PIO.getCurrentDir
  -- Jar has same content as @sampleJar@
  pure (cwd </> $(mkRelFile "test/Reachability/testdata/maven-default/target/project-1.0.0.jar"))

mavenProjectWithCustomName :: IO (Path Abs Dir)
mavenProjectWithCustomName = do
  cwd <- PIO.getCurrentDir
  pure (cwd </> $(mkRelDir "test/Reachability/testdata/maven-build-config/"))

mavenProjectWithCustomDistJar :: IO (Path Abs File)
mavenProjectWithCustomDistJar = do
  cwd <- PIO.getCurrentDir
  -- Jar has same content as @sampleJar@
  pure (cwd </> $(mkRelFile "test/Reachability/testdata/maven-build-config/dist/custom-jar-name.jar"))
