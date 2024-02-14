{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Reachability.MavenSpec (spec) where

import App.Fossa.Reachability.Maven (getJarsByBuild)
import Control.Effect.Lift (sendIO)
import Path (Abs, Dir, File, Path, mkRelDir, mkRelFile, (</>))
import Path.IO qualified as PIO
import Test.Effect (it', shouldBe')
import Test.Hspec (Spec, describe)

spec :: Spec
spec = describe "Maven" $ do
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
