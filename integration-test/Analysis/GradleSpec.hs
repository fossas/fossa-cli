{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.GradleSpec (spec) where

import Analysis.FixtureExpectationUtils (
  expectProject,
  withAnalysisOf,
 )
import Analysis.FixtureUtils (
  AnalysisTestFixture (AnalysisTestFixture),
  FixtureArtifact (FixtureArtifact),
  FixtureEnvironment (NixEnv),
 )
import App.Types (Mode (NonStrict))
import Path (reldir)
import Strategy.Gradle qualified as Gradle
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe)
import Types (DiscoveredProjectType (..))

gradleEnv :: FixtureEnvironment
gradleEnv = NixEnv ["gradle"]

springBoot :: AnalysisTestFixture (Gradle.GradleProject)
springBoot =
  AnalysisTestFixture
    "gradle-java-springboot"
    Gradle.discover
    gradleEnv
    Nothing
    $ FixtureArtifact
      "https://github.com/spring-projects/spring-boot/archive/refs/tags/v4.0.0-RC2.tar.gz"
      [reldir|gradle/sample/|]
      [reldir|spring-boot-4.0.0-RC2|]

gradleSettingsOnly :: AnalysisTestFixture (Gradle.GradleProject)
gradleSettingsOnly =
  AnalysisTestFixture
    "gradle-java-settings-only"
    Gradle.discover
    gradleEnv
    Nothing
    $ FixtureArtifact
      "https://docs.gradle.org/7.3.3/samples/zips/sample_building_java_applications-groovy-dsl.zip"
      [reldir|gradle/sample/|]
      [reldir|.|]

-- | Gradle's own 9.1.0 "building Java applications" sample ships with
-- @org.gradle.configuration-cache=true@ in its @gradle.properties@. The init
-- script used to resolve dependencies inside the task's @doLast@, which the
-- configuration cache rejects with "Invocation of 'Task.project' by task
-- ':app:jsonDeps' at execution time is unsupported with the configuration
-- cache", so analysis of any such build failed.
gradleConfigurationCache :: AnalysisTestFixture (Gradle.GradleProject)
gradleConfigurationCache =
  AnalysisTestFixture
    "gradle-java-configuration-cache"
    Gradle.discover
    gradleEnv
    Nothing
    $ FixtureArtifact
      "https://docs.gradle.org/9.1.0/samples/zips/sample_building_java_applications-groovy-dsl.zip"
      [reldir|gradle/sample-configuration-cache/|]
      [reldir|.|]

testSpringBoot :: Spec
testSpringBoot =
  aroundAll (withAnalysisOf NonStrict springBoot) $ do
    describe "gradle-java springboot" $ do
      it "should find targets" $ \(result, extractedDir) -> do
        expectProject (GradleProjectType, extractedDir) result
        length result `shouldBe` 1

testGradleSettingsOnly :: Spec
testGradleSettingsOnly =
  aroundAll (withAnalysisOf NonStrict gradleSettingsOnly) $ do
    describe "gradle-java gradle settings only" $ do
      it "should find targets" $ \(result, extractedDir) -> do
        expectProject (GradleProjectType, extractedDir) result
        length result `shouldBe` 1

testGradleConfigurationCache :: Spec
testGradleConfigurationCache =
  aroundAll (withAnalysisOf NonStrict gradleConfigurationCache) $ do
    describe "gradle-java with the configuration cache enabled" $ do
      it "should find targets" $ \(result, extractedDir) -> do
        expectProject (GradleProjectType, extractedDir) result
        length result `shouldBe` 1

spec :: Spec
spec = do
  testSpringBoot
  testGradleSettingsOnly
  testGradleConfigurationCache
