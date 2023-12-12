{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.MavenSpec (spec) where

import Analysis.FixtureExpectationUtils (
  DependencyResultsSummary (
    DependencyResultsSummary,
    graphType,
    numDeps,
    numDirectDeps,
    numEdges,
    numManifestFiles
  ),
  expectProject,
  testSuiteDepResultSummary,
  withAnalysisOf,
 )
import Analysis.FixtureUtils (
  AnalysisTestFixture (AnalysisTestFixture),
  FixtureArtifact (FixtureArtifact),
  FixtureEnvironment (NixEnv),
 )
import Path (reldir)
import Strategy.Maven qualified as Maven
import Test.Hspec (Spec, aroundAll, describe, it)
import Types (
  DiscoveredProjectType (MavenProjectType),
  GraphBreadth (Complete),
 )

mavenEnv :: FixtureEnvironment
mavenEnv = NixEnv ["maven", "jdk11"]

keycloak :: AnalysisTestFixture (Maven.MavenProject)
keycloak =
  AnalysisTestFixture
    "keycloak"
    Maven.discover
    mavenEnv
    Nothing
    $ FixtureArtifact
      "https://github.com/keycloak/keycloak/archive/refs/tags/15.1.0.tar.gz"
      [reldir|maven/keycloak/|]
      [reldir|keycloak-15.1.0//|]

guava :: AnalysisTestFixture Maven.MavenProject
guava =
  AnalysisTestFixture
    "guava"
    Maven.discover
    mavenEnv
    Nothing
    $ FixtureArtifact
      "https://github.com/google/guava/archive/refs/tags/v31.1.tar.gz"
      [reldir|maven/guava/|]
      [reldir|guava-31.1|]

simplePomFile :: AnalysisTestFixture Maven.MavenProject
simplePomFile =
  AnalysisTestFixture
    "simple-pom-file"
    Maven.discover
    mavenEnv
    Nothing
    $ FixtureArtifact
      "https://github.com/fossas/example-pom-file/archive/refs/heads/main.tar.gz"
      [reldir|maven/simple_pom_file/|]
      [reldir|example-pom-file-main|]

pomFileWithBuildDirOverride :: AnalysisTestFixture Maven.MavenProject
pomFileWithBuildDirOverride =
  AnalysisTestFixture
    "build-dir-override"
    Maven.discover
    mavenEnv
    Nothing
    $ FixtureArtifact
      "https://github.com/fossas/example-pom-file/archive/refs/heads/override-build-directory.tar.gz"
      [reldir|maven/build_dir_override/|]
      [reldir|example-pom-file-override-build-directory|]

testKeycloak :: Spec
testKeycloak = do
  aroundAll (withAnalysisOf keycloak) $ do
    describe "keycloak" $ do
      it "should find targets" $ \(result, extractedDir) ->
        expectProject (MavenProjectType, extractedDir) result

testGuava :: Spec
testGuava =
  testSuiteDepResultSummary
    guava
    Types.MavenProjectType
    DependencyResultsSummary
      { numDeps = 85
      , numDirectDeps = 17
      , numEdges = 75
      , numManifestFiles = 1
      , graphType = Complete
      }

testSimplePomFile :: Spec
testSimplePomFile = do
  aroundAll (withAnalysisOf simplePomFile) $ do
    describe "simple POM file" $ do
      it "should find targets" $ \(result, extractedDir) ->
        expectProject (MavenProjectType, extractedDir) result

testBuildDirOverride :: Spec
testBuildDirOverride = do
  aroundAll (withAnalysisOf pomFileWithBuildDirOverride) $ do
    describe "POM file with build-directory override" $ do
      it "should find targets" $ \(result, extractedDir) ->
        expectProject (MavenProjectType, extractedDir) result

spec :: Spec
spec = do
  testKeycloak
  testGuava
  testBuildDirOverride
  testSimplePomFile
