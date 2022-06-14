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

spec :: Spec
spec = do
  testKeycloak
  testGuava
