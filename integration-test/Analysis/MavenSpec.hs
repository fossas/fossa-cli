{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.MavenSpec (spec) where

import Analysis.FixtureExpectationUtils
import Analysis.FixtureUtils
import Path
import Strategy.Maven qualified as Maven
import Test.Hspec
import Types

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

testKeycloak :: Spec
testKeycloak =
  aroundAll (withAnalysisOf keycloak) $ do
    describe "keycloak" $ do
      it "should find targets" $ \(result, extractedDir) ->
        expectProject (MavenProjectType, extractedDir) result

spec :: Spec
spec = do
  testKeycloak
