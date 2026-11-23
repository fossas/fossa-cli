{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.ScalaSpec (spec) where

import Analysis.FixtureExpectationUtils (
  DependencyResultsSummary (..),
  testSuiteDepResultSummary,
 )
import Analysis.FixtureUtils (
  AnalysisTestFixture (..),
  FixtureArtifact (..),
  FixtureEnvironment (NixEnv),
 )
import App.Types (Mode (..))
import Discovery.Simple (cachedDiscoverWith)
import Discovery.Walk (walkWithFilters')
import Path (reldir)
import Strategy.Scala qualified as Scala
import Test.Hspec (Spec)
import Types (DiscoveredProjectType (..), GraphBreadth (Complete))

scalaEnv :: FixtureEnvironment
scalaEnv = NixEnv ["scala", "sbt"]

scalaExampleProject :: AnalysisTestFixture (Scala.ScalaProject)
scalaExampleProject =
  AnalysisTestFixture
    "scalaExampleProject"
    (\dir -> cachedDiscoverWith ScalaProjectType Scala.postDiscover dir (walkWithFilters' Scala.discoverVisitor dir))
    scalaEnv
    Nothing
    $ FixtureArtifact
      "https://github.com/fossas/scala3-example-project/archive/refs/heads/main.tar.gz"
      [reldir|scala/scala-3-ex-project/|]
      [reldir|scala3-example-project-main/target/scala-3.4.0/|]

spec :: Spec
spec = do
  testSuiteDepResultSummary NonStrict scalaExampleProject ScalaProjectType (DependencyResultsSummary 3 2 1 1 Complete)
