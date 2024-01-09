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
import Path (reldir)
import Strategy.Scala qualified as Scala
import Test.Hspec (Spec)
import Types (DiscoveredProjectType (..), GraphBreadth (Complete))

scalaEnv :: FixtureEnvironment
scalaEnv = NixEnv ["scala", "sbt"]

oldScalaEnv :: FixtureEnvironment
oldScalaEnv =
  NixEnv
    [ "scala_2_12"
    , "jdk"
    , -- Need jdk 8, it is in this archive.
      "-I"
    , "nixpkgs=https://github.com/NixOS/nixpkgs/archive/e296e89d75ab8aa1d0eed1c3688883a4f7937515.tar.gz"
    ]

scalaExampleProject :: AnalysisTestFixture (Scala.ScalaProject)
scalaExampleProject =
  AnalysisTestFixture
    "scalaExampleProject"
    Scala.discover
    scalaEnv
    Nothing
    $ FixtureArtifact
      "https://github.com/fossas/scala3-example-project/archive/refs/heads/main.tar.gz"
      [reldir|scala/scala-3-ex-project/|]
      [reldir|scala3-example-project-main/target/scala-3.1.2/|]

-- | Example project which uses a pre 1.4 version of SBT.
--  These versions required manually setting up the dependency graph plugin.
scalaOldSbt :: AnalysisTestFixture (Scala.ScalaProject)
scalaOldSbt =
  AnalysisTestFixture
    { testName = "scalaExampleOldSbt"
    , discover = Scala.discover
    , -- SBT version can be configured in the project definition.
      environment = oldScalaEnv
    , buildCmd = Nothing
    , artifact =
        FixtureArtifact
          { tarGzFileUrl = "https://github.com/fossas/example-projects/archive/refs/heads/main.tar.gz"
          , extractAt = [reldir|example-projects/|]
          , scopedDir = [reldir|example-projects-main/scala/single_explicit_plugin/target/scala-3.1|]
          }
    }

scalaOldSbtExpected :: DependencyResultsSummary
scalaOldSbtExpected =
  DependencyResultsSummary
    { numDeps = 2
    , numDirectDeps = 1
    , numEdges = 1
    , numManifestFiles = 1
    , graphType = Complete
    }

spec :: Spec
spec = do
  testSuiteDepResultSummary scalaExampleProject ScalaProjectType (DependencyResultsSummary 3 2 1 1 Complete)
  testSuiteDepResultSummary scalaOldSbt ScalaProjectType scalaOldSbtExpected
