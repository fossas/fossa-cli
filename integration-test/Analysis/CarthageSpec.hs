{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.CarthageSpec (spec) where

import Analysis.FixtureExpectationUtils
import Analysis.FixtureUtils
import Path
import Strategy.Carthage qualified as Carthage
import Test.Hspec
import Types (GraphBreadth (Complete))

swiftQueue :: AnalysisTestFixture (Carthage.CarthageProject)
swiftQueue =
  AnalysisTestFixture
    "SwiftQueue"
    Carthage.discover
    LocalEnvironment
    Nothing
    $ FixtureArtifact
      "https://github.com/lucas34/SwiftQueue/archive/refs/tags/5.0.2.tar.gz"
      [reldir|carthage/SwiftQueue/|]
      [reldir|SwiftQueue-5.0.2/|]

spec :: Spec
spec = do
  testSuiteDepResultSummary swiftQueue "carthage" (DependencyResultsSummary 1 1 0 1 Complete)
