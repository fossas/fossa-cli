{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.SwiftSpec (spec) where

import Analysis.FixtureExpectationUtils
import Analysis.FixtureUtils
import Path
import Strategy.SwiftPM qualified as SwiftPM
import Test.Hspec
import Types (DiscoveredProjectType (..), GraphBreadth (Partial))

exampleProject :: AnalysisTestFixture (SwiftPM.SwiftProject)
exampleProject =
  AnalysisTestFixture
    "example_project"
    SwiftPM.discover
    LocalEnvironment
    Nothing
    $ FixtureArtifact
      "https://github.com/fossas/example-pbxproj-project/archive/refs/heads/main.tar.gz"
      [reldir|swift/example_project/|]
      [reldir|example-pbxproj-project-main/myproj/|]

spec :: Spec
spec = do
  testSuiteDepResultSummary exampleProject SwiftProjectType (DependencyResultsSummary 6 6 0 1 Partial)
