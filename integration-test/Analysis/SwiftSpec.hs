{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.SwiftSpec (spec) where

import Analysis.FixtureExpectationUtils
import Analysis.FixtureUtils
import App.Types (Mode (NonStrict))
import Discovery.Simple (cachedDiscover)
import Discovery.Walk (walkWithFilters')
import Path
import Strategy.SwiftPM qualified as SwiftPM
import Test.Hspec
import Types (DiscoveredProjectType (..), GraphBreadth (Partial))

exampleProject :: AnalysisTestFixture (SwiftPM.SwiftProject)
exampleProject =
  AnalysisTestFixture
    "example_project"
    (\dir -> cachedDiscover SwiftProjectType SwiftPM.mkProject dir (walkWithFilters' SwiftPM.discoverVisitor dir))
    LocalEnvironment
    Nothing
    $ FixtureArtifact
      "https://github.com/fossas/example-pbxproj-project/archive/refs/heads/main.tar.gz"
      [reldir|swift/example_project/|]
      [reldir|example-pbxproj-project-main/myproj/|]

spec :: Spec
spec = do
  testSuiteDepResultSummary NonStrict exampleProject SwiftProjectType (DependencyResultsSummary 6 6 0 1 Partial)
