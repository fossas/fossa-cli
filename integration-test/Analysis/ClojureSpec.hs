{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.ClojureSpec (spec) where

import Analysis.FixtureExpectationUtils
import Analysis.FixtureUtils
import App.Types (Mode (NonStrict))
import Path
import Strategy.Leiningen qualified as Leiningen
import Test.Hspec
import Types (DiscoveredProjectType (..), GraphBreadth (Complete))

clojureEnv :: FixtureEnvironment
clojureEnv = NixEnv ["openjdk11", "clojure", "leiningen"]

eastwood :: AnalysisTestFixture (Leiningen.LeiningenProject)
eastwood =
  AnalysisTestFixture
    "eastwood"
    Leiningen.discover
    clojureEnv
    Nothing
    $ FixtureArtifact
      "https://github.com/jonase/eastwood/archive/refs/tags/Release-1.0.0.tar.gz"
      [reldir|clojure/eastwood/|]
      [reldir|eastwood-Release-1.0.0/|]

ring :: AnalysisTestFixture (Leiningen.LeiningenProject)
ring =
  AnalysisTestFixture
    "ring"
    Leiningen.discover
    clojureEnv
    Nothing
    $ FixtureArtifact
      "https://github.com/ring-clojure/ring/archive/refs/tags/1.9.4.tar.gz"
      [reldir|clojure/ring/|]
      [reldir|ring-1.9.4/|]

spec :: Spec
spec = do
  testSuiteDepResultSummary NonStrict eastwood LeiningenProjectType (DependencyResultsSummary 10 7 3 1 Complete)
  testSuiteDepResultSummary NonStrict ring LeiningenProjectType (DependencyResultsSummary 23 6 17 1 Complete)
