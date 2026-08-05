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

cljHttp :: AnalysisTestFixture (Leiningen.LeiningenProject)
cljHttp =
  AnalysisTestFixture
    "clj-http"
    Leiningen.discover
    clojureEnv
    Nothing
    $ FixtureArtifact
      "https://github.com/dakrone/clj-http/archive/refs/tags/3.13.1.tar.gz"
      [reldir|clojure/clj-http/|]
      [reldir|clj-http-3.13.1/|]

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
  testSuiteDepResultSummary NonStrict cljHttp LeiningenProjectType (DependencyResultsSummary 71 27 44 1 Complete)
  testSuiteDepResultSummary NonStrict ring LeiningenProjectType (DependencyResultsSummary 23 6 17 1 Complete)
