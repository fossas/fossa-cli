{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.ClojureSpec (spec) where

import Analysis.FixtureExpectationUtils
import Analysis.FixtureUtils
import App.Types (Mode (NonStrict))
import Data.List (isInfixOf)
import Path
import Strategy.Leiningen qualified as Leiningen
import Test.Hspec
import Types (DiscoveredProject (..), DiscoveredProjectType (..), GraphBreadth (Complete))

clojureEnv :: FixtureEnvironment
clojureEnv = NixEnv ["openjdk11", "clojure", "leiningen"]

-- | Discover Leiningen projects but skip anything under a @.circleci@ directory.
--
-- The pinned eastwood Release-1.0.0 tarball ships a secondary @project.clj@ at
-- @.circleci/nvd/@ that pulls in OWASP dependency-check artifacts which are no
-- longer resolvable from Maven Central / Clojars. Without this filter, @lein
-- deps :tree-data@ fails on that subproject and breaks the integration test
-- even though the eastwood project itself is healthy.
discoverSkippingCircleCI :: Path Abs Dir -> TestC IO [DiscoveredProject Leiningen.LeiningenProject]
discoverSkippingCircleCI dir = filter notUnderCircleCI <$> Leiningen.discover dir
  where
    notUnderCircleCI dp = not (".circleci" `isInfixOf` toFilePath (projectPath dp))

eastwood :: AnalysisTestFixture (Leiningen.LeiningenProject)
eastwood =
  AnalysisTestFixture
    "eastwood"
    discoverSkippingCircleCI
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
