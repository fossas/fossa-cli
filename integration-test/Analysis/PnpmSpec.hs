{-# LANGUAGE QuasiQuotes #-}

module Analysis.PnpmSpec (spec) where

import Analysis.FixtureExpectationUtils (
  testSuiteHasSomeDepResults,
 )
import Analysis.FixtureUtils (
  AnalysisTestFixture (AnalysisTestFixture),
  FixtureArtifact (FixtureArtifact),
  FixtureEnvironment (LocalEnvironment),
 )
import Path (reldir)
import Strategy.Node qualified as Node
import Test.Hspec (Spec)
import Types (DiscoveredProjectType (..))

elementPlus :: AnalysisTestFixture (Node.NodeProject)
elementPlus =
  AnalysisTestFixture
    "element-plus"
    Node.discover
    LocalEnvironment
    Nothing
    $ FixtureArtifact
      "https://github.com/element-plus/element-plus/archive/refs/tags/2.0.0.tar.gz"
      [reldir|pnpm/element-plus/|]
      [reldir|element-plus-2.0.0/|]

-- | jotai-eager uses pnpm v9 catalogs (catalog: specifiers in package.json
-- resolved via the catalogs section in pnpm-lock.yaml).
jotaiEager :: AnalysisTestFixture (Node.NodeProject)
jotaiEager =
  AnalysisTestFixture
    "jotai-eager"
    Node.discover
    LocalEnvironment
    Nothing
    $ FixtureArtifact
      "https://github.com/jotaijs/jotai-eager/archive/refs/tags/v0.2.4.tar.gz"
      [reldir|pnpm/jotai-eager/|]
      [reldir|jotai-eager-0.2.4/|]

spec :: Spec
spec = do
  testSuiteHasSomeDepResults elementPlus PnpmProjectType
  testSuiteHasSomeDepResults jotaiEager PnpmProjectType
