{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.PnpmSpec (spec) where

import Analysis.FixtureExpectationUtils
import Analysis.FixtureUtils
import Path
import Strategy.Node qualified as Node
import Test.Hspec
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

spec :: Spec
spec = do
  testSuitHasSomeDepResults elementPlus PnpmProjectType
