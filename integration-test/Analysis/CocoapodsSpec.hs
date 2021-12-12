{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.CocoapodsSpec (spec) where

import Analysis.FixtureExpectationUtils
import Analysis.FixtureUtils
import Path
import Strategy.Cocoapods qualified as Cocoapods
import Test.Hspec
import Types (GraphBreadth (..))

shadowsocksXNG :: AnalysisTestFixture (Cocoapods.CocoapodsProject)
shadowsocksXNG =
  AnalysisTestFixture
    "ShadowsocksX-NG"
    Cocoapods.discover
    LocalEnvironment
    Nothing
    $ FixtureArtifact
      "https://github.com/shadowsocks/ShadowsocksX-NG/archive/refs/tags/v1.9.4.tar.gz"
      [reldir|cocoapods/ShadowsocksX-NG/|]
      [reldir|ShadowsocksX-NG-1.9.4/|]

sDWebImage :: AnalysisTestFixture (Cocoapods.CocoapodsProject)
sDWebImage =
  AnalysisTestFixture
    "SDWebImage"
    Cocoapods.discover
    LocalEnvironment
    Nothing
    $ FixtureArtifact
      "https://github.com/SDWebImage/SDWebImage/archive/refs/tags/5.12.0.tar.gz"
      [reldir|cocoapods/ring/|]
      [reldir|SDWebImage-5.12.0/|]

spec :: Spec
spec = do
  testSuiteDepResultSummary shadowsocksXNG "cocoapods" (DependencyResultsSummary 6 6 0 1 Partial)
  testSuiteDepResultSummary sDWebImage "cocoapods" (DependencyResultsSummary 4 4 0 1 Partial)
