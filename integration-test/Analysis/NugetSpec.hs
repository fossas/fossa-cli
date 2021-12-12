{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.NugetSpec (spec) where

import Analysis.FixtureExpectationUtils
import Analysis.FixtureUtils
import Path
import Strategy.NuGet.PackageReference qualified as PackageReference
import Strategy.NuGet.PackagesConfig qualified as PackagesConfig
import Test.Hspec
import Types

serviceStack :: (Path Abs Dir -> TestC IO [DiscoveredProject a]) -> AnalysisTestFixture a
serviceStack discoveryFunc =
  AnalysisTestFixture
    "ServiceStack"
    discoveryFunc
    LocalEnvironment
    Nothing
    $ FixtureArtifact
      "https://github.com/ServiceStack/ServiceStack/archive/refs/tags/v5.13.2.tar.gz"
      [reldir|nuget/ServiceStack/|]
      [reldir|servicestack-5.13.2//|]

testServiceStackForPkgReferences :: Spec
testServiceStackForPkgReferences =
  aroundAll (withAnalysisOf $ serviceStack PackageReference.discover) $ do
    describe "ServiceStack" $ do
      it "should find targets" $ \(result, _) -> do
        length result `shouldBe` 64

testServiceStackForPkgConfig :: Spec
testServiceStackForPkgConfig =
  aroundAll (withAnalysisOf $ serviceStack PackagesConfig.discover) $ do
    describe "ServiceStack" $ do
      it "should find targets" $ \(result, _) -> do
        length result `shouldBe` 4

spec :: Spec
spec = do
  testServiceStackForPkgReferences
  testServiceStackForPkgConfig
