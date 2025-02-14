{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.NugetSpec (spec) where

import Analysis.FixtureExpectationUtils
import Analysis.FixtureUtils
import App.Types (Mode (NonStrict))
import Data.Set (member)
import Data.Set qualified as Set
import Discovery.Walk (fileName)
import Path
import Strategy.NuGet qualified as NuGet
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

dotnetCoreTwoExample :: (Path Abs Dir -> TestC IO [DiscoveredProject a]) -> AnalysisTestFixture a
dotnetCoreTwoExample discoveryFunc =
  AnalysisTestFixture
    "dotnet-core-2.0-example"
    discoveryFunc
    LocalEnvironment
    Nothing
    $ FixtureArtifact
      "https://github.com/james-fossa/dotnet-core-2.0-example/archive/refs/tags/0.0.1.tar.gz"
      [reldir|nuget/dotnet-core-2.0-example-0.0.1/|]
      [reldir|dotnet-core-2.0-example-0.0.1//|]

testServiceStackForPkgReferences :: Spec
testServiceStackForPkgReferences =
  aroundAll (withAnalysisOf NonStrict $ serviceStack NuGet.discover) $ do
    describe "ServiceStack" $ do
      it "should find targets" $ \(result, _) -> do
        length result `shouldBe` 64

testServiceStackForPkgConfig :: Spec
testServiceStackForPkgConfig =
  aroundAll (withAnalysisOf NonStrict $ serviceStack PackagesConfig.discover) $ do
    describe "ServiceStack" $ do
      it "should find targets" $ \(result, _) -> do
        length result `shouldBe` 4

testDotnetCoreTwoExampleForProjectAssetsJson :: Spec
testDotnetCoreTwoExampleForProjectAssetsJson =
  aroundAll (withAnalysisOf NonStrict $ dotnetCoreTwoExample NuGet.discover) $ do
    describe "dotnet-core-2.0-example" $ do
      it "should find targets" $ \(result, _) -> do
        length result `shouldBe` 2
        let projectDataPaths = Set.fromList $ map (fileName . NuGet.nugetProjectFile . Types.projectData . fst) result
        let doesProjectAssetsJsonTargetExist = member "project.assets.json" projectDataPaths
        let doesCsprojTargetExist = member "example.csproj" projectDataPaths
        doesProjectAssetsJsonTargetExist `shouldBe` True
        doesCsprojTargetExist `shouldBe` True

spec :: Spec
spec = do
  testServiceStackForPkgReferences
  testServiceStackForPkgConfig
  testDotnetCoreTwoExampleForProjectAssetsJson
