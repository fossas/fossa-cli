{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.NugetSpec (spec) where

import Analysis.FixtureExpectationUtils
import Analysis.FixtureUtils
import App.Types (Mode (NonStrict))
import Data.Foldable (find)
import Data.Set (member)
import Data.Set qualified as Set
import Discovery.Walk (fileName)
import Graphing qualified
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

-- | DapperAOT uses NuGet Central Package Management (CPM).
-- PackageReference entries in .csproj files omit Version attributes;
-- versions are defined centrally in Directory.Packages.props.
dapperAOT :: AnalysisTestFixture NuGet.NuGetProject
dapperAOT =
  AnalysisTestFixture
    "DapperAOT-CPM"
    NuGet.discover
    LocalEnvironment
    Nothing
    $ FixtureArtifact
      "https://github.com/DapperLib/DapperAOT/archive/refs/tags/1.0.48.tar.gz"
      [reldir|nuget/DapperAOT/|]
      [reldir|DapperAOT-1.0.48//|]

-- | Versions pinned in DapperAOT's Directory.Packages.props at tag 1.0.48.
-- Used to verify that PackageReferences without inline versions are resolved
-- from the NuGet CPM catalog.
dapperAOTCpmVersions :: Set.Set (Dependency)
dapperAOTCpmVersions =
  Set.fromList
    [ mkCpmDep "Dapper" "2.1.66"
    , mkCpmDep "Microsoft.CodeAnalysis.CSharp" "4.12.0"
    , mkCpmDep "Microsoft.Data.SqlClient" "6.0.1"
    , mkCpmDep "Microsoft.NET.Test.Sdk" "17.12.0"
    , mkCpmDep "Npgsql" "9.0.2"
    , mkCpmDep "Testcontainers.PostgreSql" "4.1.0"
    , mkCpmDep "xunit" "[2.3.0]"
    , mkCpmDep "xunit.runner.visualstudio" "[2.3.0]"
    ]
  where
    mkCpmDep name ver =
      Dependency
        { dependencyType = NuGetType
        , dependencyName = name
        , dependencyVersion = Just (CEq ver)
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = mempty
        }

testDapperAOTForCPM :: Spec
testDapperAOTForCPM =
  aroundAll (withAnalysisOf NonStrict dapperAOT) $ do
    describe "DapperAOT-CPM" $ do
      it "should find targets" $ \(result, _) -> do
        length result `shouldBe` 11

      -- Dapper.AOT.Test.Integration.csproj has only PackageReferences without
      -- inline Version attributes, so every direct dep must be resolved from
      -- the Directory.Packages.props catalog.
      it "should resolve PackageReference versions from Directory.Packages.props" $ \(result, _) -> do
        let testIntegration =
              find
                ( (== "Dapper.AOT.Test.Integration.csproj")
                    . fileName
                    . NuGet.nugetProjectFile
                    . Types.projectData
                    . fst
                )
                result
        case testIntegration of
          Nothing -> expectationFailure "expected to find Dapper.AOT.Test.Integration project"
          Just (_, deps) -> do
            let directDeps =
                  Set.fromList
                    . Graphing.directList
                    $ dependencyGraph deps
            directDeps `shouldBe` dapperAOTCpmVersions

spec :: Spec
spec = do
  testServiceStackForPkgReferences
  testServiceStackForPkgConfig
  testDotnetCoreTwoExampleForProjectAssetsJson
  testDapperAOTForCPM
