module NuGet.PackageReferenceSpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Data.String.Conversion (toString)
import Data.Text.IO qualified as TIO
import DepTypes
import GraphUtil
import Parse.XML
import Strategy.NuGet.PackageReference
import Test.Hspec

dependencyOne :: Dependency
dependencyOne =
  Dependency
    { dependencyType = NuGetType
    , dependencyName = "one"
    , dependencyVersion = Just (CEq "1.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

dependencyTwo :: Dependency
dependencyTwo =
  Dependency
    { dependencyType = NuGetType
    , dependencyName = "two"
    , dependencyVersion = Just (CEq "2.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

dependencyThree :: Dependency
dependencyThree =
  Dependency
    { dependencyType = NuGetType
    , dependencyName = "three"
    , dependencyVersion = Just (CEq "3.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

dependencyFour :: Dependency
dependencyFour =
  Dependency
    { dependencyType = NuGetType
    , dependencyName = "four"
    , dependencyVersion = Nothing
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

packageReference :: PackageReference
packageReference = PackageReference itemGroupList

itemGroupList :: [ItemGroup]
itemGroupList = [ItemGroup [refOne, refTwo], ItemGroup [refThree, refFour]]

refOne :: Package
refOne = Package "one" $ Just "1.0.0"

refTwo :: Package
refTwo = Package "two" $ Just "2.0.0"

refThree :: Package
refThree = Package "three" $ Just "3.0.0"

refFour :: Package
refFour = Package "four" Nothing

-- Packages with unresolved MSBuild variables (should be filtered out)
refWithMSBuildVar :: Package
refWithMSBuildVar = Package "unresolved-msbuild" $ Just "$(PackageVersion)"

refWithLegacyVar :: Package
refWithLegacyVar = Package "unresolved-legacy" $ Just "$documentProcessingContractsVersion$"

refWithMixedVar :: Package
refWithMixedVar = Package "unresolved-mixed" $ Just "1.0.$(BuildNumber)"

spec :: Spec
spec = do
  refFile <- runIO (TIO.readFile "test/NuGet/testdata/test.csproj")

  describe "Package Reference parser" $ do
    it "reads a file and constructs an accurate list of item groups" $ do
      case parseXML refFile of
        Right project -> (groups project) `shouldContain` itemGroupList
        Left err -> expectationFailure (toString ("could not parse package reference file" <> xmlErrorPretty err))

    it "constructs an accurate graph" $ do
      let graph = buildGraph packageReference
      expectDeps [dependencyOne, dependencyTwo, dependencyThree, dependencyFour] graph
      expectDirect [dependencyOne, dependencyTwo, dependencyThree, dependencyFour] graph
      expectEdges [] graph

    it "filters out packages with unresolved MSBuild variables" $ do
      let packageRefWithVars =
            PackageReference
              [ ItemGroup
                  [ refOne
                  , refWithMSBuildVar -- $(PackageVersion) - should be filtered
                  , refWithLegacyVar -- $documentProcessingContractsVersion$ - should be filtered
                  , refWithMixedVar -- 1.0.$(BuildNumber) - should be filtered
                  , refTwo
                  ]
              ]
          graph = buildGraph packageRefWithVars
      -- Only refOne and refTwo should be in the graph
      expectDeps [dependencyOne, dependencyTwo] graph
      expectDirect [dependencyOne, dependencyTwo] graph
