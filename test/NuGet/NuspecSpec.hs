module NuGet.NuspecSpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Data.String.Conversion (toString)
import Data.Text.IO qualified as TIO
import DepTypes
import GraphUtil
import Parse.XML
import Strategy.NuGet.Nuspec
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

nuspec :: Nuspec
nuspec = Nuspec groupList licenses (Just "test.com")

licenses :: [NuspecLicense]
licenses = [NuspecLicense "expression" "", NuspecLicense "" "BSD-2-Clause", NuspecLicense "expression" "  ", NuspecLicense "file" "", NuspecLicense "file" "doesnt-exist.txt", NuspecLicense "expression" "Foo"]

groupList :: [Group]
groupList = [Group [depOne, depTwo], Group [depThree]]

depOne :: NuGetDependency
depOne = NuGetDependency "one" "1.0.0"

depTwo :: NuGetDependency
depTwo = NuGetDependency "two" "2.0.0"

depThree :: NuGetDependency
depThree = NuGetDependency "three" "3.0.0"

nuspecWithVariables :: Nuspec
nuspecWithVariables = Nuspec [Group [NuGetDependency "good" "1.0.0", NuGetDependency "tokenPkg" "$version$"]] [] Nothing

dependencyGood :: Dependency
dependencyGood =
  Dependency
    { dependencyType = NuGetType
    , dependencyName = "good"
    , dependencyVersion = Just (CEq "1.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

dependencyTokenPkg :: Dependency
dependencyTokenPkg =
  Dependency
    { dependencyType = NuGetType
    , dependencyName = "tokenPkg"
    , dependencyVersion = Nothing
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

spec :: Spec
spec = do
  dependenciesAndLicense <- runIO (TIO.readFile "test/NuGet/testdata/nuspec/test.nuspec")
  singleLicense <- runIO (TIO.readFile "test/NuGet/testdata/nuspec/license.nuspec")
  multipleLicenses <- runIO (TIO.readFile "test/NuGet/testdata/nuspec/multiple-licenses.nuspec")

  describe "nuspec analyzer" $ do
    it "reads a file and constructs an accurate graph" $ do
      case parseXML dependenciesAndLicense of
        Right project -> do
          (groups project) `shouldContain` groupList
          (license project) `shouldMatchList` [NuspecLicense "file" "license-file"]
          (licenseUrl project) `shouldBe` (Just "https://licence.location.com/LICENSE.md")
        Left err -> expectationFailure (toString ("could not parse nuspec file: " <> xmlErrorPretty err))

    it "reads a file and extracts the correct license" $ do
      case parseXML singleLicense of
        Right project -> do
          (groups project) `shouldBe` []
          (license project) `shouldMatchList` [NuspecLicense "file" "LICENSE.txt"]
          (licenseUrl project) `shouldBe` Nothing
        Left err -> expectationFailure (toString ("could not parse nuspec file: " <> xmlErrorPretty err))

    it "reads a file with multiple licenses" $ do
      case parseXML multipleLicenses of
        Right project -> do
          (groups project) `shouldBe` []
          (license project) `shouldMatchList` licenses
          (licenseUrl project) `shouldBe` (Just "test.com")
        Left err -> expectationFailure (toString ("could not parse nuspec file: " <> xmlErrorPretty err))

    it "constructs an accurate graph" $ do
      let graph = buildGraph nuspec
      expectDeps [dependencyOne, dependencyTwo, dependencyThree] graph
      expectDirect [dependencyOne, dependencyTwo, dependencyThree] graph
      expectEdges [] graph

    it "should filter unresolved NuGet token variable versions" $ do
      let graph = buildGraph nuspecWithVariables
      expectDeps [dependencyGood, dependencyTokenPkg] graph
      expectDirect [dependencyGood, dependencyTokenPkg] graph
      expectEdges [] graph
