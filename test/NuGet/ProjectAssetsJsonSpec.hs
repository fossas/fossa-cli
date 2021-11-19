module NuGet.ProjectAssetsJsonSpec (
  spec,
) where

import Data.Aeson
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.String.Conversion (toString)
import Data.Text (Text)
import DepTypes
import GraphUtil
import Strategy.NuGet.ProjectAssetsJson (FrameworkName (..), approxEql, buildGraph)
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
    , dependencyVersion = Just (CEq "4.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

dependencyFive :: Dependency
dependencyFive =
  Dependency
    { dependencyType = NuGetType
    , dependencyName = "five"
    , dependencyVersion = Just (CEq "5.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

-- | Reference: https://github.com/NuGet/NuGet.Client/blob/820bfe9d5e5e4e7bf0b6310c285db7b07aff3087/test/NuGet.Core.Tests/NuGet.Frameworks.Test/NuGetFrameworkParseTests.cs
testCasesForEquivalentFrameworks :: [(Text, Text)]
testCasesForEquivalentFrameworks =
  [ -- Short
    ("45", "net45")
  , ("40", "net40")
  , ("35", "net35")
  , ("4.5", "net45")
  , ("4.0", "net40")
  , ("3.5", "net35")
  , ("2.0", "net20")
  , -- Portables
    (".NETPortable40-Profile1", ".NETPortable,Version=v4.0,Profile=Profile1")
  , -- Full names
    (".NETPlatform,Version=v1.0", ".NETPlatform,Version=v1.0")
  , (".NETPlatform,Version=v5.0", ".NETPlatform,Version=v5.0")
  , (".NETFramework,Version=v4.5", ".NETFramework,Version=v4.5")
  , ("NETFramework,Version=v4.5", ".NETFramework,Version=v4.5")
  , (".NETPortable,Version=v0.0,Profile=Profile7", ".NETPortable,Version=v0.0,Profile=Profile7")
  , -- Short to Full
    ("net45", ".NETFramework,Version=v4.5")
  , ("net10", ".NETFramework,Version=v1.0")
  , ("net20", ".NETFramework,Version=v2.0")
  , ("net40", ".NETFramework,Version=v4.0")
  , ("net35", ".NETFramework,Version=v3.5")
  , ("net40-client", ".NETFramework,Version=v4.0,Profile=Client")
  , ("net5.0", ".NetCoreApp,Version=v5.0")
  , ("net5.0", "net5.0")
  , ("net", ".NETFramework,Version=v0.0")
  , ("net10.1.2.3", ".NETFramework,Version=v10.1.2.3")
  , ("net10.0", ".NETFramework,Version=v10.0")
  , ("net45-cf", ".NETFramework,Version=v4.5,Profile=CompactFramework")
  , ("uap10.0", "UAP,Version=v10.0")
  , ("netstandard", ".NETStandard,Version=v0.0")
  , ("netstandard1.0", ".NETStandard,Version=v1.0")
  , ("netstandard1.0", ".NETStandard,Version=v1.0.0")
  , ("netstandard0.9", ".NETStandard,Version=v0.9")
  , ("netstandard1.1", ".NETStandard,Version=v1.1")
  , ("netstandard1.2", ".NETStandard,Version=v1.2")
  , ("netstandard1.3", ".NETStandard,Version=v1.3")
  , ("netstandard1.4", ".NETStandard,Version=v1.4")
  , ("netstandard1.5", ".NETStandard,Version=v1.5")
  , ("netcoreapp", ".NETCoreApp,Version=v0.0")
  , ("netcoreapp1.0", ".NETCoreApp,Version=v1.0")
  , ("netcoreapp1.5", ".NETCoreApp,Version=v1.5")
  , ("netcoreapp2.0", ".NetCoreApp,Version=v2.0")
  , ("netcoreapp3.0", ".NetCoreApp,Version=v3.0")
  , ("net5.0", "netcoreapp5.0")
  , ("net5.0-windows", "netcoreapp5.0-windows")
  , ("net5.0-windows10.0", "netcoreapp5.0-windows10.0")
  , ("net5.0-android", "net5.0-android")
  , ("net5.0-android", "net5.0-android0.0")
  , ("net5.0-android10.0", "net5.0-android10")
  , ("net5.0-ios14.0", "net5.0-ios14.0")
  , ("net5.0-macos10.0", "net5.0-macos10.0")
  , ("net5.0-watchos1.0", "net5.0-watchos1.0")
  , ("net5.0-tvos1.0", "net5.0-tvos1.0")
  , ("net5.0-windows10.0", "net5.0-windows10.0")
  , ("net5.0-macos10.15.2.3", "net5.0-macos10.15.2.3")
  , -- Common
    ("net40", "net4")
  , ("net40", "net40")
  , ("net4", "net40")
  , ("net4", "net4")
  , ("net45", "net45")
  , ("net451", "net451")
  , ("net461", "net461")
  , ("net462", "net462")
  , ("win8", "win8")
  , ("win81", "win81")
  , ("netstandard", "netstandard")
  , ("netstandard1.0", "netstandard1.0")
  , ("netstandard1.0", "netstandard10")
  , ("netstandard10", "netstandard1.0")
  , ("netstandard10", "netstandard10")
  , ("netstandard1.1", "netstandard1.1")
  , ("netstandard1.1", "netstandard11")
  , ("netstandard11", "netstandard1.1")
  , ("netstandard11", "netstandard11")
  , ("netstandard1.2", "netstandard1.2")
  , ("netstandard1.2", "netstandard12")
  , ("netstandard12", "netstandard1.2")
  , ("netstandard12", "netstandard12")
  , ("netstandard1.3", "netstandard1.3")
  , ("netstandard1.3", "netstandard13")
  , ("netstandard13", "netstandard1.3")
  , ("netstandard13", "netstandard13")
  , ("netstandard1.4", "netstandard1.4")
  , ("netstandard1.4", "netstandard14")
  , ("netstandard14", "netstandard1.4")
  , ("netstandard14", "netstandard14")
  , ("netstandard1.5", "netstandard1.5")
  , ("netstandard1.5", "netstandard15")
  , ("netstandard15", "netstandard1.5")
  , ("netstandard15", "netstandard15")
  , ("netstandard1.6", "netstandard1.6")
  , ("netstandard1.6", "netstandard16")
  , ("netstandard16", "netstandard1.6")
  , ("netstandard16", "netstandard16")
  , ("netstandard1.7", "netstandard1.7")
  , ("netstandard1.7", "netstandard17")
  , ("netstandard17", "netstandard1.7")
  , ("netstandard17", "netstandard17")
  , ("netstandard2.0", "netstandard2.0")
  , ("netstandard2.0", "netstandard20")
  , ("netstandard20", "netstandard2.0")
  , ("netstandard20", "netstandard20")
  , ("netstandard2.1", "netstandard2.1")
  , ("netstandard2.1", "netstandard21")
  , ("netstandard21", "netstandard2.1")
  , ("netstandard21", "netstandard21")
  , ("netcoreapp2.1", "netcoreapp2.1")
  , ("netcoreapp2.1", "netcoreapp21")
  , ("netcoreapp21", "netcoreapp2.1")
  , ("netcoreapp21", "netcoreapp21")
  , ("netcoreapp3.1", "netcoreapp3.1")
  , ("netcoreapp3.1", "netcoreapp31")
  , ("netcoreapp31", "netcoreapp3.1")
  , ("netcoreapp31", "netcoreapp31")
  , ("net5.0", "net5.0")
  , ("net50", "net5.0")
  , ("net452", ".NETFramework,Version=v4.5.2/win7-x86")
  , ("net472", ".NETFramework,Version=4.7.2")
  ]

spec :: Spec
spec = do
  testFile <- runIO (BS.readFile "test/NuGet/testdata/project.assets.json")

  describe "approxEqlFramework" $ do
    it "should return false when provided with not equivalent framework" $ do
      approxEql (FrameworkName "net472") (FrameworkName ".NETFramework,Version=4.5.2") `shouldBe` False
      approxEql (FrameworkName "netstandard2.0") (FrameworkName ".NETStandard,Version=3.0") `shouldBe` False

    for_ testCasesForEquivalentFrameworks $ \(candidateLhs, candidateRhs) -> do
      it ("should return true when, comparing " <> (toString candidateLhs) <> " ~ " <> (toString candidateRhs)) $ do
        approxEql (FrameworkName candidateLhs) (FrameworkName candidateRhs) `shouldBe` True

  describe "project.assets.json analyzer" $ do
    it "reads a file and constructs an accurate graph" $ do
      case eitherDecodeStrict testFile of
        Right res -> do
          let graph = buildGraph res
          expectDeps [dependencyOne, dependencyTwo, dependencyThree, dependencyFour, dependencyFive] graph
          expectDirect [dependencyOne, dependencyTwo, dependencyFour] graph
          expectEdges [(dependencyOne, dependencyThree)] graph
        Left err -> expectationFailure $ show err
