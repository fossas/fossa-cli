module NuGet.PackageReferenceSpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Data.String.Conversion (toString)
import Data.Text (Text)
import Data.Text qualified as Text
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

-- | A project file whose PackageReference items include MSBuild operations
-- other than Include/Update (here @Remove@), which name no package.
projectWithRemoveItems :: Text
projectWithRemoveItems =
  Text.unlines
    [ "<Project Sdk=\"Microsoft.NET.Sdk\">"
    , "  <ItemGroup>"
    , "    <PackageReference Include=\"one\" Version=\"1.0.0\" />"
    , "    <PackageReference Remove=\"one\" />"
    , "  </ItemGroup>"
    , "  <ItemGroup>"
    , "    <PackageReference Remove=\"unrelated\" />"
    , "    <PackageReference Update=\"two\" Version=\"2.0.0\" />"
    , "  </ItemGroup>"
    , "</Project>"
    ]

spec :: Spec
spec = do
  refFile <- runIO (TIO.readFile "test/NuGet/testdata/test.csproj")

  describe "Package Reference parser" $ do
    it "reads a file and constructs an accurate list of item groups" $ do
      case parseXML refFile of
        Right project -> (groups project) `shouldContain` itemGroupList
        Left err -> expectationFailure (toString ("could not parse package reference file" <> xmlErrorPretty err))

    -- Items without an Include or Update attribute used to fail the whole file with
    -- `Missing attribute at [Project.ItemGroup.PackageReference]; attrName: Update`.
    it "skips PackageReference items that name no package" $ do
      case parseXML projectWithRemoveItems of
        Right project -> groups project `shouldBe` [ItemGroup [refOne], ItemGroup [refTwo]]
        Left err -> expectationFailure (toString ("could not parse package reference file" <> xmlErrorPretty err))

    it "constructs an accurate graph" $ do
      let graph = buildGraph packageReference
      expectDeps [dependencyOne, dependencyTwo, dependencyThree, dependencyFour] graph
      expectDirect [dependencyOne, dependencyTwo, dependencyThree, dependencyFour] graph
      expectEdges [] graph

    it "resolves missing versions from CPM version map" $ do
      let versionMap = Map.fromList [("four", "4.0.0"), ("five", "5.0.0")]
          graph = buildGraphWithCPM versionMap packageReference
          dependencyFourResolved =
            dependencyFour{dependencyVersion = Just (CEq "4.0.0")}
      expectDeps [dependencyOne, dependencyTwo, dependencyThree, dependencyFourResolved] graph
      expectDirect [dependencyOne, dependencyTwo, dependencyThree, dependencyFourResolved] graph
      expectEdges [] graph

    it "prefers inline version over CPM version" $ do
      let versionMap = Map.fromList [("one", "9.9.9"), ("four", "4.0.0")]
          graph = buildGraphWithCPM versionMap packageReference
          dependencyFourResolved =
            dependencyFour{dependencyVersion = Just (CEq "4.0.0")}
      -- "one" keeps its inline version 1.0.0, not the CPM version 9.9.9
      expectDeps [dependencyOne, dependencyTwo, dependencyThree, dependencyFourResolved] graph
      expectDirect [dependencyOne, dependencyTwo, dependencyThree, dependencyFourResolved] graph
      expectEdges [] graph
