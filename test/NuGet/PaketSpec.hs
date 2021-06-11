module NuGet.PaketSpec (
  spec,
) where

import Data.Map.Strict qualified as M
import Data.Text.IO qualified as TIO
import DepTypes
import GraphUtil
import Strategy.NuGet.Paket
import Test.Hspec qualified as T
import Text.Megaparsec

dependencyOne :: Dependency
dependencyOne =
  Dependency
    { dependencyType = NuGetType
    , dependencyName = "one"
    , dependencyVersion = Just (CEq "1.0.0")
    , dependencyLocations = ["nuget.com"]
    , dependencyEnvironments = []
    , dependencyTags = M.fromList [("location", ["NUGET"]), ("group", ["MAIN"])]
    }

dependencyTwo :: Dependency
dependencyTwo =
  Dependency
    { dependencyType = NuGetType
    , dependencyName = "two"
    , dependencyVersion = Just (CEq "2.0.0")
    , dependencyLocations = ["nuget-v2.com", "nuget.com"]
    , dependencyEnvironments = []
    , dependencyTags = M.fromList [("location", ["NUGET"]), ("group", ["MAIN", "TEST"])]
    }

dependencyThree :: Dependency
dependencyThree =
  Dependency
    { dependencyType = NuGetType
    , dependencyName = "three"
    , dependencyVersion = Just (CEq "3.0.0")
    , dependencyLocations = ["custom-site.com"]
    , dependencyEnvironments = []
    , dependencyTags = M.fromList [("location", ["HTTP"]), ("group", ["MAIN"])]
    }

dependencyFour :: Dependency
dependencyFour =
  Dependency
    { dependencyType = NuGetType
    , dependencyName = "four"
    , dependencyVersion = Just (CEq "4.0.0")
    , dependencyLocations = ["nuget-v2.com"]
    , dependencyEnvironments = []
    , dependencyTags = M.fromList [("location", ["NUGET"]), ("group", ["TEST"])]
    }

dependencyFive :: Dependency
dependencyFive =
  Dependency
    { dependencyType = NuGetType
    , dependencyName = "five"
    , dependencyVersion = Just (CEq "5.0.0")
    , dependencyLocations = ["nuget-v2.com"]
    , dependencyEnvironments = []
    , dependencyTags = M.fromList [("location", ["NUGET"]), ("group", ["TEST"])]
    }

dependencySix :: Dependency
dependencySix =
  Dependency
    { dependencyType = NuGetType
    , dependencyName = "six"
    , dependencyVersion = Just (CEq "6.0.0")
    , dependencyLocations = ["github.com"]
    , dependencyEnvironments = []
    , dependencyTags = M.fromList [("location", ["GITHUB"]), ("group", ["TEST"])]
    }

nugetSection :: Section
nugetSection =
  StandardSection
    "NUGET"
    [ Remote
        "nuget.com"
        [ PaketDep "one" "1.0.0" ["two"]
        , PaketDep "two" "2.0.0" []
        ]
    ]

httpSection :: Section
httpSection = StandardSection "HTTP" [Remote "custom-site.com" [PaketDep "three" "3.0.0" []]]

nugetGroupRemote :: Remote
nugetGroupRemote =
  Remote
    "nuget-v2.com"
    [ PaketDep "four" "4.0.0" ["five"]
    , PaketDep "five" "5.0.0" []
    , PaketDep "two" "2.0.0" []
    ]

gitGroupRemote :: Remote
gitGroupRemote = Remote "github.com" [PaketDep "six" "6.0.0" []]

groupSection :: Section
groupSection = GroupSection "TEST" [StandardSection "NUGET" [nugetGroupRemote], StandardSection "GITHUB" [gitGroupRemote]]

paketLockSections :: [Section]
paketLockSections = [nugetSection, httpSection, groupSection]

spec :: T.Spec
spec = do
  T.describe "paket lock analyzer" $
    T.it "produces the expected output" $ do
      let graph = buildGraph paketLockSections

      expectDeps [dependencyOne, dependencyTwo, dependencyThree, dependencyFour, dependencyFive, dependencySix] graph
      expectDirect [dependencyOne, dependencyTwo, dependencyThree, dependencyFour, dependencyFive, dependencySix] graph
      expectEdges
        [ (dependencyOne, dependencyTwo)
        , (dependencyFour, dependencyFive)
        ]
        graph

  paketLockFile <- T.runIO (TIO.readFile "test/NuGet/testdata/paket.lock")
  T.describe "paket lock parser" $
    T.it "parses error messages into an empty list" $
      case runParser findSections "" paketLockFile of
        Left _ -> T.expectationFailure "failed to parse"
        Right result -> do
          result `T.shouldContain` [nugetSection]
          result `T.shouldContain` [httpSection]
          result `T.shouldContain` [groupSection]
