module Ruby.GemfileLockSpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as TIO
import DepTypes
import GraphUtil
import Strategy.Ruby.GemfileLock
import Test.Hspec qualified as T
import Text.Megaparsec

dependencyOne :: Dependency
dependencyOne =
  Dependency
    { dependencyType = GitType
    , dependencyName = "url-for-dep-one"
    , dependencyVersion = Just (CEq "12345")
    , dependencyLocations = ["temp@12345"]
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

dependencyTwo :: Dependency
dependencyTwo =
  Dependency
    { dependencyType = GemType
    , dependencyName = "dep-two"
    , dependencyVersion = Just (CEq "2.0.0")
    , dependencyLocations = ["remote"]
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

dependencyThree :: Dependency
dependencyThree =
  Dependency
    { dependencyType = GemType
    , dependencyName = "dep-three"
    , dependencyVersion = Just (CEq "3.0.0")
    , dependencyLocations = ["remote"]
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

gitSection :: Section
gitSection =
  GitSection
    "url-for-dep-one"
    (Just "12345")
    (Just "branch")
    [ Spec
        { specVersion = "1.0.0"
        , specName = "dep-one"
        , specDeps =
            [ SpecDep{depName = "dep-three"}
            , SpecDep{depName = "dep-two"}
            ]
        }
    ]

gemSection :: Section
gemSection =
  GemSection
    "remote"
    [ Spec
        { specVersion = "2.0.0"
        , specName = "dep-two"
        , specDeps = [SpecDep{depName = "dep-three"}]
        }
    , Spec
        { specVersion = "3.0.0"
        , specName = "dep-three"
        , specDeps = []
        }
    ]

dependencySection :: Section
dependencySection =
  DependencySection
    [ DirectDep{directName = "dep-one"}
    , DirectDep{directName = "dep-two"}
    ]

gemfileLockSection :: [Section]
gemfileLockSection = [gitSection, gemSection, dependencySection]

spec :: T.Spec
spec = do
  gemfileLock <- T.runIO (TIO.readFile "test/Ruby/testdata/gemfileLock")

  T.describe "gemfile lock analyzer" $
    T.it "produces the expected output" $ do
      let graph = buildGraph gemfileLockSection

      expectDeps [dependencyOne, dependencyTwo, dependencyThree] graph
      expectDirect [dependencyOne, dependencyTwo] graph
      expectEdges
        [ (dependencyOne, dependencyTwo)
        , (dependencyOne, dependencyThree)
        , (dependencyTwo, dependencyThree)
        ]
        graph

  T.describe "gemfile lock parser" $ do
    T.it "parses error messages into an empty list" $ do
      case runParser findSections "" gemfileLock of
        Left _ -> T.expectationFailure "failed to parse"
        Right result -> do
          result `T.shouldContain` [gitSection]
          result `T.shouldContain` [dependencySection]
          result `T.shouldContain` [gemSection]
