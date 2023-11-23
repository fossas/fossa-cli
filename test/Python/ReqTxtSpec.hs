{-# LANGUAGE QuasiQuotes #-}

module Python.ReqTxtSpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Text.URI.QQ (uri)

import Data.Foldable
import DepTypes
import Effect.Grapher
import Graphing (Graphing)
import Strategy.Python.Pip (Package (..))
import Strategy.Python.Util

import Test.Hspec

setupPyInput :: [Req]
setupPyInput =
  [ NameReq
      "pkgOne"
      Nothing
      ( Just
          [ Version OpGtEq "1.0.0"
          , Version OpLt "2.0.0"
          ]
      )
      Nothing
  , NameReq "pkgTwo" Nothing Nothing Nothing
  , UrlReq "pkgThree" Nothing [uri|https://example.com|] Nothing
  ]

installedPackages :: [Package]
installedPackages =
  [ Package "foo" "2" []
  , Package "pkgOne" "1.0.0" [Package "pkgOne_One" "3.0" []]
  , Package "pkgTwo" "1" [Package "pkgTwo_One" "1" []]
  , Package "pkgNotThree" "https://example-not.com" [Package "ignored_me" "1" []]
  ]

deps :: [(Dependency, [Dependency])]
deps =
  [
    ( Dependency
        { dependencyType = PipType
        , dependencyName = "pkgOne"
        , dependencyVersion =
            Just
              ( CAnd
                  (CGreaterOrEq "1.0.0")
                  (CLess "2.0.0")
              )
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }
    ,
      [ Dependency
          { dependencyType = PipType
          , dependencyName = "pkgOne_One"
          , dependencyVersion = Just (CEq "3.0")
          , dependencyLocations = []
          , dependencyEnvironments = mempty
          , dependencyTags = Map.empty
          }
      ]
    )
  ,
    ( Dependency
        { dependencyType = PipType
        , dependencyName = "pkgTwo"
        , dependencyVersion = Nothing
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }
    ,
      [ Dependency
          { dependencyType = PipType
          , dependencyName = "pkgTwo_One"
          , dependencyVersion = Just (CEq "1")
          , dependencyLocations = []
          , dependencyEnvironments = mempty
          , dependencyTags = Map.empty
          }
      ]
    )
  ,
    ( Dependency
        { dependencyType = PipType
        , dependencyName = "pkgThree"
        , dependencyVersion = Just (CURI "https://example.com")
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }
    , []
    )
  ]

expected :: Graphing Dependency
expected = run . evalGrapher $ traverse (\(d, _) -> direct d) deps

transitiveExpected :: Graphing Dependency
transitiveExpected = run . evalGrapher $
  for_ deps $ \(dep, children) -> do
    direct dep
    for_ children $ \c -> do
      deep c
      edge dep c


spec :: Spec
spec =
  describe "analyze" $ do
    it "should produce expected output" $ do
      let result = buildGraph Nothing setupPyInput

      result `shouldBe` expected

    it "should only report transitive dependencies for packages found in req.txt" $ do
      let result = buildGraph (Just installedPackages) setupPyInput

      result `shouldBe` transitiveExpected
