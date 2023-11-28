{-# LANGUAGE QuasiQuotes #-}

module Python.ReqTxtSpec (
  spec,
) where

import Control.Monad (void)
import Data.Map.Strict qualified as Map
import DepTypes
import Effect.Grapher
import Graphing (Graphing)
import Strategy.Python.Pip (Package (..))
import Strategy.Python.Util
import Text.URI.QQ (uri)

import Test.Hspec

newtype ExpectedDependency = ExpectedDependency (Dependency, [ExpectedDependency])

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

expectedDeps :: [ExpectedDependency]
expectedDeps =
  [ ExpectedDependency
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
        [ ExpectedDependency
            ( Dependency
                { dependencyType = PipType
                , dependencyName = "pkgOne_One"
                , dependencyVersion = Just (CEq "3.0")
                , dependencyLocations = []
                , dependencyEnvironments = mempty
                , dependencyTags = Map.empty
                }
            , []
            )
        ]
      )
  , ExpectedDependency
      ( Dependency
          { dependencyType = PipType
          , dependencyName = "pkgTwo"
          , dependencyVersion = Nothing
          , dependencyLocations = []
          , dependencyEnvironments = mempty
          , dependencyTags = Map.empty
          }
      ,
        [ ExpectedDependency
            ( Dependency
                { dependencyType = PipType
                , dependencyName = "pkgTwo_One"
                , dependencyVersion = Just (CEq "1")
                , dependencyLocations = []
                , dependencyEnvironments = mempty
                , dependencyTags = Map.empty
                }
            , []
            )
        ]
      )
  , ExpectedDependency
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

traverseDirect :: [ExpectedDependency] -> Graphing Dependency
traverseDirect deps = run . evalGrapher $ do
  traverse
    ( \(ExpectedDependency (dep, _)) -> do
        direct dep
    )
    deps

traverseDirectAndDeep :: [ExpectedDependency] -> Graphing Dependency
traverseDirectAndDeep deps = run . evalGrapher $ do
  traverse
    ( \(ExpectedDependency (dep, deepDeps)) -> do
        direct dep
        traverseDeepDeps dep deepDeps
    )
    deps
  where
    traverseDeepDeps parent children = do
      traverse addDeps children
      where
        addDeps (ExpectedDependency (child, deeperDeps)) = do
          deep child
          edge parent child
          void $ traverseDeepDeps child deeperDeps

spec :: Spec
spec =
  describe "analyze" $ do
    it "should produce expected output" $ do
      let result = buildGraph Nothing setupPyInput

      result `shouldBe` traverseDirect expectedDeps

    it "should only report transitive dependencies for packages found in req.txt" $ do
      let result = buildGraph (Just installedPackages) setupPyInput

      result `shouldBe` traverseDirectAndDeep expectedDeps
