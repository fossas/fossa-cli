module Ruby.BundleShowTest
  ( spec_analyze
  ) where

import Prologue

import qualified Data.Map.Strict as M
import           Polysemy
import           Polysemy.Input
import qualified Data.Text.IO as TIO
import           Text.Megaparsec

import           Effect.GraphBuilder
import qualified Graph as G
import           Strategy.Ruby.BundleShow

import Test.Tasty.Hspec

expected :: G.Graph
expected = run . evalGraphBuilder G.empty $ do
  ref1 <- addNode (G.Dependency { dependencyType = G.GemType
                        , dependencyName = "pkgOne"
                        , dependencyVersion = Just (G.CEq "1.0.0")
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        })
  ref2 <- addNode (G.Dependency { dependencyType = G.GemType
                        , dependencyName = "pkgTwo"
                        , dependencyVersion = Just (G.CEq "2.0.0")
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        })
  addDirect ref1
  addDirect ref2

bundleShowOutput :: [BundleShowDep]
bundleShowOutput =
  [ BundleShowDep { depName = "pkgOne"
                , depVersion = "1.0.0"
                }
  , BundleShowDep { depName = "pkgTwo"
                , depVersion = "2.0.0"
                }
  ]

spec_analyze :: Spec
spec_analyze = do
  contents <- runIO (TIO.readFile "test/Ruby/testdata/bundleShow")
  complexShow <- runIO (TIO.readFile "test/Ruby/testdata/bundleShowComplex")
  emptyShow <- runIO (TIO.readFile "test/Ruby/testdata/bundleShowEmpty")

  describe "bundle show analyzer" $
    it "produces the expected output" $ do
      let result = analyze
            & runInputConst @[BundleShowDep] bundleShowOutput
            & run
      result `shouldBe` expected

  describe "bundle show parser" $ do
    it "parses ideal bundle show output" $ do
      case runParser bundleShowParser "" contents of
        Left _ -> expectationFailure "failed to parse"
        Right result -> result `shouldMatchList` bundleShowOutput

    it "parses complex bundle show output" $ do
      case runParser bundleShowParser "" complexShow of
        Left _ -> expectationFailure "failed to parse"
        Right result -> length result `shouldBe` 130

    it "parses error messages into an empty list" $ do
      case runParser bundleShowParser "" emptyShow of
        Left _ -> expectationFailure "failed to parse"
        Right result -> result `shouldMatchList` []
