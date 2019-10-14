{-# language QuasiQuotes #-}

module Python.SetupPyTest
  ( spec_analyze
  ) where

import Prologue

import qualified Data.Map.Strict as M
import           Polysemy
import           Polysemy.Input
import           Text.URI.QQ (uri)

import           Effect.GraphBuilder
import qualified Graph as G
import           Strategy.Python.SetupPy
import           Strategy.Python.Util

import Test.Tasty.Hspec

setupPyInput :: [Req]
setupPyInput =
  [ NameReq "pkgOne" Nothing (Just [ Version OpGtEq "1.0.0"
                                   , Version OpLt   "2.0.0"
                                   ]) Nothing
  , NameReq "pkgTwo" Nothing Nothing Nothing
  , UrlReq "pkgThree" Nothing [uri|https://example.com/|] Nothing
  ]

expected :: G.Graph
expected = run . evalGraphBuilder G.empty $ do
  ref1 <- addNode (G.Dependency { dependencyType = G.PipType
                        , dependencyName = "pkgOne"
                        , dependencyVersion =
                            Just (G.CAnd (G.CGreaterOrEq "1.0.0")
                                         (G.CLess "2.0.0"))
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        })
  ref2 <- addNode (G.Dependency { dependencyType = G.PipType
                        , dependencyName = "pkgTwo"
                        , dependencyVersion = Nothing
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        })
  ref3 <- addNode (G.Dependency { dependencyType = G.PipType
                        , dependencyName = "pkgThree"
                        , dependencyVersion = Just (G.CURI "https://example.com/")
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        })
  addDirect ref1
  addDirect ref2
  addDirect ref3

spec_analyze :: Spec
spec_analyze =
  describe "analyze" $
    it "should produce expected output" $ do
      let result = analyze
            & runInputConst @[Req] setupPyInput
            & run

      result `shouldBe` expected
