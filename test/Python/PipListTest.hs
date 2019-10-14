module Python.PipListTest
  ( spec_analyze
  ) where

import Prologue

import qualified Data.Map.Strict as M
import           Polysemy
import           Polysemy.Input

import           Effect.GraphBuilder
import qualified Graph as G
import           Strategy.Python.PipList

import Test.Tasty.Hspec

expected :: G.Graph
expected = run . evalGraphBuilder G.empty $ do
  ref1 <- addNode (G.Dependency { dependencyType = G.PipType
                        , dependencyName = "pkgOne"
                        , dependencyVersion = Just (G.CEq "1.0.0")
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        })
  ref2 <- addNode (G.Dependency { dependencyType = G.PipType
                        , dependencyName = "pkgTwo"
                        , dependencyVersion = Just (G.CEq "2.0.0")
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        })
  addDirect ref1
  addDirect ref2

pipListOutput :: [PipListDep]
pipListOutput =
  [ PipListDep { depName = "pkgOne"
                , depVersion = "1.0.0"
                }
  , PipListDep { depName = "pkgTwo"
                , depVersion = "2.0.0"
                }
  ]

spec_analyze :: Spec
spec_analyze =
  describe "analyze" $
    it "produces the expected output" $ do
      let result = analyze
            & runInputConst @[PipListDep] pipListOutput
            & run
      result `shouldBe` expected
