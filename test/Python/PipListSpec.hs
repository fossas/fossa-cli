module Python.PipListSpec
  ( spec
  ) where

import Prologue

import qualified Data.Map.Strict as M

import DepTypes
import Effect.Grapher
import Graphing (Graphing)
import Strategy.Python.PipList

import Test.Hspec

expected :: Graphing Dependency
expected = run . evalGrapher $ do
  direct $ Dependency { dependencyType = PipType
                        , dependencyName = "pkgOne"
                        , dependencyVersion = Just (CEq "1.0.0")
                        , dependencyLocations = []
                        , dependencyEnvironments = []
                        , dependencyTags = M.empty
                        }
  direct $ Dependency { dependencyType = PipType
                        , dependencyName = "pkgTwo"
                        , dependencyVersion = Just (CEq "2.0.0")
                        , dependencyLocations = []
                        , dependencyEnvironments = []
                        , dependencyTags = M.empty
                        }

pipListOutput :: [PipListDep]
pipListOutput =
  [ PipListDep
    { depName = "pkgOne"
    , depVersion = "1.0.0"
    }
  , PipListDep
    { depName = "pkgTwo"
    , depVersion = "2.0.0"
    }
  ]

spec :: Spec
spec =
  describe "analyze" $
    it "produces the expected output" $ do
      let result = buildGraph pipListOutput
      result `shouldBe` expected
