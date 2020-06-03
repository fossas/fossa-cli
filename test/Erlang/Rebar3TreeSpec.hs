module Erlang.Rebar3TreeSpec
  ( spec
  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.Text.IO as TIO
import Text.Megaparsec

import DepTypes
import Graphing()
import Strategy.Erlang.Rebar3Tree
import GraphUtil

import Test.Hspec

dependencyOne :: Dependency
dependencyOne = Dependency { dependencyType = GitType
                      , dependencyName = "https://github.com/dep/one"
                      , dependencyVersion = Just (CEq "1.0.0")
                      , dependencyLocations = []
                      , dependencyEnvironments = []
                      , dependencyTags = M.empty
                      }

dependencyTwo :: Dependency
dependencyTwo = Dependency { dependencyType = HexType
                      , dependencyName = "two"
                      , dependencyVersion = Just (CEq "2.0.0")
                      , dependencyLocations = []
                      , dependencyEnvironments = []
                      , dependencyTags = M.empty
                      }
dependencyThree :: Dependency
dependencyThree = Dependency { dependencyType = HexType
                      , dependencyName = "three"
                      , dependencyVersion = Just (CEq "3.0.0")
                      , dependencyLocations = []
                      , dependencyEnvironments = []
                      , dependencyTags = M.empty
                      }

dependencyFour :: Dependency
dependencyFour = Dependency { dependencyType = GitType
                      , dependencyName = "https://github.com/dep/four"
                      , dependencyVersion = Just (CEq "4.0.0")
                      , dependencyLocations = []
                      , dependencyEnvironments = []
                      , dependencyTags = M.empty
                      }

dependencyFive :: Dependency
dependencyFive = Dependency { dependencyType = HexType
                      , dependencyName = "five"
                      , dependencyVersion = Just (CEq "5.0.0")
                      , dependencyLocations = []
                      , dependencyEnvironments = []
                      , dependencyTags = M.empty
                      }

depOne :: Rebar3Dep
depOne = Rebar3Dep 
          { depName = "one"
          , depVersion = "1.0.0"
          , depLocation = "https://github.com/dep/one"
          , subDeps = [depTwo, depFour]
          }
 
depTwo :: Rebar3Dep
depTwo = Rebar3Dep 
          { depName = "two"
          , depVersion = "2.0.0"
          , depLocation = "hex package"
          , subDeps = [depThree]
          }         

depThree :: Rebar3Dep
depThree = Rebar3Dep 
          { depName = "three"
          , depVersion = "3.0.0"
          , depLocation = "hex package"
          , subDeps = []
          }         

depFour :: Rebar3Dep
depFour = Rebar3Dep 
          { depName = "four"
          , depVersion = "4.0.0"
          , depLocation = "https://github.com/dep/four"
          , subDeps = []
          }         

depFive :: Rebar3Dep
depFive = Rebar3Dep 
          { depName = "five"
          , depVersion = "5.0.0"
          , depLocation = "hex package"
          , subDeps = []
          }         

spec :: Spec
spec = do
  contents <- runIO (TIO.readFile "test/Erlang/testdata/rebar3tree")

  describe "rebar3 tree analyzer" $
    it "produces the expected output" $ do
      let res = buildGraph [depOne, depFive]
      expectDeps [dependencyOne, dependencyTwo, dependencyThree, dependencyFour, dependencyFive] res
      expectDirect [dependencyOne, dependencyFive] res
      expectEdges [(dependencyOne, dependencyTwo), (dependencyOne, dependencyFour), (dependencyTwo, dependencyThree)] res

  describe "rebar3 tree parser" $ do
    it "parses ideal rebar3 tree output" $ do
      case runParser rebar3TreeParser "" contents of
        Left failCode -> traceM $ show failCode
        Right result -> result `shouldMatchList` [depOne, depFive]