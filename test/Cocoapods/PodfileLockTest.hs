module Cocoapods.PodfileLockTest
  ( spec_analyze
  ) where

import Prologue

import qualified Data.Map.Strict as M
import           Polysemy
import           Polysemy.Input
import qualified Data.Text.IO as TIO
import           Text.Megaparsec

import DepTypes
import Strategy.Cocoapods.PodfileLock
import GraphUtil

import qualified Test.Tasty.Hspec as T

dependencyOne :: Dependency
dependencyOne = Dependency { dependencyType = PodType
                           , dependencyName = "one"
                           , dependencyVersion = Just (CEq "1.0.0")
                           , dependencyLocations = []
                           , dependencyTags = M.empty 
                           }

dependencyTwo :: Dependency
dependencyTwo = Dependency { dependencyType = PodType
                           , dependencyName = "two"
                           , dependencyVersion = Just (CEq "2.0.0")
                           , dependencyLocations = []
                           , dependencyTags = M.empty 
                           }

dependencyThree :: Dependency
dependencyThree = Dependency { dependencyType = PodType
                             , dependencyName = "three"
                             , dependencyVersion = Just (CEq "3.0.0")
                             , dependencyLocations = []
                             , dependencyTags = M.empty 
                             }

dependencyFour :: Dependency
dependencyFour = Dependency { dependencyType = PodType
                            , dependencyName = "four"
                            , dependencyVersion = Just (CEq "4.0.0")
                            , dependencyLocations = []
                            , dependencyTags = M.empty 
                            }

podSection :: Section
podSection = PodSection [Pod "one" "1.0.0" [Dep "two", Dep "three"], Pod "two" "2.0.0" [], Pod "three" "3.0.0" [Dep "four"], Pod "four" "4.0.0" []]

dependencySection :: Section
dependencySection = DependencySection [Dep "one", Dep "three"]

spec_analyze :: T.Spec
spec_analyze = do
  T.describe "podfile lock analyzer" $
    T.it "produces the expected output" $ do
      let graph = analyze
            & runInputConst @[Section] [podSection, dependencySection]
            & run
      expectDeps [dependencyOne, dependencyTwo, dependencyThree, dependencyFour] graph
      expectDirect [dependencyOne, dependencyThree] graph
      expectEdges [ (dependencyOne, dependencyTwo)
                  , (dependencyOne, dependencyThree)
                  , (dependencyThree, dependencyFour)
                  ] graph

  podLockFile <- T.runIO (TIO.readFile "test/Cocoapods/testdata/Podfile.lock")
  T.describe "podfile lock parser" $
    T.it "parses error messages into an empty list" $
      case runParser findSections "" podLockFile of
        Left _ -> T.expectationFailure "failed to parse"
        Right result -> do
            result `T.shouldContain` [podSection]
            result `T.shouldContain` [dependencySection]