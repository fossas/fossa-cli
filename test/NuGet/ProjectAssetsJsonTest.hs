{-# language TemplateHaskell #-}

module NuGet.ProjectAssetsJsonTest
  ( spec_analyze
  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS
import           Polysemy
import           Polysemy.Input

import DepTypes
import GraphUtil

import Strategy.NuGet.ProjectAssetsJson

import Test.Tasty.Hspec

dependencyOne :: Dependency
dependencyOne = Dependency { dependencyType = NuGetType
                        , dependencyName = "one"
                        , dependencyVersion = Just (CEq "1.0.0")
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        }

dependencyTwo :: Dependency
dependencyTwo = Dependency { dependencyType = NuGetType
                        , dependencyName = "two"
                        , dependencyVersion = Just (CEq "2.0.0")
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        }

dependencyThree :: Dependency
dependencyThree = Dependency { dependencyType = NuGetType
                        , dependencyName = "three"
                        , dependencyVersion = Just (CEq "3.0.0")
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        }

dependencyFour :: Dependency
dependencyFour = Dependency { dependencyType = NuGetType
                        , dependencyName = "four"
                        , dependencyVersion = Just (CEq "4.0.0")
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        }

spec_analyze :: Spec
spec_analyze = do
  testFile <- runIO (BS.readFile "test/NuGet/testdata/project.assets.json")

  describe "project.assets.json analyzer" $ do
    it "reads a file and constructs an accurate graph" $ do
      case eitherDecodeStrict testFile of
        Right res -> do
              let graph = analyze & runInputConst @ProjectAssetsJson res & run
              expectDeps [dependencyOne, dependencyTwo, dependencyThree, dependencyFour] graph
              expectDirect [dependencyOne, dependencyTwo, dependencyFour] graph
              expectEdges [ (dependencyOne, dependencyThree) ] graph
        Left _ -> expectationFailure "failed to parse"