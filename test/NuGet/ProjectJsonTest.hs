{-# language TemplateHaskell #-}

module NuGet.ProjectJsonTest
  ( spec_analyze
  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS

import DepTypes
import GraphUtil

import Strategy.NuGet.ProjectJson

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
                        , dependencyVersion = Just (CCompatible "2.*")
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        }

dependencyThree :: Dependency
dependencyThree = Dependency { dependencyType = NuGetType
                        , dependencyName = "three"
                        , dependencyVersion = Just (CEq "3.0.0")
                        , dependencyLocations = []
                        , dependencyTags = M.fromList [ ("type", ["test"]) ]
                        }

spec_analyze :: Spec
spec_analyze = do
  testFile <- runIO (BS.readFile "test/NuGet/testdata/project.json")

  describe "project.json analyzer" $ do
    it "reads a file and constructs an accurate graph" $ do
      case eitherDecodeStrict testFile of
        Right res -> do
              let graph = buildGraph res
              expectDeps [dependencyOne, dependencyTwo, dependencyThree] graph
              expectDirect [dependencyOne, dependencyTwo, dependencyThree] graph
              expectEdges [] graph
        Left _ -> expectationFailure "failed to parse"
