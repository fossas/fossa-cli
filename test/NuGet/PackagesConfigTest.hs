{-# language TemplateHaskell #-}

module NuGet.PackagesConfigTest
  ( spec_analyze
  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as BL
import qualified Text.XML.Light as XML
import DepTypes
import GraphUtil
import Strategy.NuGet.PackagesConfig
import Test.Tasty.Hspec
import Polysemy
import Polysemy.Input

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

depList :: [NuGetDependency]
depList = [NuGetDependency "one" "1.0.0", NuGetDependency "two" "2.0.0"]

spec_analyze :: Spec
spec_analyze = do
  nuspecFile <- runIO (BL.readFile "test/NuGet/testdata/packages.config")

  describe "packages.config analyzer" $ do
    it "reads a file and constructs an accurate graph" $ do
      case parsePackagesConfig =<< XML.parseXMLDoc nuspecFile of
        Just deps -> deps `shouldContain` depList
        Nothing -> expectationFailure "could not parse packages.config file"

    it "constructs an accurate graph" $ do
          let graph = analyze & runInputConst @[NuGetDependency] depList & run
          expectDeps [dependencyOne, dependencyTwo] graph
          expectDirect [dependencyOne, dependencyTwo] graph
          expectEdges [] graph