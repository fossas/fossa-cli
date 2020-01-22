{-# language TemplateHaskell #-}

module NuGet.NuspecTest
  ( spec_analyze
  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import DepTypes
import GraphUtil
import Parse.XML
import Polysemy
import Polysemy.Input
import Strategy.NuGet.Nuspec
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

nuspec :: Nuspec
nuspec = Nuspec groupList

groupList :: [Group]
groupList = [Group [depOne, depTwo], Group [depThree]]

depOne :: NuGetDependency
depOne = NuGetDependency "one" "1.0.0"

depTwo :: NuGetDependency
depTwo = NuGetDependency "two" "2.0.0"

depThree :: NuGetDependency
depThree = NuGetDependency "three" "3.0.0"

spec_analyze :: Spec
spec_analyze = do
  nuspecFile <- runIO (TIO.readFile "test/NuGet/testdata/test.nuspec")

  describe "nuspec analyzer" $ do
    it "reads a file and constructs an accurate graph" $ do
      case parseXML nuspecFile of
        Right project -> (groups project) `shouldContain` groupList
        Left err -> expectationFailure (T.unpack ("could not parse nuspec file: " <> xmlErrorPretty err))

    it "constructs an accurate graph" $ do
          let graph = analyze & runInputConst nuspec & run
          expectDeps [dependencyOne, dependencyTwo, dependencyThree] graph
          expectDirect [dependencyOne, dependencyTwo, dependencyThree] graph
          expectEdges [] graph
