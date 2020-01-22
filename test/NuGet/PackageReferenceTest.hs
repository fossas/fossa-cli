{-# language TemplateHaskell #-}

module NuGet.PackageReferenceTest
  ( spec_analyze
  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as BL
import qualified Text.XML.Light as XML
import DepTypes
import GraphUtil
import Strategy.NuGet.PackageReference
import Polysemy
import Polysemy.Input
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
                            , dependencyVersion = Nothing
                            , dependencyLocations = []
                            , dependencyTags = M.empty
                            }

itemGroupList :: [ItemGroup]
itemGroupList = [ItemGroup [refOne, refTwo], ItemGroup [refThree, refFour]]

refOne :: PackageReference
refOne = PackageReference "one" $ Just "1.0.0"

refTwo :: PackageReference
refTwo = PackageReference "two" $ Just "2.0.0"

refThree :: PackageReference
refThree = PackageReference "three" $ Just "3.0.0"

refFour :: PackageReference
refFour = PackageReference "four" Nothing

spec_analyze :: Spec
spec_analyze = do
  refFile <- runIO (BL.readFile "test/NuGet/testdata/test.csproj")

  describe "Package Reference parser" $ do
    it "reads a file and constructs an accurate list of item groups" $ do
      case parsePackageReference =<< XML.parseXMLDoc refFile of
        Just groups -> groups `shouldContain` itemGroupList
        Nothing -> expectationFailure "could not parse package reference file"

    it "constructs an accurate graph" $ do
          let graph = analyze & runInputConst @[ItemGroup] itemGroupList & run
          expectDeps [dependencyOne, dependencyTwo, dependencyThree, dependencyFour] graph
          expectDirect [dependencyOne, dependencyTwo, dependencyThree, dependencyFour] graph
          expectEdges [] graph