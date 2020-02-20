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
nuspec = Nuspec groupList licenses (Just "test.com")

licenses :: [NuspecLicense]
licenses = [NuspecLicense "expression" "", NuspecLicense "" "BSD-2-Clause", NuspecLicense "expression" "  ", NuspecLicense "file" "", NuspecLicense "file" "doesnt-exist.txt", NuspecLicense "expression" "Foo"]

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
  dependenciesAndLicense <- runIO (TIO.readFile "test/NuGet/testdata/nuspec/test.nuspec")
  singleLicense <- runIO (TIO.readFile "test/NuGet/testdata/nuspec/license.nuspec")
  multipleLicenses <- runIO (TIO.readFile "test/NuGet/testdata/nuspec/multiple-licenses.nuspec")

  describe "nuspec analyzer" $ do
    it "reads a file and constructs an accurate graph" $ do
      case parseXML dependenciesAndLicense of
        Right project -> do
          (groups project) `shouldContain` groupList
          (license project) `shouldMatchList` [NuspecLicense "file" "license-file"]
          (licenseUrl project) `shouldBe` (Just "https://licence.location.com/LICENSE.md")
        Left err -> expectationFailure (T.unpack ("could not parse nuspec file: " <> xmlErrorPretty err))

    it "reads a file and extracts the correct license" $ do
      case parseXML singleLicense of
        Right project -> do
          (groups project) `shouldBe` [] 
          (license project) `shouldMatchList` [NuspecLicense "file" "LICENSE.txt"]
          (licenseUrl project) `shouldBe` Nothing
        Left err -> expectationFailure (T.unpack ("could not parse nuspec file: " <> xmlErrorPretty err))

    it "reads a file with multiple licenses" $ do
      case parseXML multipleLicenses of
        Right project -> do
          (groups project) `shouldBe` []
          (license project) `shouldMatchList` licenses
          (licenseUrl project) `shouldBe` (Just "test.com")
        Left err -> expectationFailure (T.unpack ("could not parse nuspec file: " <> xmlErrorPretty err))

    it "constructs an accurate graph" $ do
          let graph = buildGraph nuspec
          expectDeps [dependencyOne, dependencyTwo, dependencyThree] graph
          expectDirect [dependencyOne, dependencyTwo, dependencyThree] graph
          expectEdges [] graph
