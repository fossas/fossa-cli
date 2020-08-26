{-# LANGUAGE TemplateHaskell #-}

module NuGet.ProjectJsonSpec
  ( spec
  ) where

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import DepTypes
import GraphUtil
import Strategy.NuGet.ProjectJson
import Test.Hspec

dependencyOne :: Dependency
dependencyOne = Dependency { dependencyType = NuGetType
                        , dependencyName = "one"
                        , dependencyVersion = Just (CEq "1.0.0")
                        , dependencyLocations = []
                        , dependencyEnvironments = []
                        , dependencyTags = M.empty
                        }

dependencyTwo :: Dependency
dependencyTwo = Dependency { dependencyType = NuGetType
                        , dependencyName = "two"
                        , dependencyVersion = Just (CCompatible "2.*")
                        , dependencyLocations = []
                        , dependencyEnvironments = []
                        , dependencyTags = M.empty
                        }

dependencyThree :: Dependency
dependencyThree = Dependency { dependencyType = NuGetType
                        , dependencyName = "three"
                        , dependencyVersion = Just (CEq "3.0.0")
                        , dependencyLocations = []
                        , dependencyEnvironments = []
                        , dependencyTags = M.fromList [ ("type", ["sometype"]) ]
                        }

spec :: Spec
spec = do
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
