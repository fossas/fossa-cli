module Maven.MavenTest
  ( spec_mavenBuildGraph
  ) where

import Prologue

import qualified Data.Map.Strict as M

import DepTypes
import Strategy.Maven
import Strategy.Maven.Plugin

import GraphUtil
import Test.Tasty.Hspec

packageOne :: Dependency
packageOne = Dependency
  { dependencyType = MavenType
  , dependencyName = "mygroup:packageOne"
  , dependencyVersion = Just (CEq "1.0.0")
  , dependencyLocations = []
  , dependencyTags = M.fromList [("scopes", ["compile", "test"])]
  }

packageTwo :: Dependency
packageTwo = Dependency
  { dependencyType = MavenType
  , dependencyName = "mygroup:packageTwo"
  , dependencyVersion = Just (CEq "2.0.0")
  , dependencyLocations = []
  , dependencyTags = M.fromList [("scopes", ["compile"]), ("optional", ["true"])]
  }

mavenOutput :: PluginOutput
mavenOutput = PluginOutput
  { outArtifacts =
    [ Artifact
        { artifactNumericId = 0
        , artifactGroupId = "mygroup"
        , artifactArtifactId = "packageOne"
        , artifactVersion = "1.0.0"
        , artifactOptional = False
        , artifactScopes = ["compile", "test"]
        }
    , Artifact
        { artifactNumericId = 1
        , artifactGroupId = "mygroup"
        , artifactArtifactId = "packageTwo"
        , artifactVersion = "2.0.0"
        , artifactOptional = True
        , artifactScopes = ["compile"]
        }
    ]
  , outEdges =
    [ Edge
        { edgeFrom = 0
        , edgeTo = 1
        }
    ]
  }

spec_mavenBuildGraph :: Spec
spec_mavenBuildGraph = do
  describe "buildGraph" $ do
    it "should produce expected output" $ do
      let graph = buildGraph mavenOutput

      expectDeps [ packageOne, packageTwo ] graph
      expectDirect [] graph
      expectEdges [(packageOne, packageTwo)] graph
