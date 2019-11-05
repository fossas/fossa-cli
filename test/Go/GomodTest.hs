module Go.GomodTest
  ( spec_analyze
  , spec_parse
  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.Text.IO as TIO
import           Polysemy
import           Polysemy.Input

import           Effect.GraphBuilder
import qualified Graph as G
import           Strategy.Go.Gomod
import           Strategy.Python.Util
import           Text.Megaparsec

import Test.Hspec.Megaparsec
import Test.Tasty.Hspec

gomod :: Gomod
gomod = Gomod
  { modName = "github.com/my/package"
  , modRequires = [ Require "github.com/pkg/one" "v1.0.0"
                  , Require "github.com/pkg/two/v2" "v2.0.0"
                  , Require "github.com/pkg/three/v3" "v3.0.0"
                  ]
  , modReplaces = M.fromList [("github.com/pkg/two/v2", Require "github.com/pkg/overridden" "overridden")]
  , modExcludes = []
  }

expected :: G.Graph
expected = run . evalGraphBuilder G.empty $ do
  ref1 <- addNode (G.Dependency
                        { dependencyType = G.GoType
                        , dependencyName = "github.com/pkg/one"
                        , dependencyVersion = Just (G.CEq "v1.0.0")
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        })
  ref2 <- addNode (G.Dependency
                        { dependencyType = G.GoType
                        , dependencyName = "github.com/pkg/overridden"
                        , dependencyVersion = Just (G.CEq "overridden")
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        })
  ref3 <- addNode (G.Dependency
                        { dependencyType = G.GoType
                        , dependencyName = "github.com/pkg/three/v3"
                        , dependencyVersion = Just (G.CEq "v3.0.0")
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        })
  addDirect ref1
  addDirect ref2
  addDirect ref3

spec_analyze :: Spec
spec_analyze =
  describe "analyze" $
    it "should produce expected output" $ do
      let result = analyze
            & runInputConst @Gomod gomod
            & run

      result `shouldBe` expected

spec_parse :: Spec
spec_parse = do
  trivialInput <- runIO (TIO.readFile "test/Go/testdata/go.mod.trivial")
  edgecaseInput <- runIO (TIO.readFile "test/Go/testdata/go.mod.edgecases")

  describe "gomod parser" $ do
    it "should parse a trivial example" $ do
      runParser gomodParser "" trivialInput `shouldParse` gomod

    it "should parse each edge case" $ do
      runParser gomodParser "" edgecaseInput
        `shouldParse` Gomod
                        { modName = "test/package"
                        , modRequires = [ Require "repo/name/A" "v1.0.0"
                                        , Require "repo/B" "v0.0.5-0.20190714195934-000000000002"
                                        , Require "repo/C" "v1.1.0"
                                        , Require "repo/name/D" "v4.0.0"
                                        , Require "repo/E" "v8.0.0+incompatible"
                                        ]
                        , modReplaces = M.fromList
                            [ ("repo/B", Require "alias/repo/B" "v0.1.0")
                            , ("repo/C", Require "alias/repo/C" "v0.0.0-20180207000608-000000000003")
                            , ("repo/E", Require "alias/repo/E" "v0.0.0-20170808103936-000000000005+incompatible")
                            ]
                        , modExcludes = [ Require "repo/B" "v0.9.0"
                                        , Require "repo/C" "v1.0.0"
                                        , Require "repo/name/D" "v3.0.0"
                                        ]
                        }
