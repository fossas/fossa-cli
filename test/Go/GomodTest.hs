module Go.GomodTest
  ( spec_gomodBuildGraph
  , spec_gomodParse
  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.Text.IO as TIO
import           Polysemy
import           Text.Megaparsec

import DepTypes
import Effect.Grapher
import Graphing (Graphing)
import Strategy.Go.Gomod
import Strategy.Go.Types (graphingGolang)

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

expected :: Graphing Dependency
expected = run . evalGrapher $ do
  direct $ Dependency
             { dependencyType = GoType
             , dependencyName = "github.com/pkg/one"
             , dependencyVersion = Just (CEq "v1.0.0")
             , dependencyLocations = []
             , dependencyTags = M.empty
             }
  direct $ Dependency
             { dependencyType = GoType
             , dependencyName = "github.com/pkg/overridden"
             , dependencyVersion = Just (CEq "overridden")
             , dependencyLocations = []
             , dependencyTags = M.empty
             }
  direct $ Dependency
             { dependencyType = GoType
             , dependencyName = "github.com/pkg/three/v3"
             , dependencyVersion = Just (CEq "v3.0.0")
             , dependencyLocations = []
             , dependencyTags = M.empty
             }

spec_gomodBuildGraph :: Spec
spec_gomodBuildGraph =
  describe "buildGraph" $
    it "should produce expected output" $ do
      let result = buildGraph gomod & graphingGolang & run

      result `shouldBe` expected

spec_gomodParse :: Spec
spec_gomodParse = do
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
