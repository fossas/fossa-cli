{-# language QuasiQuotes #-}

module Python.ReqTxtTest
  ( spec_analyze
  ) where

import Prologue

import qualified Data.Map.Strict as M
import           Polysemy
import           Polysemy.Input
import           Text.URI.QQ (uri)

import DepTypes
import Effect.Grapher
import Graphing (Graphing)
import Strategy.Python.ReqTxt
import Strategy.Python.Util

import Test.Tasty.Hspec

setupPyInput :: [Req]
setupPyInput =
  [ NameReq "pkgOne" Nothing (Just [ Version OpGtEq "1.0.0"
                                   , Version OpLt   "2.0.0"
                                   ]) Nothing
  , NameReq "pkgTwo" Nothing Nothing Nothing
  , UrlReq "pkgThree" Nothing [uri|https://example.com/|] Nothing
  ]

expected :: Graphing Dependency
expected = run . evalGrapher $ do
  direct $ Dependency { dependencyType = PipType
                      , dependencyName = "pkgOne"
                      , dependencyVersion =
                          Just (CAnd (CGreaterOrEq "1.0.0")
                                     (CLess "2.0.0"))
                      , dependencyLocations = []
                      , dependencyTags = M.empty
                      }
  direct $ Dependency { dependencyType = PipType
                      , dependencyName = "pkgTwo"
                      , dependencyVersion = Nothing
                      , dependencyLocations = []
                      , dependencyTags = M.empty
                      }
  direct $ Dependency { dependencyType = PipType
                      , dependencyName = "pkgThree"
                      , dependencyVersion = Just (CURI "https://example.com/")
                      , dependencyLocations = []
                      , dependencyTags = M.empty
                      }

spec_analyze :: Spec
spec_analyze =
  describe "analyze" $
    it "should produce expected output" $ do
      let result = analyze
            & runInputConst @[Req] setupPyInput
            & run

      result `shouldBe` expected
