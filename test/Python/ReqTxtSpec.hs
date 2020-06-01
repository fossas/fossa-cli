{-# language QuasiQuotes #-}

module Python.ReqTxtSpec
  ( spec
  ) where

import Prologue

import qualified Data.Map.Strict as M
import Text.URI.QQ (uri)

import DepTypes
import Effect.Grapher
import Graphing (Graphing)
import Strategy.Python.Util

import Test.Hspec

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
                      , dependencyEnvironments = []
                      , dependencyTags = M.empty
                      }
  direct $ Dependency { dependencyType = PipType
                      , dependencyName = "pkgTwo"
                      , dependencyVersion = Nothing
                      , dependencyLocations = []
                      , dependencyEnvironments = []
                      , dependencyTags = M.empty
                      }
  direct $ Dependency { dependencyType = PipType
                      , dependencyName = "pkgThree"
                      , dependencyVersion = Just (CURI "https://example.com/")
                      , dependencyLocations = []
                      , dependencyEnvironments = []
                      , dependencyTags = M.empty
                      }

spec :: Spec
spec =
  describe "analyze" $
    it "should produce expected output" $ do
      let result = buildGraph setupPyInput

      result `shouldBe` expected
