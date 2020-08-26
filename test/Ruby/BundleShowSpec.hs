module Ruby.BundleShowSpec
  ( spec
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Text.IO as TIO
import Text.Megaparsec

import DepTypes
import Effect.Grapher
import Graphing (Graphing)
import Strategy.Ruby.BundleShow

import Test.Hspec

expected :: Graphing Dependency
expected = run . evalGrapher $ do
  direct $ Dependency { dependencyType = GemType
                      , dependencyName = "pkgOne"
                      , dependencyVersion = Just (CEq "1.0.0")
                      , dependencyLocations = []
                      , dependencyEnvironments = []
                      , dependencyTags = M.empty
                      }
  direct $ Dependency { dependencyType = GemType
                      , dependencyName = "pkgTwo"
                      , dependencyVersion = Just (CEq "2.0.0")
                      , dependencyLocations = []
                      , dependencyEnvironments = []
                      , dependencyTags = M.empty
                      }

bundleShowOutput :: [BundleShowDep]
bundleShowOutput =
  [ BundleShowDep
    { depName = "pkgOne"
    , depVersion = "1.0.0"
    }
  , BundleShowDep
    { depName = "pkgTwo"
    , depVersion = "2.0.0"
    }
  ]

spec :: Spec
spec = do
  contents <- runIO (TIO.readFile "test/Ruby/testdata/bundleShow")
  complexShow <- runIO (TIO.readFile "test/Ruby/testdata/bundleShowComplex")
  emptyShow <- runIO (TIO.readFile "test/Ruby/testdata/bundleShowEmpty")

  describe "bundle show analyzer" $
    it "produces the expected output" $ do
      let result = buildGraph bundleShowOutput
      result `shouldBe` expected

  describe "bundle show parser" $ do
    it "parses ideal bundle show output" $ do
      case runParser bundleShowParser "" contents of
        Left _ -> expectationFailure "failed to parse"
        Right result -> result `shouldMatchList` bundleShowOutput

    it "parses complex bundle show output" $ do
      case runParser bundleShowParser "" complexShow of
        Left _ -> expectationFailure "failed to parse"
        Right result -> length result `shouldBe` 130

    it "parses error messages into an empty list" $ do
      case runParser bundleShowParser "" emptyShow of
        Left _ -> expectationFailure "failed to parse"
        Right result -> result `shouldMatchList` []
