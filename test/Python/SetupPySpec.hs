{-# LANGUAGE QuasiQuotes #-}

module Python.SetupPySpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Text.URI.QQ (uri)

import DepTypes (DepType (PipType), Dependency (..), VerConstraint (CAnd, CGreaterOrEq, CLess, CURI))
import Effect.Grapher (direct, evalGrapher, run)
import Graphing (Graphing)
import Strategy.Python.Util (Operator (OpGtEq, OpLt), Req (..), Version (Version), buildGraph)

import Data.Text (Text)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)

import Data.Void (Void)
import Strategy.Python.SetupPy (installRequiresParserSetupCfg)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (Parsec, parse)

parseMatch :: (Show a, Eq a) => Parsec Void Text a -> Text -> a -> Expectation
parseMatch parser input parseExpected = parse parser "" input `shouldParse` parseExpected

setupPyInput :: [Req]
setupPyInput =
  [ NameReq
      "pkgOne"
      Nothing
      ( Just
          [ Version OpGtEq "1.0.0"
          , Version OpLt "2.0.0"
          ]
      )
      Nothing
  , NameReq "pkgTwo" Nothing Nothing Nothing
  , UrlReq "pkgThree" Nothing [uri|https://example.com|] Nothing
  ]

expected :: Graphing Dependency
expected = run . evalGrapher $ do
  direct $
    Dependency
      { dependencyType = PipType
      , dependencyName = "pkgOne"
      , dependencyVersion =
          Just
            ( CAnd
                (CGreaterOrEq "1.0.0")
                (CLess "2.0.0")
            )
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }
  direct $
    Dependency
      { dependencyType = PipType
      , dependencyName = "pkgTwo"
      , dependencyVersion = Nothing
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }
  direct $
    Dependency
      { dependencyType = PipType
      , dependencyName = "pkgThree"
      , dependencyVersion = Just (CURI "https://example.com")
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }

spec :: Spec
spec = do
  setupCfgSpec
  describe "analyze" $
    it "should produce expected output" $ do
      let result = buildGraph setupPyInput

      result `shouldBe` expected

setupCfgSpec :: Spec
setupCfgSpec =
  describe "setup.cfg parser" $ do
    it "should parse setup.cfg when it has no install_requires" $ do
      let shouldParseInto = parseMatch installRequiresParserSetupCfg
      setupCgfWithoutInstallReqs `shouldParseInto` []

    it "should parse setup.cfg" $ do
      let shouldParseInto = parseMatch installRequiresParserSetupCfg
      setupCgfSimple `shouldParseInto` [mkReq "PyYAML", mkReq "pandas"]
      setupCgfSimple2 `shouldParseInto` [mkReq "PyYAML", mkReq "pandas"]
      setupCgfSimple3 `shouldParseInto` [mkReq "PyYAML", mkReq "pandas"]
      setupCgfSimpleComment `shouldParseInto` [mkReq "PyYAML", mkReq "pandas"]

mkReq :: Text -> Req
mkReq name = NameReq name Nothing Nothing Nothing

setupCgfWithoutInstallReqs :: Text
setupCgfWithoutInstallReqs =
  [r|[metadata]
name = vnpy
version = attr: vnpy.__version__
author = hey
|]

setupCgfSimple :: Text
setupCgfSimple =
  [r|[options]
packages = find:
include_package_data = True
zip_safe = False
install_requires =
    PyYAML
    pandas

[options.package_data]
|]

setupCgfSimple2 :: Text
setupCgfSimple2 =
  [r|[options]
packages = find:
include_package_data = True
zip_safe = False
install_requires =
    PyYAML
    pandas

something
|]

setupCgfSimple3 :: Text
setupCgfSimple3 =
  [r|[options]
packages = find:
include_package_data = True
zip_safe = False
install_requires =
    PyYAML
    pandas
something
|]

setupCgfSimpleComment :: Text
setupCgfSimpleComment =
  [r|[options]
packages = find:
include_package_data = True
zip_safe = False
install_requires =
    PyYAML # some comment
    # weird
    # numpy
    pandas
|]
