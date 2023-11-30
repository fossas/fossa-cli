{-# LANGUAGE QuasiQuotes #-}

module Python.SetupPySpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Text.URI.QQ (uri)

import DepTypes (DepType (PipType), Dependency (..), VerConstraint (CAnd, CEq, CGreaterOrEq, CLess, CURI))
import Effect.Grapher (deep, direct, edge, evalGrapher, run)
import Graphing (Graphing)
import Strategy.Python.Util (Operator (OpEq, OpGtEq, OpLt), Req (..), Version (Version), buildGraphSetupFile)

import Data.Text (Text)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)

import Control.Monad (void)
import Data.Void (Void)
import Strategy.Python.Pip (PythonPackage (..))
import Strategy.Python.SetupPy (installRequiresParser, installRequiresParserSetupCfg)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (Parsec, parse)

newtype ExpectedDependency = ExpectedDependency (Dependency, [ExpectedDependency])

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

setupCfgInput :: [Req]
setupCfgInput =
  [ NameReq "pkgFour" Nothing (Just [Version OpEq "1"]) Nothing
  , NameReq "pkgFive" Nothing Nothing Nothing
  ]

traverseDeps :: [ExpectedDependency] -> Graphing Dependency
traverseDeps deps = run . evalGrapher $ do
  traverse
    ( \(ExpectedDependency (dep, deepDeps)) -> do
        direct dep
        traverseDeepDeps dep deepDeps
    )
    deps
  where
    traverseDeepDeps parent children = do
      traverse addDeps children
      where
        addDeps (ExpectedDependency (child, deeperDeps)) = do
          deep child
          edge parent child
          void $ traverseDeepDeps child deeperDeps

expected :: Graphing Dependency
expected = traverseDeps setupPyExpectedDeps

setupPyExpectedDeps :: [ExpectedDependency]
setupPyExpectedDeps =
  [ ExpectedDependency
      ( Dependency
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
      , []
      )
  , ExpectedDependency
      ( Dependency
          { dependencyType = PipType
          , dependencyName = "pkgTwo"
          , dependencyVersion = Nothing
          , dependencyLocations = []
          , dependencyEnvironments = mempty
          , dependencyTags = Map.empty
          }
      , []
      )
  , ExpectedDependency
      ( Dependency
          { dependencyType = PipType
          , dependencyName = "pkgThree"
          , dependencyVersion = Just (CURI "https://example.com")
          , dependencyLocations = []
          , dependencyEnvironments = mempty
          , dependencyTags = Map.empty
          }
      , []
      )
  ]

setupCfgExpectedDeps :: [ExpectedDependency]
setupCfgExpectedDeps =
  [ ExpectedDependency
      ( Dependency
          { dependencyType = PipType
          , dependencyName = "pkgFour"
          , dependencyVersion = Just (CEq "1")
          , dependencyLocations = []
          , dependencyEnvironments = mempty
          , dependencyTags = Map.empty
          }
      , []
      )
  , ExpectedDependency
      ( Dependency
          { dependencyType = PipType
          , dependencyName = "pkgFive"
          , dependencyVersion = Nothing
          , dependencyLocations = []
          , dependencyEnvironments = mempty
          , dependencyTags = Map.empty
          }
      , []
      )
  ]

setupPyBarDeps :: [ExpectedDependency]
setupPyBarDeps =
  [ ExpectedDependency
      ( Dependency
          { dependencyType = PipType
          , dependencyName = "pkgChild"
          , dependencyVersion = Just (CEq "1")
          , dependencyLocations = []
          , dependencyEnvironments = mempty
          , dependencyTags = Map.empty
          }
      ,
        [ ExpectedDependency
            ( Dependency
                { dependencyType = PipType
                , dependencyName = "pkgChildTwo"
                , dependencyVersion = Just (CEq "1")
                , dependencyLocations = []
                , dependencyEnvironments = mempty
                , dependencyTags = Map.empty
                }
            , []
            )
        ]
      )
  ]
expectedCombined :: Graphing Dependency
expectedCombined = traverseDeps (setupCfgExpectedDeps ++ setupPyExpectedDeps)

expectedBar :: Graphing Dependency
expectedBar = traverseDeps setupPyBarDeps

spec :: Spec
spec = do
  setupCfgSpec
  describe "parse" $ do
    it "should parse setup.py without comments" $ do
      let shouldParseInto = parseMatch installRequiresParser
      setupPyWithoutComment `shouldParseInto` ([mkReq "PyYAML", mkReq "pandas", mkReq "numpy"], Just "foo")
      setupPyWithoutComment2 `shouldParseInto` ([mkReq "PyYAML", mkReq "pandas", mkReq "numpy"], Nothing)

    it "should parse setup.py with backslash" $ do
      let shouldParseInto = parseMatch installRequiresParser
      setupPyWithBackslash `shouldParseInto` ([mkReq "PyYAML", mkReq "pandas", mkReq "numpy"], Nothing)

    it "should parse setup.py with comments" $ do
      let shouldParseInto = parseMatch installRequiresParser

      setupPyWithCommentAfterComma `shouldParseInto` ([mkReq "PyYAML", mkReq "pandas", mkReq "numpy"], Nothing)
      setupPyWithCommentBeforeReq `shouldParseInto` ([mkReq "PyYAML", mkReq "numpy"], Nothing)
      setupPyWithAllComments `shouldParseInto` ([], Just "foo")

  describe "analyze" $ do
    it "should produce expected output" $ do
      let result = buildGraphSetupFile Nothing Nothing setupPyInput Nothing []
      result `shouldBe` expected

    it "should combine setup.py and setup.cfg" $ do
      let result = buildGraphSetupFile Nothing Nothing setupPyInput Nothing setupCfgInput
      result `shouldBe` expectedCombined

    it "should default to install_requires when the setupPy package name is not found" $ do
      let result = buildGraphSetupFile (Just [(PythonPackage "non-bar" "1" [])]) (Just "bar") setupPyInput Nothing []
      result `shouldBe` expected

    it "should only report found package dependencies when the setupPy package name is found" $ do
      let result = buildGraphSetupFile (Just [(PythonPackage "bar" "1" [(PythonPackage "pkgChild" "1" [(PythonPackage "pkgChildTwo" "1" [])])])]) (Just "bar") setupPyInput Nothing []
      result `shouldBe` expectedBar

setupCfgSpec :: Spec
setupCfgSpec =
  describe "setup.cfg parser" $ do
    it "should parse setup.cfg when it has no install_requires" $ do
      let shouldParseInto = parseMatch installRequiresParserSetupCfg
      setupCgfWithoutInstallReqs `shouldParseInto` ([], Just "vnpy")

    it "should parse setup.cfg" $ do
      let shouldParseInto = parseMatch installRequiresParserSetupCfg
      setupCgfSimple `shouldParseInto` ([mkReq "PyYAML", mkReq "pandas"], Just "foo")
      setupCgfSimple2 `shouldParseInto` ([mkReq "PyYAML", mkReq "pandas"], Nothing)
      setupCgfSimple3 `shouldParseInto` ([mkReq "PyYAML", mkReq "pandas"], Nothing)
      setupCgfSimpleComment `shouldParseInto` ([mkReq "PyYAML", mkReq "pandas"], Nothing)

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
name = foo
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

setupPyWithoutComment :: Text
setupPyWithoutComment =
  [r|from setuptools import setup, find_packages
setup(
    name='foo',
    install_requires=[
        'PyYAML',
        'pandas',
        'numpy'
    ],
)
|]

setupPyWithoutComment2 :: Text
setupPyWithoutComment2 =
  [r|from setuptools import setup, find_packages
setup(
    install_requires=[
        'PyYAML',
        'pandas',
        'numpy',
    ],
)
|]

setupPyWithCommentAfterComma :: Text
setupPyWithCommentAfterComma =
  [r|from setuptools import setup, find_packages
setup(
    install_requires=[
        'PyYAML', # should not fail
        'pandas',
        'numpy'
    ],
)
|]

setupPyWithCommentBeforeReq :: Text
setupPyWithCommentBeforeReq =
  [r|from setuptools import setup, find_packages
setup(
    install_requires=[
        'PyYAML', # should not fail
        # 'pandas==0.23.3',
        'numpy'
    ],
)
|]

setupPyWithAllComments :: Text
setupPyWithAllComments =
  [r|from setuptools import setup, find_packages
setup(
    name='foo',
    install_requires=[
        # 'PyYAML',
        # 'pandas==0.23.3',
        # 'numpy>=1.14.5'
    ],
)
|]

setupPyWithBackslash :: Text
setupPyWithBackslash =
  [r|from setuptools import setup, find_packages
setup(
    install_requires=[ 'PyYAML', \
      'pandas', \
      'numpy'
    ],
)
|]
