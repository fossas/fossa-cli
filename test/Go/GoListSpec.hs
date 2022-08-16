{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Go.GoListSpec (
  spec,
) where

import Control.Algebra (run)
import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Simple (SimpleC, interpret)
import Control.Carrier.Stack (runStack)
import Data.Aeson (decode)
import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import DepTypes (
  DepType (GoType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.Exec (ExecF (..))
import Effect.Grapher (deep, direct, evalGrapher)
import Graphing (Graphing)
import Path.IO (getCurrentDir)
import ResultUtil (assertOnSuccess)
import Strategy.Go.GoList (
  GoListModule (..),
  GoModuleReplacement (
    GoModuleReplacement,
    pathReplacement,
    versionReplacement
  ),
  analyze',
 )
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Text.RawString.QQ (r)

type ConstExecC = SimpleC ExecF

runConstExec :: Applicative m => BL.ByteString -> ConstExecC m a -> m a
runConstExec output = interpret $ \case
  Exec{} -> pure (Right output)

expected :: Graphing Dependency
expected = run . evalGrapher $ do
  direct $
    Dependency
      { dependencyType = GoType
      , dependencyName = "gopkg.in/yaml.v3"
      , dependencyVersion = Just (CEq "496545a6307b")
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }
  deep $
    Dependency
      { dependencyType = GoType
      , dependencyName = "gopkg.in/check.v1"
      , dependencyVersion = Just (CEq "788fd7840127")
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }

analysisSpec :: Spec
analysisSpec = do
  outputTrivial <- runIO (BL.readFile "test/Go/testdata/golist-stdout")
  testdir <- runIO getCurrentDir

  describe "golist analyze" $ do
    it "produces the expected output" $ do
      let result =
            analyze' testdir
              & runConstExec outputTrivial
              & runDiagnostics
              & runStack
              & run
      assertOnSuccess result $ \_ (graph, _) -> graph `shouldBe` expected

replacedModule :: BL.ByteString
replacedModule =
  [r| {
    "Path": "/foo/bar",
    "Version": "1.2",
    "Replace": {
      "Path": "/foo/bar",
      "Version": "1.6"
      }
    }|]

expectedReplacedModule :: GoListModule
expectedReplacedModule =
  GoListModule
    { path = "/foo/bar"
    , version = Just "1.2"
    , isMain = False
    , isIndirect = False
    , moduleReplacement =
        Just
          GoModuleReplacement
            { pathReplacement = "/foo/bar"
            , versionReplacement = "1.6"
            }
    }

listParseSpec :: Spec
listParseSpec = do
  describe "Parsing json list output" $
    it "Parses replacements" $ do
      case decode replacedModule of
        Just goMod -> goMod `shouldBe` expectedReplacedModule
        Nothing -> fail "Couldn't parse replacedModule"

spec :: Spec
spec =
  do
    analysisSpec
    listParseSpec
