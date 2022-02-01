{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Go.GoListSpec (
  spec,
) where

import Control.Algebra
import Control.Carrier.Diagnostics
import Control.Carrier.Simple
import Control.Carrier.Stack (runStack)
import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import DepTypes
import Effect.Exec
import Effect.Grapher
import Graphing (Graphing)
import Path.IO (getCurrentDir)
import ResultUtil
import Strategy.Go.GoList
import Test.Hspec

type ConstExecC = SimpleC ExecF

runConstExec :: Applicative m => BL.ByteString -> ConstExecC m a -> m a
runConstExec output = interpret $ \case
  Exec _ _ -> pure (Right output)

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

spec :: Spec
spec = do
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
