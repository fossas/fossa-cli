{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Go.GoListSpec (
  spec,
) where

import Control.Algebra
import Control.Carrier.Diagnostics
import Control.Carrier.Simple
import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import DepTypes
import Effect.Exec
import Effect.Grapher
import Graphing (Graphing)
import Graphing qualified
import Path.IO (getCurrentDir)
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
      , dependencyName = "github.com/pkg/one"
      , dependencyVersion = Just (CEq "commithash")
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }
  direct $
    Dependency
      { dependencyType = GoType
      , dependencyName = "github.com/pkg/two"
      , dependencyVersion = Just (CEq "v2.0.0")
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }

spec :: Spec
spec = do
  outputTrivial <- runIO (BL.readFile "test/Go/testdata/golist-stdout.trivial")
  outputComplex <- runIO (BL.readFile "test/Go/testdata/golist-stdout.complex")
  testdir <- runIO getCurrentDir

  describe "golist analyze" $ do
    it "produces the expected output" $ do
      let result =
            analyze' testdir
              & runConstExec outputTrivial
              & runDiagnostics
              & run
      case result of
        Left err -> expectationFailure ("analyze failed: " <> show (renderFailureBundle err))
        Right (graph, _) -> graph `shouldBe` expected

    it "can handle complex inputs" $ do
      let result =
            analyze' testdir
              & runConstExec outputComplex
              & runDiagnostics
              & run

      case result of
        Left err -> fail $ "failed to build graph" <> show (renderFailureBundle err)
        Right (graph, _) -> length (Graphing.directList graph) `shouldBe` 12
