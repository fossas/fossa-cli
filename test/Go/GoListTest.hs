{-# language TemplateHaskell #-}

module Go.GoListTest
  ( spec_analyze
  ) where

import Prologue

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import           Polysemy
import           Polysemy.Error

import DepTypes
import Diagnostics
import Effect.Exec
import Effect.Grapher
import Graphing (Graphing(..))
import Strategy.Go.GoList
import Types (BasicDirOpts(..))

import Test.Tasty.Hspec

mockExec :: BL.ByteString -> Sem (Exec ': r) a -> Sem r a
mockExec stdout = interpret $ \case
  Exec _ _ _ -> pure (Right stdout)

expected :: Graphing Dependency
expected = run . evalGrapher $ do
  direct $ Dependency { dependencyType = GoType
                      , dependencyName = "github.com/pkg/one"
                      , dependencyVersion = Just (CEq "commithash")
                      , dependencyLocations = []
                      , dependencyTags = M.empty
                      }
  direct $ Dependency { dependencyType = GoType
                      , dependencyName = "github.com/pkg/two"
                      , dependencyVersion = Just (CEq "v2.0.0")
                      , dependencyLocations = []
                      , dependencyTags = M.empty
                      }

testdir :: Path Rel Dir
testdir = $(mkRelDir ".")

spec_analyze :: Spec
spec_analyze = do
  outputTrivial <- runIO (BL.readFile "test/Go/testdata/golist-stdout.trivial")
  outputComplex <- runIO (BL.readFile "test/Go/testdata/golist-stdout.complex")

  describe "golist analyze" $ do
    it "produces the expected output" $ do
      let result =
            analyze (BasicDirOpts testdir)
              & mockExec outputTrivial
              & runError @ExecErr
              & run
      case result of
        Left err -> expectationFailure ("analyze failed: " <> show err)
        Right graph -> graph `shouldBe` expected

    it "can handle complex inputs" $ do
      let result =
            analyze (BasicDirOpts testdir)
              & mockExec outputComplex
              & runError @ExecErr
              & run

      case result of
          Left err -> fail $ "failed to build graph" <> show err
          Right graph -> length (graphingDirect graph) `shouldBe` 12
