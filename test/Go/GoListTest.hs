{-# language TemplateHaskell #-}

module Go.GoListTest
  ( spec_analyze
  ) where

import Prologue

import qualified Data.ByteString.Lazy as BL
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import           Polysemy
import           Polysemy.Error

import           Diagnostics
import           Effect.Exec
import           Effect.GraphBuilder
import qualified Graph as G
import           Strategy.Go.GoList
import           Types (BasicDirOpts(..))

import Test.Tasty.Hspec

mockExec :: BL.ByteString -> Sem (Exec ': r) a -> Sem r a
mockExec stdout = interpret $ \case
  Exec _ _ _ -> pure (Right stdout)

expected :: G.Graph
expected = run . evalGraphBuilder G.empty $ do
  ref1 <- addNode (G.Dependency { dependencyType = G.GoType
                        , dependencyName = "github.com/pkg/one"
                        , dependencyVersion = Just (G.CEq "commithash")
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        })
  ref2 <- addNode (G.Dependency { dependencyType = G.GoType
                        , dependencyName = "github.com/pkg/two"
                        , dependencyVersion = Just (G.CEq "v2.0.0")
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        })
  addDirect ref1
  addDirect ref2

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
              & runError @CLIErr
              & run
      case result of
        Left err -> expectationFailure ("analyze failed: " <> show err)
        Right graph -> graph `shouldBe` expected

    it "can handle complex inputs" $ do
      let result =
            analyze (BasicDirOpts testdir)
              & mockExec outputComplex
              & runError @CLIErr
              & run

      case result of
          Left err -> fail $ "failed to build graph" <> show err
          Right graph -> do
              IS.size (G.graphDirect graph) `shouldBe` 12
