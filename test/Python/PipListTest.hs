{-# language QuasiQuotes #-}

module Python.PipListTest
  ( spec_analyze
  , spec_buildGraph
  ) where

import Prologue
import Test.Tasty.Hspec

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import           Data.String.QQ
import           Polysemy
import           Polysemy.Error

import           Diagnostics
import           Effect.Exec
import           Effect.GraphBuilder
import qualified Graph as G
import           Strategy.Python.PipList
import           Types

expected :: G.Graph
expected = run . evalGraphBuilder G.empty $ do
  ref1 <- addNode (G.Dependency { dependencyType = G.PipType
                        , dependencyName = "pkgOne"
                        , dependencyVersion = Just "1.0.0"
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        })
  ref2 <- addNode (G.Dependency { dependencyType = G.PipType
                        , dependencyName = "pkgTwo"
                        , dependencyVersion = Just "2.0.0"
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        })
  addDirect ref1
  addDirect ref2

pipListOutput :: BL.ByteString
pipListOutput = [s|
  [{"name": "pkgOne", "version": "1.0.0"}, {"name": "pkgTwo", "version": "2.0.0"}]
|]

mockExec :: InterpreterFor Exec r
mockExec = interpret $ \case
  Exec _ "pip3" ("list":_) -> pure (ExitSuccess, pipListOutput, "")
  Exec dir cmd args -> error $ "Unexpected exec: dir:" <> show dir <> ", cmd:" <> show cmd <> ", args:" <> show args

spec_analyze :: Spec
spec_analyze =
  describe "analyze" $ do
    it "produces the expected output" $ do
      let result = analyze (BasicDirOpts [reldir|nulldir|])
            & mockExec
            & runError @CLIErr
            & run
      result `shouldBe` (Right expected)

spec_buildGraph :: Spec
spec_buildGraph =
  describe "buildGraph" $ do
    it "produces the expected output" $ do
      let result =
            buildGraph [ PipListDep "pkgOne" "1.0.0"
                       , PipListDep "pkgTwo" "2.0.0"
                       ]

      result `shouldBe` expected
