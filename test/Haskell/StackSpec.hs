{-# LANGUAGE RecordWildCards #-}

module Haskell.StackSpec (
  spec,
) where

import Control.Carrier.Diagnostics
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import GraphUtil
import Graphing qualified as G
import Strategy.Haskell.Stack
import Test.Hspec qualified as Test
import Types
import Prelude

allDeps :: [StackDep]
allDeps = [builtinDep, deepDep, localDep, remoteDep]

mkDep :: Text -> [Text] -> StackLocation -> StackDep
mkDep name deps = StackDep (PackageName name) (name <> "-ver") (map PackageName deps)

builtinDep :: StackDep
builtinDep = mkDep "builtin" [] BuiltIn
deepDep :: StackDep
deepDep = mkDep "deep" [] Remote
localDep :: StackDep
localDep = mkDep "local" ["remote", "builtin"] Local
remoteDep :: StackDep
remoteDep = mkDep "remote" ["deep"] Remote

spec :: Test.Spec
spec = do
  Test.describe "Stack json deps parser" $ do
    jsonBytes <- Test.runIO $ BL.readFile "test/Haskell/testdata/stack.json"
    Test.it "should parse a json dependencies file" $
      case eitherDecode jsonBytes of
        Left err -> Test.expectationFailure $ "Failed to parse: " ++ err
        Right deps -> deps `Test.shouldMatchList` allDeps

  Test.describe "Stack graph builder" $
    case run . runDiagnostics . buildGraph $ allDeps of
      Left fbundle -> Test.it "should build a graph" $ Test.expectationFailure (show $ renderFailureBundle fbundle)
      Right graph -> do
        let gr = G.gmap dependencyName graph
        Test.it "should have the correct deps" $ expectDeps ["deep", "remote"] gr
        Test.it "should have the correct direct deps" $ expectDirect ["remote"] gr
        Test.it "should have the correct edges" $ expectEdges [("remote", "deep")] gr
