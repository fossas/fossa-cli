{-# LANGUAGE RecordWildCards #-}

module Haskell.StackSpec
  ( spec,
  )
where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Graphing as G
import qualified Test.Hspec as Test
import Prelude
import Strategy.Haskell.Stack
import Control.Carrier.Diagnostics
import Types
import GraphUtil

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
