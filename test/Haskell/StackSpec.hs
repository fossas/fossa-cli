module Haskell.StackSpec (
  spec,
) where

import Control.Carrier.Diagnostics
import Control.Carrier.Stack (runStack)
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import GraphUtil
import Graphing qualified as G
import ResultUtil
import Strategy.Haskell.Stack
import Test.Hspec qualified as Test
import Types
import Prelude

baseDeps :: [StackDep]
baseDeps = [builtinDep, deepDep, localDep, remoteDep]

mkDep :: Text -> [Text] -> StackLocation -> StackDep
mkDep name deps = StackDep (PackageName name) (name <> "-ver") (map PackageName deps)

builtinDep :: StackDep
builtinDep = mkDep "builtin" [] BuiltIn
deepDep :: StackDep
deepDep = mkDep "deep" [] Remote
localDep :: StackDep
localDep = mkDep "local" ["remote", "builtin"] Local
remoteDep :: StackDep
remoteDep = mkDep "remote" ["deep", "git-pkg"] Remote

gitDepUrl :: Text
gitDepUrl = "https://domain.com/user/git-pkg"

gitDepUncorrectedName :: StackDep
gitDepUncorrectedName = StackDep (PackageName "git-pkg") "abc123" [] (Git (GitUrl gitDepUrl) (GitSha "abc123"))

allDepsParsed :: [StackDep]
allDepsParsed = gitDepUncorrectedName : baseDeps

spec :: Test.Spec
spec = do
  Test.describe "Stack json deps parser" $ do
    jsonBytes <- Test.runIO $ BL.readFile "test/Haskell/testdata/stack.json"
    Test.it "should parse a json dependencies file" $
      case eitherDecode jsonBytes of
        Left err -> Test.expectationFailure $ "Failed to parse: " ++ show err
        Right deps -> deps `Test.shouldMatchList` allDepsParsed

  Test.describe "Stack graph builder" $ do
    let result = run . runStack . runDiagnostics . buildGraph $ allDepsParsed

    Test.it "should build a correct graph" $
      assertOnSuccess result $ \_ graph -> do
        let gr = G.gmap dependencyName graph
        expectDeps ["deep", "remote", gitDepUrl] gr
        expectDirect ["remote"] gr
        expectEdges [("remote", "deep"), ("remote", gitDepUrl)] gr
