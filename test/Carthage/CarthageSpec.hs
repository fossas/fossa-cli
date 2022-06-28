{-# LANGUAGE TemplateHaskell #-}

module Carthage.CarthageSpec (
  spec,
) where

import Control.Carrier.Diagnostics
import Control.Carrier.Stack (runStack)
import Data.Function ((&))
import Effect.ReadFS
import GraphUtil
import Graphing qualified as G
import Path
import Path.IO (makeAbsolute)
import ResultUtil
import Strategy.Carthage
import Test.Hspec

testProjectEmpty :: Path Rel File
testProjectEmpty = $(mkRelFile "test/Carthage/testdata/testempty/Cartfile.resolved")

testProjectComplex :: Path Rel File
testProjectComplex = $(mkRelFile "test/Carthage/testdata/testproject/Cartfile.resolved")

spec :: Spec
spec = do
  let runIt f =
        analyze f
          & runReadFSIO
          & runDiagnostics
          & runStack

  emptyResult <- runIO $ runIt =<< makeAbsolute testProjectEmpty
  complexResult <- runIO $ runIt =<< makeAbsolute testProjectComplex

  describe "carthage analyze" $ do
    it "should work for empty projects" $ do
      assertOnSuccess emptyResult $ \_ result -> result `shouldBe` G.empty

    it "should work for a complex project" $ do
      assertOnSuccess complexResult $ \_ graph -> do
        expectDirect [nimble713, swinject, ocmock, someRepo, someRepoViaGitScheme, someRepoViaHttpScheme] graph
        expectDeps
          [ nimble713
          , nimble703
          , swinject
          , ocmock
          , quick
          , cwlPreconditionTesting
          , cwlCatchException
          , someRepo
          , someRepoViaGitScheme
          , someRepoViaHttpScheme
          ]
          graph
        expectEdges
          [ (nimble713, cwlPreconditionTesting)
          , (nimble713, cwlCatchException)
          , (swinject, nimble703)
          , (swinject, quick)
          ]
          graph

nimble713 :: ResolvedEntry
nimble713 = ResolvedEntry GithubType "Quick/Nimble" "v7.1.3"

nimble703 :: ResolvedEntry
nimble703 = ResolvedEntry GithubType "Quick/Nimble" "v7.0.3"

quick :: ResolvedEntry
quick = ResolvedEntry GithubType "Quick/Quick" "v1.2.0"

cwlPreconditionTesting :: ResolvedEntry
cwlPreconditionTesting = ResolvedEntry GithubType "mattgallagher/CwlPreconditionTesting" "1e62a726d54c743f4585233f08fcaac7307319b5"

cwlCatchException :: ResolvedEntry
cwlCatchException = ResolvedEntry GithubType "mattgallagher/CwlCatchException" "b14c111e9b33cd142bd4bc75c482cfd5c3490923"

swinject :: ResolvedEntry
swinject = ResolvedEntry GithubType "Swinject/Swinject" "2.4.1"

ocmock :: ResolvedEntry
ocmock = ResolvedEntry GithubType "erikdoe/ocmock" "v3.4.2"

someRepo :: ResolvedEntry
someRepo = ResolvedEntry GithubType "https://github.example.com/someOwner/someRepo" "0.0.1"

someRepoViaGitScheme :: ResolvedEntry
someRepoViaGitScheme = ResolvedEntry GithubType "git://github.example.com/someOwner/someRepo" "0.0.1"

someRepoViaHttpScheme :: ResolvedEntry
someRepoViaHttpScheme = ResolvedEntry GithubType "http://github.example.com/someOwner/someRepo" "0.0.1"
