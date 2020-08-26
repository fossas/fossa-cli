{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Carthage.CarthageSpec
  ( spec
  ) where

import Control.Carrier.Diagnostics
import Data.Function ((&))
import Effect.ReadFS
import GraphUtil
import qualified Graphing as G
import Path
import Path.IO (makeAbsolute)
import Strategy.Carthage
import Test.Hspec

testProjectEmpty :: Path Rel File
testProjectEmpty = $(mkRelFile "test/Carthage/testdata/testempty/Cartfile.resolved")

testProjectComplex :: Path Rel File
testProjectComplex = $(mkRelFile "test/Carthage/testdata/testproject/Cartfile.resolved")

spec :: Spec
spec = do
  let runIt f = analyze f
        & runReadFSIO
        & runDiagnostics

  emptyResult <- runIO $ runIt =<< makeAbsolute testProjectEmpty
  complexResult <- runIO $ runIt =<< makeAbsolute testProjectComplex

  describe "carthage analyze" $ do
    it "should work for empty projects" $ do
      case emptyResult of
        Left err -> expectationFailure ("analyze failed: " <> show (renderFailureBundle err))
        Right result -> resultValue result `shouldBe` G.empty

    it "should work for a complex project" $ do
      case complexResult of
        Left err -> expectationFailure ("analyze failed: " <> show (renderFailureBundle err))
        Right result -> do
          let graph = resultValue result
          expectDirect [nimble713, swinject, ocmock] graph
          expectDeps [ nimble713
                     , nimble703
                     , swinject
                     , ocmock
                     , quick
                     , cwlPreconditionTesting
                     , cwlCatchException
                     ] graph
          expectEdges [ (nimble713, cwlPreconditionTesting)
                      , (nimble713, cwlCatchException)
                      , (swinject, nimble703)
                      , (swinject, quick)
                      ] graph

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
