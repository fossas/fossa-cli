{-# language TemplateHaskell #-}

module Go.GopkgTomlTest
  ( spec_analyze
  , spec_buildGraph
  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.Text.IO as TIO
import           Polysemy
import           Polysemy.Error

import           Diagnostics
import           Effect.Exec
import           Effect.GraphBuilder
import           Effect.ReadFS
import qualified Graph as G
import           Strategy.Go.GopkgToml
import           Types (BasicFileOpts(..))

import Test.Tasty.Hspec

gopkg :: Gopkg
gopkg = Gopkg
  { pkgConstraints =
      [ PkgConstraint
          { constraintName = "cat/fossa"
          , constraintSource = Just "https://someotherlocation/"
          , constraintVersion = Just "v3.0.0"
          , constraintBranch = Nothing
          , constraintRevision = Nothing
          }
      , PkgConstraint
          { constraintName = "repo/name/A"
          , constraintSource = Nothing
          , constraintVersion = Just "v1.0.0"
          , constraintBranch = Nothing
          , constraintRevision = Nothing
          }
      , PkgConstraint
          { constraintName = "repo/name/B"
          , constraintSource = Nothing
          , constraintVersion = Nothing
          , constraintBranch = Nothing
          , constraintRevision = Just "12345"
          }
      , PkgConstraint
          { constraintName = "repo/name/C"
          , constraintSource = Nothing
          , constraintVersion = Nothing
          , constraintBranch = Just "branchname"
          , constraintRevision = Nothing
          }
      ]
  , pkgOverrides =
    [ PkgConstraint
        { constraintName = "repo/name/B"
        , constraintSource = Nothing
        , constraintVersion = Nothing
        , constraintBranch = Just "overridebranch"
        , constraintRevision = Nothing
        }
    ]
  }

expected :: G.Graph
expected = run . evalGraphBuilder G.empty $ do
  ref1 <- addNode (G.Dependency
                        { dependencyType = G.GoType
                        , dependencyName = "cat/fossa"
                        , dependencyVersion = Just (G.CEq "v3.0.0")
                        , dependencyLocations = ["https://someotherlocation/"]
                        , dependencyTags = M.empty
                        })
  ref2 <- addNode (G.Dependency
                        { dependencyType = G.GoType
                        , dependencyName = "repo/name/A"
                        , dependencyVersion = Just (G.CEq "v1.0.0")
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        })
  ref3 <- addNode (G.Dependency
                        { dependencyType = G.GoType
                        , dependencyName = "repo/name/B"
                        , dependencyVersion = Just (G.CEq "overridebranch")
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        })
  ref4 <- addNode (G.Dependency
                        { dependencyType = G.GoType
                        , dependencyName = "repo/name/C"
                        , dependencyVersion = Just (G.CEq "branchname")
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        })
  addDirect ref1
  addDirect ref2
  addDirect ref3
  addDirect ref4

mockReadFSText :: Text -> Sem (ReadFS ': r) a -> Sem r a
mockReadFSText contents = interpret $ \case
  ReadContentsText _ -> pure contents
  _ -> error "unexpected ReadFS method. Expecting ReadContentsText"

testfile :: Path Rel File
testfile = $(mkRelFile "nonexistentfile")

spec_analyze :: Spec
spec_analyze = do
  contents <- runIO (TIO.readFile "test/Go/testdata/Gopkg.toml")

  describe "analyze" $
    it "should produce expected output" $ do
      let result = analyze (BasicFileOpts testfile)
            & mockReadFSText contents
            & execErrToCLIErr
            & readFSErrToCLIErr
            & runError @CLIErr
            & execConst (Left [])
            & run

      case result of
        Left err -> expectationFailure ("analyze failed: " <> show err)
        Right graph -> graph `shouldBe` expected

spec_buildGraph :: Spec
spec_buildGraph = do
  describe "buildGraph" $
    it "should produce expected output" $ do
      let result = buildGraph gopkg

      result `shouldBe` expected
