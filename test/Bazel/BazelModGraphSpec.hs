module Bazel.BazelModGraphSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import DepTypes (DepType (BazelType), Dependency (..), VerConstraint (CEq))
import GraphUtil (expectDeps, expectDirect, expectEdges)
import Strategy.Bazel.BazelModGraph (
  BazelModGraphJson (..),
  BazelModGraphNode (..),
  buildModGraphDeps,
 )
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

mkDep :: Text -> Text -> Dependency
mkDep name ver =
  Dependency
    { dependencyType = BazelType
    , dependencyName = name
    , dependencyVersion = Just (CEq ver)
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

spec :: Spec
spec = do
  spec_parse
  spec_buildGraph
  spec_edgeCases

spec_parse :: Spec
spec_parse = do
  jsonInput <- runIO (BL.readFile "test/Bazel/testdata/bazel_mod_graph.json")
  describe "bazel mod graph JSON parser" $ do
    it "should parse the JSON output" $ do
      case Aeson.eitherDecode jsonInput of
        Left err -> fail err
        Right parsed -> do
          modGraphKey parsed `shouldBe` "my_project"
          modGraphVersion parsed `shouldBe` "1.0.0"
          length (modGraphDeps parsed) `shouldBe` 2

    it "should parse nested dependencies" $ do
      case Aeson.eitherDecode jsonInput of
        Left err -> fail err
        Right parsed -> do
          case modGraphDeps parsed of
            (rulesGo : _) -> do
              nodeKey rulesGo `shouldBe` "rules_go"
              length (nodeDeps rulesGo) `shouldBe` 1
            [] -> fail "Expected at least one dependency"

spec_buildGraph :: Spec
spec_buildGraph = do
  jsonInput <- runIO (BL.readFile "test/Bazel/testdata/bazel_mod_graph.json")
  describe "bazel mod graph graph building" $ do
    it "should mark root deps as direct" $ do
      case Aeson.eitherDecode jsonInput of
        Left err -> fail err
        Right parsed -> do
          let graph = buildModGraphDeps parsed
          expectDirect
            [ mkDep "rules_go" "0.57.0"
            , mkDep "protobuf" "29.3"
            ]
            graph

    it "should include transitive deps" $ do
      case Aeson.eitherDecode jsonInput of
        Left err -> fail err
        Right parsed -> do
          let graph = buildModGraphDeps parsed
          expectDeps
            [ mkDep "rules_go" "0.57.0"
            , mkDep "protobuf" "29.3"
            , mkDep "platforms" "0.0.10"
            , mkDep "abseil-cpp" "20240722.0"
            ]
            graph

    it "should have correct edges" $ do
      case Aeson.eitherDecode jsonInput of
        Left err -> fail err
        Right parsed -> do
          let graph = buildModGraphDeps parsed
              rulesGo = mkDep "rules_go" "0.57.0"
              protobuf = mkDep "protobuf" "29.3"
              platforms = mkDep "platforms" "0.0.10"
              abseil = mkDep "abseil-cpp" "20240722.0"
          expectEdges
            [ (rulesGo, platforms)
            , (protobuf, platforms)
            , (protobuf, abseil)
            ]
            graph

spec_edgeCases :: Spec
spec_edgeCases = describe "bazel mod graph edge cases" $ do
  it "should handle empty dependencies" $ do
    let input =
          BazelModGraphJson
            { modGraphKey = "empty_project"
            , modGraphVersion = "0.1.0"
            , modGraphDeps = []
            }
        graph = buildModGraphDeps input
    expectDeps [] graph
    expectDirect [] graph

  it "should handle nodes with no version" $ do
    let input =
          BazelModGraphJson
            { modGraphKey = "root"
            , modGraphVersion = ""
            , modGraphDeps =
                [ BazelModGraphNode
                    { nodeKey = "some_dep"
                    , nodeVersion = ""
                    , nodeDeps = []
                    }
                ]
            }
        graph = buildModGraphDeps input
        depNoVer =
          Dependency
            { dependencyType = BazelType
            , dependencyName = "some_dep"
            , dependencyVersion = Nothing
            , dependencyLocations = []
            , dependencyEnvironments = mempty
            , dependencyTags = Map.empty
            }
    expectDirect [depNoVer] graph
