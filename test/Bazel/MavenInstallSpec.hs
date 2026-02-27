module Bazel.MavenInstallSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import DepTypes (DepType (MavenType), Dependency (..), VerConstraint (CEq))
import GraphUtil (expectDeps, expectDirect, expectEdges)
import Strategy.Bazel.MavenInstall (
  MavenArtifactInfo (..),
  MavenDependencyTree (..),
  MavenInstallJson (..),
  TreeArtifact (..),
  buildMavenInstallGraph,
 )
import Test.Hspec (Spec, describe, it, runIO, shouldSatisfy)

mkMavenDep :: Text -> Text -> Dependency
mkMavenDep name ver =
  Dependency
    { dependencyType = MavenType
    , dependencyName = name
    , dependencyVersion = Just (CEq ver)
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

guava :: Dependency
guava = mkMavenDep "com.google.guava:guava" "33.4.0-jre"

failureaccess :: Dependency
failureaccess = mkMavenDep "com.google.guava:failureaccess" "1.0.2"

listenablefuture :: Dependency
listenablefuture = mkMavenDep "com.google.guava:listenablefuture" "9999.0-empty-to-avoid-conflict-with-guava"

junit :: Dependency
junit = mkMavenDep "junit:junit" "4.13.2"

hamcrest :: Dependency
hamcrest = mkMavenDep "org.hamcrest:hamcrest-core" "1.3"

slf4j :: Dependency
slf4j = mkMavenDep "org.slf4j:slf4j-api" "2.0.16"

spec :: Spec
spec = do
  spec_parse
  spec_buildGraph
  spec_v1Fallback
  spec_edgeCases

spec_parse :: Spec
spec_parse = do
  jsonInput <- runIO (BL.readFile "test/Bazel/testdata/maven_install.json")
  describe "maven_install.json parser" $ do
    it "should parse the dependency_tree format" $ do
      case Aeson.eitherDecode jsonInput of
        Left err -> fail err
        Right (parsed :: MavenInstallJson) ->
          mavenDependencyTree parsed `shouldSatisfy` \case
            Just _ -> True
            Nothing -> False

spec_buildGraph :: Spec
spec_buildGraph = do
  jsonInput <- runIO (BL.readFile "test/Bazel/testdata/maven_install.json")
  describe "maven_install.json graph building" $ do
    it "should include all 6 artifacts" $ do
      case Aeson.eitherDecode jsonInput of
        Left err -> fail err
        Right parsed ->
          expectDeps [guava, failureaccess, listenablefuture, junit, hamcrest, slf4j] (buildMavenInstallGraph parsed)

    it "should mark root artifacts as direct" $ do
      case Aeson.eitherDecode jsonInput of
        Left err -> fail err
        Right parsed ->
          expectDirect [guava, junit, slf4j] (buildMavenInstallGraph parsed)

    it "should have correct transitive edges" $ do
      case Aeson.eitherDecode jsonInput of
        Left err -> fail err
        Right parsed ->
          expectEdges
            [ (guava, failureaccess)
            , (guava, listenablefuture)
            , (junit, hamcrest)
            ]
            (buildMavenInstallGraph parsed)

spec_v1Fallback :: Spec
spec_v1Fallback = describe "maven_install.json v1 format (artifacts map)" $ do
  it "should build graph from artifacts map when no dependency_tree" $ do
    let v1Json =
          MavenInstallJson
            { mavenArtifacts =
                Map.fromList
                  [ ("com.google.guava:guava", MavenArtifactInfo "33.4.0-jre")
                  , ("junit:junit", MavenArtifactInfo "4.13.2")
                  ]
            , mavenDependencyTree = Nothing
            }
        graph = buildMavenInstallGraph v1Json
    expectDirect [guava, junit] graph
    -- v1 has no edge info
    expectEdges [] graph

spec_edgeCases :: Spec
spec_edgeCases = describe "maven_install.json edge cases" $ do
  it "should handle empty lockfile" $ do
    let emptyJson =
          MavenInstallJson
            { mavenArtifacts = Map.empty
            , mavenDependencyTree = Nothing
            }
        graph = buildMavenInstallGraph emptyJson
    expectDeps [] graph

  it "should parse coordinates with packaging (group:artifact:jar:version)" $ do
    let result = buildMavenInstallGraph $ jsonWithCoord "com.example:lib:jar:2.0"
    expectDirect [mkMavenDep "com.example:lib" "2.0"] result

  it "should parse coordinates with packaging and classifier (group:artifact:jar:sources:version)" $ do
    let result = buildMavenInstallGraph $ jsonWithCoord "com.example:lib:jar:sources:3.0"
    expectDirect [mkMavenDep "com.example:lib" "3.0"] result

-- Helper to create a MavenInstallJson with a single coord in the dependency tree.
jsonWithCoord :: Text -> MavenInstallJson
jsonWithCoord coord =
  MavenInstallJson
    { mavenArtifacts = Map.empty
    , mavenDependencyTree =
        Just
          MavenDependencyTree
            { treeArtifacts = [TreeArtifact coord []]
            }
    }
