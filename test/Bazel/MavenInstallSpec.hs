module Bazel.MavenInstallSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import DepTypes (DepType (MavenType), Dependency (..), VerConstraint (CEq))
import Graphing qualified
import Strategy.Bazel.MavenInstall (
  MavenInstallJson (..),
  buildMavenInstallGraph,
 )
import Test.Hspec (Spec, describe, it, runIO, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
  spec_parse
  spec_buildGraph

spec_parse :: Spec
spec_parse = do
  jsonInput <- runIO (BL.readFile "test/Bazel/testdata/maven_install.json")
  describe "maven_install.json parser" $ do
    it "should parse the lockfile" $ do
      case Aeson.eitherDecode jsonInput of
        Left err -> fail err
        Right (parsed :: MavenInstallJson) -> do
          mavenDependencyTree parsed `shouldSatisfy` \case
            Just _ -> True
            Nothing -> False

spec_buildGraph :: Spec
spec_buildGraph = do
  jsonInput <- runIO (BL.readFile "test/Bazel/testdata/maven_install.json")
  describe "maven_install.json graph building" $ do
    it "should build graph with transitive dependencies" $ do
      case Aeson.eitherDecode jsonInput of
        Left err -> fail err
        Right parsed -> do
          let graph = buildMavenInstallGraph parsed
              allDeps = Graphing.vertexList graph
              directDeps = Graphing.directList graph

          -- There should be 6 total artifacts
          length allDeps `shouldBe` 6

          -- guava, junit, and slf4j-api are roots (not dependencies of anything else)
          -- failureaccess, listenablefuture, and hamcrest-core are transitive
          let directNames = map dependencyName directDeps
          directNames `shouldSatisfy` elem "com.google.guava:guava"
          directNames `shouldSatisfy` elem "junit:junit"
          directNames `shouldSatisfy` elem "org.slf4j:slf4j-api"

          -- Check that guava has correct version
          case filter (\d -> dependencyName d == "com.google.guava:guava") allDeps of
            (guavaDep : _) -> do
              dependencyVersion guavaDep `shouldBe` Just (CEq "33.4.0-jre")
              dependencyType guavaDep `shouldBe` MavenType
            [] -> fail "Expected to find guava dependency"

          -- Check edges exist (guava -> failureaccess)
          Graphing.hasEdge
            ( Dependency
                { dependencyType = MavenType
                , dependencyName = "com.google.guava:guava"
                , dependencyVersion = Just (CEq "33.4.0-jre")
                , dependencyLocations = []
                , dependencyEnvironments = mempty
                , dependencyTags = Map.empty
                }
            )
            ( Dependency
                { dependencyType = MavenType
                , dependencyName = "com.google.guava:failureaccess"
                , dependencyVersion = Just (CEq "1.0.2")
                , dependencyLocations = []
                , dependencyEnvironments = mempty
                , dependencyTags = Map.empty
                }
            )
            graph
            `shouldBe` True
