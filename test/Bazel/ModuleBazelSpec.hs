{-# LANGUAGE QuasiQuotes #-}

module Bazel.ModuleBazelSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as TIO
import DepTypes (DepType (BazelType, MavenType), Dependency (..), VerConstraint (CEq))
import Graphing qualified
import Strategy.Bazel.ModuleBazel (
  BazelDep (..),
  BazelModuleFile (..),
  ModuleInfo (..),
  buildBazelGraph,
  moduleBazelParser,
 )
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (runParser)
import Text.RawString.QQ (r)

spec :: Spec
spec = do
  spec_parseSimple
  spec_parseMaven
  spec_parseVariables
  spec_parseInline
  spec_buildGraph

spec_parseSimple :: Spec
spec_parseSimple = do
  simpleInput <- runIO (TIO.readFile "test/Bazel/testdata/MODULE.bazel.simple")
  describe "MODULE.bazel simple parser" $ do
    it "should parse a simple MODULE.bazel with bazel_dep entries" $ do
      runParser moduleBazelParser "" simpleInput
        `shouldParse` BazelModuleFile
          { moduleInfo = Just (ModuleInfo "my_project" "1.0.0")
          , bazelDeps =
              [ BazelDep "rules_go" "0.57.0" Nothing
              , BazelDep "gazelle" "0.42.0" (Just "bazel_gazelle")
              , BazelDep "protobuf" "29.3" Nothing
              ]
          , mavenArtifacts = []
          , mavenRepositories = []
          }

spec_parseMaven :: Spec
spec_parseMaven = do
  mavenInput <- runIO (TIO.readFile "test/Bazel/testdata/MODULE.bazel.maven")
  describe "MODULE.bazel maven parser" $ do
    it "should parse MODULE.bazel with maven extension" $ do
      let result = runParser moduleBazelParser "" mavenInput
      case result of
        Left err -> fail (show err)
        Right parsed -> do
          moduleInfo parsed `shouldBe` Just (ModuleInfo "my_java_project" "2.0.0")
          bazelDeps parsed `shouldBe` [BazelDep "rules_jvm_external" "6.6" Nothing]
          mavenArtifacts parsed
            `shouldBe` [ "com.google.guava:guava:33.4.0-jre"
                       , "junit:junit:4.13.2"
                       , "org.slf4j:slf4j-api:2.0.16"
                       ]
          mavenRepositories parsed
            `shouldBe` [ "https://repo1.maven.org/maven2"
                       , "https://maven.google.com"
                       ]

spec_parseVariables :: Spec
spec_parseVariables = do
  variablesInput <- runIO (TIO.readFile "test/Bazel/testdata/MODULE.bazel.variables")
  describe "MODULE.bazel variable substitution" $ do
    it "should resolve named constant lists" $ do
      let result = runParser moduleBazelParser "" variablesInput
      case result of
        Left err -> fail (show err)
        Right parsed -> do
          moduleInfo parsed `shouldBe` Just (ModuleInfo "my_lib" "0.5.0")
          mavenArtifacts parsed
            `shouldBe` [ "com.google.guava:guava:33.4.0-jre"
                       , "com.google.protobuf:protobuf-java:4.29.3"
                       ]
          mavenRepositories parsed `shouldBe` ["https://repo1.maven.org/maven2"]

spec_parseInline :: Spec
spec_parseInline = describe "MODULE.bazel inline parsing" $ do
  it "should parse a minimal bazel_dep" $ do
    let input = [r|bazel_dep(name = "foo", version = "1.0")
|]
    runParser moduleBazelParser "" input
      `shouldParse` BazelModuleFile
        { moduleInfo = Nothing
        , bazelDeps = [BazelDep "foo" "1.0" Nothing]
        , mavenArtifacts = []
        , mavenRepositories = []
        }

  it "should handle comments" $ do
    let input =
          [r|# This is a comment
module(name = "test", version = "0.1")
# Another comment
bazel_dep(name = "bar", version = "2.0")
|]
    runParser moduleBazelParser "" input
      `shouldParse` BazelModuleFile
        { moduleInfo = Just (ModuleInfo "test" "0.1")
        , bazelDeps = [BazelDep "bar" "2.0" Nothing]
        , mavenArtifacts = []
        , mavenRepositories = []
        }

  it "should parse empty file" $ do
    runParser moduleBazelParser "" ""
      `shouldParse` BazelModuleFile
        { moduleInfo = Nothing
        , bazelDeps = []
        , mavenArtifacts = []
        , mavenRepositories = []
        }

spec_buildGraph :: Spec
spec_buildGraph = describe "buildBazelGraph" $ do
  it "should build graph with BazelType deps from bazel_dep entries" $ do
    let moduleFile =
          BazelModuleFile
            { moduleInfo = Just (ModuleInfo "my_project" "1.0.0")
            , bazelDeps = [BazelDep "rules_go" "0.57.0" Nothing]
            , mavenArtifacts = []
            , mavenRepositories = []
            }
        expected =
          Graphing.directs
            [ Dependency
                { dependencyType = BazelType
                , dependencyName = "rules_go"
                , dependencyVersion = Just (CEq "0.57.0")
                , dependencyLocations = []
                , dependencyEnvironments = mempty
                , dependencyTags = Map.empty
                }
            ]
    buildBazelGraph moduleFile `shouldBe` expected

  it "should build graph with MavenType deps from maven artifacts" $ do
    let moduleFile =
          BazelModuleFile
            { moduleInfo = Nothing
            , bazelDeps = []
            , mavenArtifacts = ["com.google.guava:guava:33.4.0-jre"]
            , mavenRepositories = []
            }
        graph = buildBazelGraph moduleFile
    Graphing.directList graph
      `shouldBe` [ Dependency
                    { dependencyType = MavenType
                    , dependencyName = "com.google.guava:guava"
                    , dependencyVersion = Just (CEq "33.4.0-jre")
                    , dependencyLocations = []
                    , dependencyEnvironments = mempty
                    , dependencyTags = Map.empty
                    }
                 ]
