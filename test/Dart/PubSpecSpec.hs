module Dart.PubSpecSpec (
  spec,
) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Yaml (decodeEither')
import DepTypes
import GraphUtil (expectDeps, expectDirect, expectEdges)
import Strategy.Dart.PubSpec (
  PubSpecContent (..),
  PubSpecDepGitSource (..),
  PubSpecDepHostedSource (..),
  PubSpecDepPathSource (..),
  PubSpecDepSdkSource (..),
  PubSpecDepSource (..),
  buildGraph,
 )
import Strategy.Dart.PubSpecLock (PackageName (..))
import Test.Hspec

-- hostedSource :: (Maybe VerConstraint) -> Maybe Text -> Maybe Text

spec :: Spec
spec = do
  specFile <- runIO (BS.readFile "test/Dart/testdata/pubspec.yaml")
  describe "parse pubspec.yml" $
    it "should parse dependencies" $ do
      let expectedPubSpecContent =
            PubSpecContent
              { pubSpecDependencies =
                  Just $
                    Map.fromList
                      [ (PackageName "pkg_default", HostedSource $ PubSpecDepHostedSource (Just "1.3.0") Nothing Nothing)
                      , (PackageName "pkg_hosted", HostedSource $ PubSpecDepHostedSource (Just "^1.0.0") (Just "pkg_hosted") (Just "http://pub.dev"))
                      , (PackageName "pkg_a", GitSource $ PubSpecDepGitSource Nothing "https://github.com/user/pkg_a.git")
                      , (PackageName "pkg_b", GitSource $ PubSpecDepGitSource (Just "release-0.9") "https://github.com/user/pkg_b")
                      , (PackageName "pkg_sdk", SdkSource $ PubSpecDepSdkSource "flutter")
                      ]
              , pubSpecDevDependencies =
                  Just $
                    Map.fromList
                      [ (PackageName "pkg_dev_default", HostedSource $ PubSpecDepHostedSource (Just "1.0.0") Nothing Nothing)
                      ]
              , pubSpecDependenciesOverrides =
                  Just $
                    Map.fromList
                      [ (PackageName "pkg_b", PathSource $ PubSpecDepPathSource "./some/dir")
                      ]
              }

      case decodeEither' specFile of
        Right res -> res `shouldBe` expectedPubSpecContent
        Left err -> expectationFailure $ "failed to parse: " <> show err

  describe "build graph from pubspec.yml" $ do
    it "should create expected graph" $ do
      let pubSpecContent =
            PubSpecContent
              { pubSpecDependencies =
                  Just $
                    Map.fromList
                      [ (PackageName "pkg_default", HostedSource $ PubSpecDepHostedSource (Just "1.3.0") Nothing Nothing)
                      , (PackageName "pkg_hosted", HostedSource $ PubSpecDepHostedSource (Just "^1.0.0") (Just "pkg_hosted") (Just "http://pub.dev"))
                      , (PackageName "pkg_a", GitSource $ PubSpecDepGitSource Nothing "https://github.com/user/pkg_a.git")
                      ]
              , pubSpecDevDependencies = Just $ Map.fromList [(PackageName "pkg_dev_default", HostedSource $ PubSpecDepHostedSource (Just "^1.0.0") Nothing Nothing)]
              , pubSpecDependenciesOverrides = Nothing
              }

      let graph = buildGraph pubSpecContent
      let expectedGraphDeps =
            [ Dependency
                { dependencyType = PubType
                , dependencyName = "pkg_default"
                , dependencyVersion = Just $ CEq "1.3.0"
                , dependencyLocations = []
                , dependencyEnvironments = Set.singleton EnvProduction
                , dependencyTags = Map.empty
                }
            , Dependency
                { dependencyType = PubType
                , dependencyName = "pkg_hosted"
                , dependencyVersion = Just $ CEq "^1.0.0"
                , dependencyLocations = ["http://pub.dev"]
                , dependencyEnvironments = Set.singleton EnvProduction
                , dependencyTags = Map.empty
                }
            , Dependency
                { dependencyType = GitType
                , dependencyName = "https://github.com/user/pkg_a.git"
                , dependencyVersion = Nothing
                , dependencyLocations = []
                , dependencyEnvironments = Set.singleton EnvProduction
                , dependencyTags = Map.empty
                }
            , Dependency
                { dependencyType = PubType
                , dependencyName = "pkg_dev_default"
                , dependencyVersion = Just $ CEq "^1.0.0"
                , dependencyLocations = []
                , dependencyEnvironments = Set.singleton EnvDevelopment
                , dependencyTags = Map.empty
                }
            ]
      expectEdges [] graph
      expectDeps expectedGraphDeps graph
      expectDirect expectedGraphDeps graph

    it "should not graph, if dependency is overriden, and the new source is not supported" $ do
      let pubSpecContent =
            PubSpecContent
              { pubSpecDependencies = Just $ Map.fromList [(PackageName "pkg_b", GitSource $ PubSpecDepGitSource (Just "release-0.9") "https://github.com/user/pkg_b")]
              , pubSpecDependenciesOverrides = Just $ Map.fromList [(PackageName "pkg_b", PathSource $ PubSpecDepPathSource "./some/dir")]
              , pubSpecDevDependencies = Nothing
              }

      let graph = buildGraph pubSpecContent
      expectEdges [] graph
      expectDeps [] graph
      expectDirect [] graph

    it "should graph, if dependency is overriden, and the new source is supported" $ do
      let pubSpecContent =
            PubSpecContent
              { pubSpecDependencies =
                  Just $ Map.fromList [(PackageName "pkg_b", GitSource $ PubSpecDepGitSource (Just "release-0.9") "https://github.com/user/pkg_b")]
              , pubSpecDependenciesOverrides =
                  Just $ Map.fromList [(PackageName "pkg_b", GitSource $ PubSpecDepGitSource (Just "develop") "https://github.com/user/pkg_b")]
              , pubSpecDevDependencies = Nothing
              }
      let graph = buildGraph pubSpecContent
      let expectedGraphDeps =
            [ Dependency
                { dependencyType = GitType
                , dependencyName = "https://github.com/user/pkg_b"
                , dependencyVersion = Just $ CEq "develop"
                , dependencyLocations = []
                , dependencyEnvironments = Set.singleton EnvProduction
                , dependencyTags = Map.empty
                }
            ]
      expectEdges [] graph
      expectDeps expectedGraphDeps graph
      expectDirect expectedGraphDeps graph

    it "should not graph dependency of path sources" $ do
      let pubSpecContent =
            PubSpecContent
              { pubSpecDependencies = Just $ Map.fromList [(PackageName "pkg_sdk", PathSource $ PubSpecDepPathSource "./../some-dir/")]
              , pubSpecDevDependencies = Nothing
              , pubSpecDependenciesOverrides = Nothing
              }
      let graph = buildGraph pubSpecContent
      expectEdges [] graph
      expectDeps [] graph
      expectDirect [] graph

    it "should not graph dependency of sdk sources" $ do
      let pubSpecContent =
            PubSpecContent
              { pubSpecDependencies = Just $ Map.fromList [(PackageName "pkg_sdk", SdkSource $ PubSpecDepSdkSource "flutter")]
              , pubSpecDevDependencies = Nothing
              , pubSpecDependenciesOverrides = Nothing
              }
      let graph = buildGraph pubSpecContent
      expectEdges [] graph
      expectDeps [] graph
      expectDirect [] graph
