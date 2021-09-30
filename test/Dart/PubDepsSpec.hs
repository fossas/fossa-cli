module Dart.PubDepsSpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.IO qualified as TIO
import DepTypes
import GraphUtil (expectDeps, expectDirect, expectEdges)
import Strategy.Dart.PubDeps (PubDepPackage (..), buildGraph, depsCmdOutputParser)
import Strategy.Dart.PubSpecLock (
  PackageName (..),
  PubDepSource (..),
  PubLockContent (..),
  PubLockPackageHostedSource (..),
  PubLockPackageMetadata (..),
  PubLockPackagePathSource (..),
  PubLockPackageSdkSource (..),
 )
import Test.Hspec
import Text.Megaparsec

expectedDepsCmdOutput :: [PubDepPackage]
expectedDepsCmdOutput =
  [ PubDepPackage (PackageName "pkg_a") (Just "5.0.0") (Just $ Set.fromList [PackageName "pkg_aa"]) True
  , PubDepPackage (PackageName "pkg_b") (Just "4.0.0") Nothing True
  , PubDepPackage (PackageName "pkg_c") (Just "3.0.0") (Just $ Set.fromList [PackageName "pkg_ca", PackageName "pkg_cb"]) True
  , PubDepPackage (PackageName "pkg_d") (Just "3.0.1") Nothing True
  , PubDepPackage (PackageName "pkg_e") (Just "2.0.0") (Just $ Set.fromList [PackageName "pkg_ea"]) True
  , PubDepPackage (PackageName "pkg_aa") (Just "1.0.0") (Just $ Set.fromList [PackageName "pkg_ca"]) False
  , PubDepPackage (PackageName "pkg_ca") (Just "1.2.0") Nothing False
  , PubDepPackage (PackageName "pkg_cb") (Just "1.3.0") Nothing False
  , PubDepPackage (PackageName "pkg_ea") (Just "1.4.0") Nothing False
  ]

spec :: Spec
spec = do
  describe "pub deps -s compact" $ do
    contents <- runIO (TIO.readFile "test/Dart/testdata/pubdeps.compact")

    it "should parse content correctly" $ do
      case runParser depsCmdOutputParser "" contents of
        Left failCode -> expectationFailure $ show failCode
        Right result -> result `shouldBe` expectedDepsCmdOutput

  describe "buildGraph" $ do
    it "should build graph with edges" $ do
      let pubDepsContent =
            [ PubDepPackage (PackageName "pkg_a") (Just "1.8.0") (Just $ Set.fromList [PackageName "pkg_deep"]) True
            , PubDepPackage (PackageName "pkg_deep") (Just "1.10.0") (Just $ Set.fromList [PackageName "pkg_deeper"]) False
            , PubDepPackage (PackageName "pkg_deeper") (Just "1.20.0") Nothing False
            ]

      let lockContent =
            PubLockContent
              { packages =
                  Map.fromList
                    [
                      ( PackageName "pkg_a"
                      , PubLockPackageMetadata
                          { pubLockPackageIsDirect = True
                          , pubLockPackageSource = HostedSource $ PubLockPackageHostedSource (Just "pkg_a") (Just "https://pub.dartlang.org")
                          , pubLockPackageVersion = Just "1.8.0"
                          , pubLockPackageEnvironment = []
                          }
                      )
                    ,
                      ( PackageName "pkg_deep"
                      , PubLockPackageMetadata
                          { pubLockPackageIsDirect = False
                          , pubLockPackageSource = HostedSource $ PubLockPackageHostedSource (Just "pkg_deep") (Just "https://pub.dartlang.org")
                          , pubLockPackageVersion = Just "1.10.0"
                          , pubLockPackageEnvironment = []
                          }
                      )
                    ,
                      ( PackageName "pkg_deeper"
                      , PubLockPackageMetadata
                          { pubLockPackageIsDirect = False
                          , pubLockPackageSource = HostedSource $ PubLockPackageHostedSource (Just "pkg_deep") (Just "https://pub.dartlang.org")
                          , pubLockPackageVersion = Just "1.20.0"
                          , pubLockPackageEnvironment = []
                          }
                      )
                    ]
              }
      let graph = buildGraph lockContent pubDepsContent

      expectDirect
        [ Dependency
            { dependencyType = PubType
            , dependencyName = "pkg_a"
            , dependencyVersion = Just $ CEq "1.8.0"
            , dependencyLocations = ["https://pub.dartlang.org"]
            , dependencyEnvironments = mempty
            , dependencyTags = Map.empty
            }
        ]
        graph

      expectDeps
        [ Dependency
            { dependencyType = PubType
            , dependencyName = "pkg_a"
            , dependencyVersion = Just $ CEq "1.8.0"
            , dependencyLocations = ["https://pub.dartlang.org"]
            , dependencyEnvironments = mempty
            , dependencyTags = Map.empty
            }
        , Dependency
            { dependencyType = PubType
            , dependencyName = "pkg_deep"
            , dependencyVersion = Just $ CEq "1.10.0"
            , dependencyLocations = ["https://pub.dartlang.org"]
            , dependencyEnvironments = mempty
            , dependencyTags = Map.empty
            }
        , Dependency
            { dependencyType = PubType
            , dependencyName = "pkg_deeper"
            , dependencyVersion = Just $ CEq "1.20.0"
            , dependencyLocations = ["https://pub.dartlang.org"]
            , dependencyEnvironments = mempty
            , dependencyTags = Map.empty
            }
        ]
        graph

      expectEdges
        [
          ( Dependency
              { dependencyType = PubType
              , dependencyName = "pkg_a"
              , dependencyVersion = Just $ CEq "1.8.0"
              , dependencyLocations = ["https://pub.dartlang.org"]
              , dependencyEnvironments = mempty
              , dependencyTags = Map.empty
              }
          , Dependency
              { dependencyType = PubType
              , dependencyName = "pkg_deep"
              , dependencyVersion = Just $ CEq "1.10.0"
              , dependencyLocations = ["https://pub.dartlang.org"]
              , dependencyEnvironments = mempty
              , dependencyTags = Map.empty
              }
          )
        ,
          ( Dependency
              { dependencyType = PubType
              , dependencyName = "pkg_deep"
              , dependencyVersion = Just $ CEq "1.10.0"
              , dependencyLocations = ["https://pub.dartlang.org"]
              , dependencyEnvironments = mempty
              , dependencyTags = Map.empty
              }
          , Dependency
              { dependencyType = PubType
              , dependencyName = "pkg_deeper"
              , dependencyVersion = Just $ CEq "1.20.0"
              , dependencyLocations = ["https://pub.dartlang.org"]
              , dependencyEnvironments = mempty
              , dependencyTags = Map.empty
              }
          )
        ]
        graph

    it "should ignore path dependency" $ do
      let pubDepsContent = [PubDepPackage (PackageName "pkg_path") (Just "1.10.0") Nothing True]
      let lockContent =
            PubLockContent
              { packages =
                  Map.fromList
                    [
                      ( PackageName "pkg_path"
                      , PubLockPackageMetadata
                          { pubLockPackageIsDirect = True
                          , pubLockPackageSource = PathSource $ PubLockPackagePathSource "./../dir"
                          , pubLockPackageVersion = Just "1.8.0"
                          , pubLockPackageEnvironment = []
                          }
                      )
                    ]
              }

      let graph = buildGraph lockContent pubDepsContent
      expectDirect [] graph
      expectDeps [] graph
      expectEdges [] graph

    it "should ignore sdk dependency" $ do
      let pubDepsContent = [PubDepPackage (PackageName "pkg_sdk") (Just "1.10.0") Nothing True]

      let lockContent =
            PubLockContent
              { packages =
                  Map.fromList
                    [
                      ( PackageName "pkg_sdk"
                      , PubLockPackageMetadata
                          { pubLockPackageIsDirect = True
                          , pubLockPackageSource = SdkSource $ PubLockPackageSdkSource "pkg_sdk_source"
                          , pubLockPackageVersion = Just "1.8.0"
                          , pubLockPackageEnvironment = []
                          }
                      )
                    ]
              }

      let graph = buildGraph lockContent pubDepsContent
      expectDirect [] graph
      expectDeps [] graph
      expectEdges [] graph

    it "should build graph when dependencies have no edges" $ do
      let pubDepsContent = [PubDepPackage (PackageName "pkg_a") (Just "1.10.0") Nothing True]

      let lockContent =
            PubLockContent
              { packages =
                  Map.fromList
                    [
                      ( PackageName "pkg_a"
                      , PubLockPackageMetadata
                          { pubLockPackageIsDirect = True
                          , pubLockPackageSource = HostedSource $ PubLockPackageHostedSource Nothing Nothing
                          , pubLockPackageVersion = Just "1.8.0"
                          , pubLockPackageEnvironment = []
                          }
                      )
                    ]
              }

      let graph = buildGraph lockContent pubDepsContent
      expectDirect
        [ Dependency
            { dependencyType = PubType
            , dependencyName = "pkg_a"
            , dependencyVersion = Just $ CEq "1.8.0"
            , dependencyLocations = []
            , dependencyEnvironments = mempty
            , dependencyTags = Map.empty
            }
        ]
        graph
      expectDeps
        [ Dependency
            { dependencyType = PubType
            , dependencyName = "pkg_a"
            , dependencyVersion = Just $ CEq "1.8.0"
            , dependencyLocations = []
            , dependencyEnvironments = mempty
            , dependencyTags = Map.empty
            }
        ]
        graph
      expectEdges [] graph

    it "should retain supported transitive dependencies, even if predecessor is not supported" $ do
      let pubDepsContent =
            [ PubDepPackage (PackageName "pkg_a") (Just "1.8.0") (Just $ Set.fromList [PackageName "pkg_deep"]) True
            , PubDepPackage (PackageName "pkg_deep") (Just "1.10.0") (Just $ Set.fromList [PackageName "pkg_deeper"]) False
            , PubDepPackage (PackageName "pkg_deeper") (Just "1.20.0") Nothing False
            ]

      let lockContent =
            PubLockContent
              { packages =
                  Map.fromList
                    [
                      ( PackageName "pkg_a"
                      , PubLockPackageMetadata
                          { pubLockPackageIsDirect = True
                          , pubLockPackageSource = HostedSource $ PubLockPackageHostedSource (Just "pkg_a") (Just "https://pub.dartlang.org")
                          , pubLockPackageVersion = Just "1.8.0"
                          , pubLockPackageEnvironment = []
                          }
                      )
                    ,
                      ( PackageName "pkg_deep"
                      , PubLockPackageMetadata
                          { pubLockPackageIsDirect = False
                          , pubLockPackageSource = PathSource $ PubLockPackagePathSource "some/path/"
                          , pubLockPackageVersion = Just "1.10.0"
                          , pubLockPackageEnvironment = []
                          }
                      )
                    ,
                      ( PackageName "pkg_deeper"
                      , PubLockPackageMetadata
                          { pubLockPackageIsDirect = False
                          , pubLockPackageSource = HostedSource $ PubLockPackageHostedSource (Just "pkg_deep") (Just "https://pub.dartlang.org")
                          , pubLockPackageVersion = Just "1.20.0"
                          , pubLockPackageEnvironment = []
                          }
                      )
                    ]
              }
      let graph = buildGraph lockContent pubDepsContent

      expectDirect
        [ Dependency
            { dependencyType = PubType
            , dependencyName = "pkg_a"
            , dependencyVersion = Just $ CEq "1.8.0"
            , dependencyLocations = ["https://pub.dartlang.org"]
            , dependencyEnvironments = mempty
            , dependencyTags = Map.empty
            }
        ]
        graph

      expectDeps
        [ Dependency
            { dependencyType = PubType
            , dependencyName = "pkg_a"
            , dependencyVersion = Just $ CEq "1.8.0"
            , dependencyLocations = ["https://pub.dartlang.org"]
            , dependencyEnvironments = mempty
            , dependencyTags = Map.empty
            }
        , Dependency
            { dependencyType = PubType
            , dependencyName = "pkg_deeper"
            , dependencyVersion = Just $ CEq "1.20.0"
            , dependencyLocations = ["https://pub.dartlang.org"]
            , dependencyEnvironments = mempty
            , dependencyTags = Map.empty
            }
        ]
        graph

      expectEdges
        [
          ( Dependency
              { dependencyType = PubType
              , dependencyName = "pkg_a"
              , dependencyVersion = Just $ CEq "1.8.0"
              , dependencyLocations = ["https://pub.dartlang.org"]
              , dependencyEnvironments = mempty
              , dependencyTags = Map.empty
              }
          , Dependency
              { dependencyType = PubType
              , dependencyName = "pkg_deeper"
              , dependencyVersion = Just $ CEq "1.20.0"
              , dependencyLocations = ["https://pub.dartlang.org"]
              , dependencyEnvironments = mempty
              , dependencyTags = Map.empty
              }
          )
        ]
        graph
