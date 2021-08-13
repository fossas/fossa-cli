module Dart.PubSpecLockSpec (
  spec,
) where

import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Data.Yaml (decodeEither')
import DepTypes
import GraphUtil (expectDeps, expectDirect)
import Graphing (empty)
import Strategy.Dart.PubSpecLock (
  PackageName (..),
  PubDepSource (..),
  PubLockContent (..),
  PubLockPackageGitSource (..),
  PubLockPackageHostedSource (..),
  PubLockPackageMetadata (..),
  PubLockPackagePathSource (..),
  PubLockPackageSdkSource (..),
  buildGraph,
  toDependency,
 )
import Test.Hspec

expectedLockFile :: PubLockContent
expectedLockFile =
  PubLockContent
    { packages =
        Map.fromList
          [
            ( PackageName "pkg_hosted"
            , PubLockPackageMetadata
                { pubLockPackageIsDirect = False
                , pubLockPackageSource = HostedSource $ PubLockPackageHostedSource (Just "pkg_hosted") (Just "https://pub.dartlang.org")
                , pubLockPackageVersion = Just "1.1"
                , pubLockPackageEnvironment = []
                }
            )
          ,
            ( PackageName "pkg_git"
            , PubLockPackageMetadata
                { pubLockPackageIsDirect = False
                , pubLockPackageSource = GitSource $ PubLockPackageGitSource "https://github.com/user/pkg" "release-0.9"
                , pubLockPackageVersion = Just "1.2"
                , pubLockPackageEnvironment = []
                }
            )
          ,
            ( PackageName "pkg_sdk"
            , PubLockPackageMetadata
                { pubLockPackageIsDirect = False
                , pubLockPackageSource = SdkSource $ PubLockPackageSdkSource "flutter"
                , pubLockPackageVersion = Just "1.3"
                , pubLockPackageEnvironment = []
                }
            )
          ,
            ( PackageName "pkg_file"
            , PubLockPackageMetadata
                { pubLockPackageIsDirect = False
                , pubLockPackageSource = PathSource $ PubLockPackagePathSource "/Users/dir/pkg_dir"
                , pubLockPackageVersion = Just "1.4"
                , pubLockPackageEnvironment = []
                }
            )
          ,
            ( PackageName "pkg_hosted_direct"
            , PubLockPackageMetadata
                { pubLockPackageIsDirect = True
                , pubLockPackageSource = HostedSource $ PubLockPackageHostedSource (Just "pkg_hosted_direct") (Just "https://pub.dartlang.org")
                , pubLockPackageVersion = Just "1.5"
                , pubLockPackageEnvironment = [EnvProduction]
                }
            )
          ,
            ( PackageName "pkg_hosted_direct_dev"
            , PubLockPackageMetadata
                { pubLockPackageIsDirect = True
                , pubLockPackageSource = HostedSource $ PubLockPackageHostedSource (Just "pkg_hosted_direct_dev") (Just "https://pub.dartlang.org")
                , pubLockPackageVersion = Just "1.6"
                , pubLockPackageEnvironment = [EnvDevelopment]
                }
            )
          ]
    }

spec :: Spec
spec = do
  lockFile <- runIO (BS.readFile "test/Dart/testdata/pubspec.lock")

  describe "pubspec.lock parsing" $
    it "should parse file correctly" $
      case decodeEither' lockFile of
        Right res -> res `shouldBe` expectedLockFile
        Left err -> expectationFailure $ "failed to parse: " <> show err

  describe "pubLockPackage to dependency conversion" $ do
    it "should create dependency for hosted sources" $ do
      let pkg =
            PubLockPackageMetadata
              { pubLockPackageIsDirect = True
              , pubLockPackageSource = HostedSource $ PubLockPackageHostedSource (Just "pkg_a") (Just "https://pub.dartlang.org")
              , pubLockPackageVersion = Just "1.1"
              , pubLockPackageEnvironment = [EnvDevelopment]
              }
      let expectedDependency =
            Dependency
              { dependencyType = PubType
              , dependencyName = "pkg_a"
              , dependencyVersion = Just $ CEq "1.1"
              , dependencyLocations = ["https://pub.dartlang.org"]
              , dependencyEnvironments = [EnvDevelopment]
              , dependencyTags = Map.empty
              }
      toDependency (PackageName "pkg_a") pkg `shouldBe` expectedDependency

    it "should create dependency for git sources" $ do
      let pkg =
            PubLockPackageMetadata
              { pubLockPackageIsDirect = True
              , pubLockPackageSource = GitSource $ PubLockPackageGitSource "https://github.com/user/pkg" "release-0.9"
              , pubLockPackageVersion = Just "1.1"
              , pubLockPackageEnvironment = []
              }
      let expectedDependency =
            Dependency
              { dependencyType = GitType
              , dependencyName = "https://github.com/user/pkg"
              , dependencyVersion = Just $ CEq "release-0.9"
              , dependencyLocations = []
              , dependencyEnvironments = []
              , dependencyTags = Map.empty
              }
      toDependency (PackageName "pkg_b") pkg `shouldBe` expectedDependency

  describe "graphing from pubspec.lock" $ do
    it "should not create graphing for packages sourced from sdk" $ do
      let sdkSources =
            PubLockContent $
              Map.filterWithKey (\x _ -> (unPackageName x) == "pkg_sdk") (packages expectedLockFile)

      Map.null (packages sdkSources) `shouldBe` False
      buildGraph sdkSources `shouldBe` Graphing.empty

    it "should not create graphing for packages sourced from file" $ do
      let fileSources =
            PubLockContent $
              Map.filterWithKey (\x _ -> (unPackageName x) == "pkg_file") (packages expectedLockFile)

      Map.null (packages fileSources) `shouldBe` False
      buildGraph fileSources `shouldBe` Graphing.empty

    it "should graph direct and deep dependencies" $ do
      let lockContent =
            PubLockContent
              { packages =
                  Map.fromList
                    [
                      ( PackageName "pkg_direct"
                      , PubLockPackageMetadata
                          { pubLockPackageIsDirect = True
                          , pubLockPackageSource = HostedSource $ PubLockPackageHostedSource (Just "pkg_direct") (Just "some-url-1")
                          , pubLockPackageVersion = Nothing
                          , pubLockPackageEnvironment = []
                          }
                      )
                    ,
                      ( PackageName "pkg_deep"
                      , PubLockPackageMetadata
                          { pubLockPackageIsDirect = False
                          , pubLockPackageSource = HostedSource $ PubLockPackageHostedSource (Just "pkg_deep") (Just "some-url-2")
                          , pubLockPackageVersion = Nothing
                          , pubLockPackageEnvironment = []
                          }
                      )
                    ]
              }
      let graph = buildGraph lockContent

      expectDirect
        [ Dependency
            { dependencyType = PubType
            , dependencyName = "pkg_direct"
            , dependencyVersion = Nothing
            , dependencyLocations = ["some-url-1"]
            , dependencyEnvironments = []
            , dependencyTags = Map.empty
            }
        ]
        graph

      expectDeps
        [ Dependency
            { dependencyType = PubType
            , dependencyName = "pkg_direct"
            , dependencyVersion = Nothing
            , dependencyLocations = ["some-url-1"]
            , dependencyEnvironments = []
            , dependencyTags = Map.empty
            }
        , Dependency
            { dependencyType = PubType
            , dependencyName = "pkg_deep"
            , dependencyVersion = Nothing
            , dependencyLocations = ["some-url-2"]
            , dependencyEnvironments = []
            , dependencyTags = Map.empty
            }
        ]
        graph
