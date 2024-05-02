module Swift.PackageResolvedSpec (
  spec,
) where

import Data.Aeson (decodeFileStrict')
import Strategy.Swift.PackageResolved (
  SwiftPackageResolvedFile (..),
  SwiftResolvedPackage (..),
 )
import Test.Hspec (Spec, describe, it, shouldBe)

expectedV1ResolvedContent :: SwiftPackageResolvedFile
expectedV1ResolvedContent =
  SwiftPackageResolvedFile
    1
    [ SwiftResolvedPackage
        "grpc-swift"
        "https://github.com/grpc/grpc-swift.git"
        Nothing
        (Just "9e464a75079928366aa7041769a271fac89271bf")
        (Just "1.0.0")
    , SwiftResolvedPackage
        "Opentracing"
        "https://github.com/undefinedlabs/opentracing-objc"
        (Just "master")
        Nothing
        Nothing
    , SwiftResolvedPackage
        "Reachability"
        "https://github.com/ashleymills/Reachability.swift"
        Nothing
        Nothing
        (Just "5.1.0")
    ]

expectedV2ResolvedContent :: SwiftPackageResolvedFile
expectedV2ResolvedContent =
  SwiftPackageResolvedFile
    { version = 2
    , pinnedPackages =
        [ SwiftResolvedPackage
            { package = "iqkeyboardmanager"
            , repositoryURL = "https://github.com/hackiftekhar/IQKeyboardManager.git"
            , repositoryBranch = Nothing
            , repositoryRevision = Just "9ab144a1a6c6ae8dad25840610c072709b15d8b5"
            , repositoryVersion = Nothing
            }
        , SwiftResolvedPackage
            { package = "popupview"
            , repositoryURL = "https://github.com/exyte/PopupView.git"
            , repositoryBranch = Nothing
            , repositoryRevision = Just "521b2ddc2ae8160f2816e03fa1f7be0937db8fff"
            , repositoryVersion = Just "1.1.1"
            }
        , SwiftResolvedPackage
            { package = "realm-core"
            , repositoryURL = "https://github.com/realm/realm-core"
            , repositoryBranch = Nothing
            , repositoryRevision = Just "d97eed3ae7dff2f2e2ffbda83fa8f3b8c445c6ba"
            , repositoryVersion = Just "11.17.0"
            }
        , SwiftResolvedPackage
            { package = "realm-swift"
            , repositoryURL = "https://github.com/realm/realm-swift.git"
            , repositoryBranch = Nothing
            , repositoryRevision = Just "7f123e48ec12926bb5080e449df360127ee0352d"
            , repositoryVersion = Just "10.26.0"
            }
        ]
    }

expectedV3ResolvedContent :: SwiftPackageResolvedFile
expectedV3ResolvedContent =
  SwiftPackageResolvedFile
    { version = 3
    , pinnedPackages =
        [ SwiftResolvedPackage
            { package = "vonage-client-sdk-video"
            , repositoryURL = "https://github.com/opentok/vonage-client-sdk-video.git"
            , repositoryBranch = Nothing
            , repositoryRevision = Just "e4b1af1808067f0c0a66fa85aaf99915b542a579"
            , repositoryVersion = Just "2.27.2"
            }
        ]
    }

spec :: Spec
spec = do
  describe "parse Package.resolved file" $ do
    it "should parse v1 content correctly" $ do
      resolvedFile <- decodeFileStrict' "test/Swift/testdata/v1/Package.resolved"
      resolvedFile `shouldBe` Just expectedV1ResolvedContent
    it "should parse v2 content correctly" $ do
      resolvedFile <- decodeFileStrict' "test/Swift/testdata/v2/Package.resolved"
      resolvedFile `shouldBe` Just expectedV2ResolvedContent
    it "should parse v3 content correctly" $ do
      resolvedFile <- decodeFileStrict' "test/Swift/testdata/v3/Package.resolved"
      resolvedFile `shouldBe` Just expectedV3ResolvedContent
