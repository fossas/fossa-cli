module Swift.PackageResolvedSpec (
  spec,
) where

import Data.Aeson (decodeFileStrict')
import Strategy.Swift.PackageResolved (
  SwiftPackageResolvedFile (..),
  SwiftResolvedPackage (..),
 )
import Test.Hspec (Spec, describe, it, shouldBe)

expectedResolvedContent :: SwiftPackageResolvedFile
expectedResolvedContent =
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

spec :: Spec
spec = do
  describe "parse Package.resolved file" $
    it "should parse content correctly" $ do
      resolvedFile <- decodeFileStrict' "test/Swift/testdata/Package.resolved"
      resolvedFile `shouldBe` Just expectedResolvedContent
