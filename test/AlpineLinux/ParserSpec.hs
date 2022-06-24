{-# LANGUAGE QuasiQuotes #-}

module AlpineLinux.ParserSpec (spec) where

import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Strategy.AlpineLinux.Parser (PackageError (..), installedPackagesDatabaseParser)
import Strategy.AlpineLinux.Types (AlpinePackage (..))
import Test.Hspec (Expectation, Spec, SpecWith, before, describe, expectationFailure, it, shouldBe)
import Text.Megaparsec (parse)
import Text.RawString.QQ (r)

shouldParseInto :: Text -> [Either PackageError AlpinePackage] -> Expectation
shouldParseInto text value = do
  let parseResult = parse installedPackagesDatabaseParser testFileName text
  case parseResult of
    Left err ->
      expectationFailure $ "Unexpected parse failure: " <> show err
    Right packages ->
      packages `shouldBe` value

withExampleDatabase :: SpecWith Text -> Spec
withExampleDatabase =
  before (TIO.readFile testFilePath)

testFilePath :: String
testFilePath = "test/AlpineLinux/testdata/installed.example"

testFileName :: String
testFileName = "installed.example"

packageWithWhitespacePadding :: Text
packageWithWhitespacePadding =
  [r|
P:  package-name  
A:   x86_64  
V:   v1.1  
|]

packageMissingName :: Text
packageMissingName =
  [r|
A:x86_64
V:v1.1
T:A package with no name field
|]

packageMissingArch :: Text
packageMissingArch =
  [r|
P:package-name
V:v1.1
T:A package with no architecture field
|]

packageMissingVersion :: Text
packageMissingVersion =
  [r|
P:package-name
A:x86_64
T:A package with no version field
|]

spec :: Spec
spec =
  describe "installedPackagesDatabaseParser" $ do
    it "parses empty files" $ do
      "" `shouldParseInto` []
    withExampleDatabase $
      it "parses the example database" $ \exampleDatabase -> do
        exampleDatabase
          `shouldParseInto` [ Right (AlpinePackage{alpinePackageArchitecture = "x86_64", alpinePackageName = "musl", alpinePackageVersion = "1.2.2-r7"})
                            , Right (AlpinePackage{alpinePackageArchitecture = "x86_64", alpinePackageName = "busybox", alpinePackageVersion = "1.34.1-r4"})
                            , Right (AlpinePackage{alpinePackageArchitecture = "x86_64", alpinePackageName = "alpine-baselayout", alpinePackageVersion = "3.2.0-r18"})
                            , Right (AlpinePackage{alpinePackageArchitecture = "x86_64", alpinePackageName = "alpine-keys", alpinePackageVersion = "2.4-r1"})
                            , Right (AlpinePackage{alpinePackageArchitecture = "x86_64", alpinePackageName = "ca-certificates-bundle", alpinePackageVersion = "20211220-r0"})
                            , Right (AlpinePackage{alpinePackageArchitecture = "x86_64", alpinePackageName = "libcrypto1.1", alpinePackageVersion = "1.1.1n-r0"})
                            , Right (AlpinePackage{alpinePackageArchitecture = "x86_64", alpinePackageName = "libssl1.1", alpinePackageVersion = "1.1.1n-r0"})
                            , Right (AlpinePackage{alpinePackageArchitecture = "x86_64", alpinePackageName = "libretls", alpinePackageVersion = "3.3.4-r3"})
                            , Right (AlpinePackage{alpinePackageArchitecture = "x86_64", alpinePackageName = "ssl_client", alpinePackageVersion = "1.34.1-r4"})
                            , Right (AlpinePackage{alpinePackageArchitecture = "x86_64", alpinePackageName = "zlib", alpinePackageVersion = "1.2.12-r0"})
                            , Right (AlpinePackage{alpinePackageArchitecture = "x86_64", alpinePackageName = "apk-tools", alpinePackageVersion = "2.12.7-r3"})
                            , Right (AlpinePackage{alpinePackageArchitecture = "x86_64", alpinePackageName = "scanelf", alpinePackageVersion = "1.3.3-r0"})
                            , Right (AlpinePackage{alpinePackageArchitecture = "x86_64", alpinePackageName = "musl-utils", alpinePackageVersion = "1.2.2-r7"})
                            , Right (AlpinePackage{alpinePackageArchitecture = "x86_64", alpinePackageName = "libc-utils", alpinePackageVersion = "0.7.2-r3"})
                            ]
    it "trims the returned strings" $ do
      packageWithWhitespacePadding
        `shouldParseInto` [ Right
                              ( AlpinePackage
                                  { alpinePackageArchitecture = "x86_64"
                                  , alpinePackageName = "package-name"
                                  , alpinePackageVersion = "v1.1"
                                  }
                              )
                          ]
    it "parses an error if the package name is missing" $ do
      packageMissingName
        `shouldParseInto` [Left MissingPackageName]
    it "parses an error if the package architecture is missing" $ do
      packageMissingArch
        `shouldParseInto` [Left $ MissingPackageArchitecture "package-name"]
    it "parses an error if the package architecture is missing" $ do
      packageMissingVersion
        `shouldParseInto` [Left $ MissingPackageVersion "package-name"]
