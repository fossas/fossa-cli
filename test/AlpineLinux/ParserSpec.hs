module AlpineLinux.ParserSpec (spec) where

import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Strategy.AlpineLinux.Parser (PackageError (..), installedPackagesDatabaseParser)
import Strategy.AlpineLinux.Types (AlpinePackage (..))
import Test.Hspec (Spec, SpecWith, before, describe, expectationFailure, it, shouldBe)
import Text.Megaparsec (parse)

testFilePath :: String
testFilePath = "test/AlpineLinux/testdata/installed.example"

testFileName :: String
testFileName = "installed.example"

spec :: Spec
spec =
  describe "installedPackagesDatabaseParser" $ do
    it "parses empty files" $ do
      let packages = parse installedPackagesDatabaseParser testFileName ""
      case packages of
        Left err ->
          expectationFailure $ "Unexpected parse failure: " <> show err
        Right val ->
          val `shouldBe` []
    withExampleDatabase $
      it "parses the example database" $ \exampleDatabase -> do
        let packages = parse installedPackagesDatabaseParser testFileName exampleDatabase
        case packages of
          Left err ->
            expectationFailure $ "Unexpected parse failure: " <> show err
          Right p -> do
            length p `shouldBe` 14
            (take 1 . drop 5 $ p)
              `shouldBe` [ Right
                            ( AlpinePackage
                                { alpinePackageArchitecture = "x86_64"
                                , alpinePackageName = "libcrypto1.1"
                                , alpinePackageVersion = "1.1.1n-r0"
                                }
                            )
                         ]
    it "parses an error if the package name is missing" $ do
      let packages = parse installedPackagesDatabaseParser testFileName "A:x86-64\r\nV:1.1.1\r\n\r\n"
      case packages of
        Left err ->
          expectationFailure $ "Unexpected parse failure: " <> show err
        Right p -> do
          length p `shouldBe` 1
          head p `shouldBe` Left MissingPackageName
    it "parses an error if the package architecture is missing" $ do
      let packages = parse installedPackagesDatabaseParser testFileName "P:package-name\r\nV:1.1.1\r\n\r\n"
      case packages of
        Left err ->
          expectationFailure $ "Unexpected parse failure: " <> show err
        Right p -> do
          length p `shouldBe` 1
          head p `shouldBe` Left (MissingPackageArchitecture "package-name")
    it "parses an error if the package architecture is missing" $ do
      let packages = parse installedPackagesDatabaseParser testFileName "P:package-name\r\nA:x86-64\r\n\r\n"
      case packages of
        Left err ->
          expectationFailure $ "Unexpected parse failure: " <> show err
        Right p -> do
          length p `shouldBe` 1
          head p `shouldBe` Left (MissingPackageVersion "package-name")

withExampleDatabase :: SpecWith Text -> Spec
withExampleDatabase =
  before (TIO.readFile testFilePath)
