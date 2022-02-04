{-# LANGUAGE QuasiQuotes #-}

module App.Fossa.VSI.DynLinked.Internal.ResolveSpec (spec) where

import App.Fossa.VSI.DynLinked.Internal.Resolve (parseLinuxDistro, toDependency)
import App.Fossa.VSI.DynLinked.Types (LinuxDistro (..), LinuxPackageManager (..), LinuxPackageMetadata (..), ResolvedLinuxPackage (..))
import Data.Text (Text)
import Data.Void (Void)
import DepTypes (DepType (..), Dependency (..), VerConstraint (CEq))
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
import Text.Megaparsec (Parsec, parse)
import Text.RawString.QQ (r)

parseMatch :: (Show a, Eq a) => Parsec Void Text a -> Text -> a -> Expectation
parseMatch parser input expected = parse parser "" input `shouldParse` expected

shouldParseInto :: Text -> LinuxDistro -> Expectation
shouldParseInto = parseMatch parseLinuxDistro

shouldFailParsing :: Text -> Expectation
shouldFailParsing input = parse parseLinuxDistro "" `shouldFailOn` input

spec :: Spec
spec = do
  describe "resolves linux dependencies to srclib dependencies" $ do
    it "resolves apk" $ do
      let original = ResolvedLinuxPackage LinuxPackageManagerAPK placeholderMeta
      let expected = Dependency LinuxAPK "id#distro#release" (Just $ CEq "arch#revision") [] mempty mempty
      toDependency placeholderDistro original `shouldBe` expected

    it "resolves debian" $ do
      let original = ResolvedLinuxPackage LinuxPackageManagerDEB placeholderMeta
      let expected = Dependency LinuxDEB "id#distro#release" (Just $ CEq "arch#revision") [] mempty mempty
      toDependency placeholderDistro original `shouldBe` expected

    it "resolves rpm with epoch" $ do
      let original = ResolvedLinuxPackage LinuxPackageManagerRPM placeholderMeta
      let expected = Dependency LinuxRPM "id#distro#release" (Just $ CEq "arch#epoch:revision") [] mempty mempty
      toDependency placeholderDistro original `shouldBe` expected

    it "resolves rpm without epoch" $ do
      let original = ResolvedLinuxPackage LinuxPackageManagerRPM $ baseMeta Nothing
      let expected = Dependency LinuxRPM "id#distro#release" (Just $ CEq "arch#revision") [] mempty mempty
      toDependency placeholderDistro original `shouldBe` expected

  describe "parses linux distro" $ do
    it "parses simple distro" $ do
      simpleDistro `shouldParseInto` expectedSimpleDistro

    it "parses complex distro" $ do
      complexDistro `shouldParseInto` expectedComplexDistro

    it "fails missing pieces distro" $ do
      shouldFailParsing malformedDistro

placeholderMeta :: LinuxPackageMetadata
placeholderMeta = baseMeta (Just "epoch")

baseMeta :: Maybe Text -> LinuxPackageMetadata
baseMeta = LinuxPackageMetadata "id" "revision" "arch"

placeholderDistro :: LinuxDistro
placeholderDistro = LinuxDistro "distro" "release"

simpleDistro :: Text
simpleDistro =
  [r| ID=distro
      VERSION_ID=release
  |]

malformedDistro :: Text
malformedDistro = "ID=distro"

expectedSimpleDistro :: LinuxDistro
expectedSimpleDistro = LinuxDistro "distro" "release"

complexDistro :: Text
complexDistro =
  [r| NAME=Fedora
      VERSION="17 (Beefy Miracle)"
      ID=fedora
      VERSION_ID=17
      PRETTY_NAME="Fedora 17 (Beefy Miracle)"
      ANSI_COLOR="0;34"
      CPE_NAME="cpe:/o:fedoraproject:fedora:17"
      HOME_URL="https://fedoraproject.org/"
      BUG_REPORT_URL="https://bugzilla.redhat.com/"
  |]

expectedComplexDistro :: LinuxDistro
expectedComplexDistro = LinuxDistro "fedora" "17"
