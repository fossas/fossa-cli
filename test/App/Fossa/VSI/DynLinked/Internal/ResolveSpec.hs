{-# LANGUAGE QuasiQuotes #-}

module App.Fossa.VSI.DynLinked.Internal.ResolveSpec (spec) where

import App.Fossa.VSI.DynLinked.Internal.Resolve (readLinuxDistro, toDependency)
import App.Fossa.VSI.DynLinked.Types (LinuxDistro (..), LinuxPackageManager (..), LinuxPackageMetadata (..), ResolvedLinuxPackage (..))
import Control.Carrier.Diagnostics (runDiagnostics)
import Data.Text (Text)
import DepTypes (DepType (..), Dependency (..), VerConstraint (CEq))
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldBe)
import Text.RawString.QQ (r)

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
    simpleResult <- runIO . runDiagnostics $ readLinuxDistro simpleDistro
    complexResult <- runIO . runDiagnostics $ readLinuxDistro complexDistro

    it "parses simple distro" $ case simpleResult of
      Left e -> expectationFailure ("could not parse: " <> show e)
      Right result -> result `shouldBe` expectedSimpleDistro

    it "parses complex distro" $ case complexResult of
      Left e -> expectationFailure ("could not parse: " <> show e)
      Right result -> result `shouldBe` expectedComplexDistro

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
