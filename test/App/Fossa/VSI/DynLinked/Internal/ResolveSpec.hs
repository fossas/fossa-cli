module App.Fossa.VSI.DynLinked.Internal.ResolveSpec (spec) where

import App.Fossa.VSI.DynLinked.Internal.Resolve (toDependency)
import App.Fossa.VSI.DynLinked.Types (LinuxDistro (..), LinuxPackageManager (..), LinuxPackageMetadata (..), ResolvedLinuxPackage (..))
import Data.Text (Text)
import DepTypes (DepType (..), Dependency (..), VerConstraint (CEq))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "resolves linux dependencies to srclib dependencies" $ do
    it "resolves apk" $ do
      let original = ResolvedLinuxPackage LinuxPackageManagerAPK placeholderDistro placeholderMeta
      let expected = Dependency LinuxAPK "id#distro#release" (Just $ CEq "arch#revision") [] mempty mempty
      toDependency original `shouldBe` expected

    it "resolves debian" $ do
      let original = ResolvedLinuxPackage LinuxPackageManagerDEB placeholderDistro placeholderMeta
      let expected = Dependency LinuxDEB "id#distro#release" (Just $ CEq "arch#revision") [] mempty mempty
      toDependency original `shouldBe` expected

    it "resolves rpm with epoch" $ do
      let original = ResolvedLinuxPackage LinuxPackageManagerRPM placeholderDistro placeholderMeta
      let expected = Dependency LinuxRPM "id#distro#release" (Just $ CEq "arch#epoch:revision") [] mempty mempty
      toDependency original `shouldBe` expected

    it "resolves rpm without epoch" $ do
      let original = ResolvedLinuxPackage LinuxPackageManagerRPM placeholderDistro $ baseMeta Nothing
      let expected = Dependency LinuxRPM "id#distro#release" (Just $ CEq "arch#revision") [] mempty mempty
      toDependency original `shouldBe` expected

placeholderMeta :: LinuxPackageMetadata
placeholderMeta = baseMeta (Just "epoch")

baseMeta :: Maybe Text -> LinuxPackageMetadata
baseMeta = LinuxPackageMetadata "id" "revision" "arch"

placeholderDistro :: LinuxDistro
placeholderDistro = LinuxDistro "distro" "release"
