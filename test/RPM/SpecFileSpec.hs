module RPM.SpecFileSpec (
  spec,
) where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import DepTypes
import GraphUtil
import Strategy.RPM
import Test.Hspec qualified as Test

mkUnVerDep :: Text -> RPMDependency
mkUnVerDep name = RPMDependency name Nothing

mkVerDep :: Text -> VerConstraint -> RPMDependency
mkVerDep name ver = RPMDependency name $ Just ver

boostDep :: RPMDependency
boostDep = mkUnVerDep "boost"

cmakeDep :: RPMDependency
cmakeDep = mkVerDep "cmake" $ CGreaterOrEq "2.8"

jsoncppDep :: RPMDependency
jsoncppDep = mkVerDep "jsoncpp" $ CLessOrEq "0.10.5"

gtestDep :: RPMDependency
gtestDep = mkVerDep "gtest" $ CGreater "6.4"

libeventDep :: RPMDependency
libeventDep = mkVerDep "libevent" $ CLess "9.8.1"

libvirtDep :: RPMDependency
libvirtDep = mkVerDep "libvirt" $ CEq "3.9.0"

opensslDep :: RPMDependency
opensslDep = mkVerDep "openssl" $ CEq "1.23.45"

xzDep :: RPMDependency
xzDep = mkUnVerDep "xz"

libxml2Dep :: RPMDependency
libxml2Dep = mkVerDep "libxml2" $ CGreaterOrEq "5.4.98"

gmockDep :: RPMDependency
gmockDep = mkVerDep "gmock" $ CEq "1.6.0-1.aapps.el7"

tacpDep :: RPMDependency
tacpDep = mkUnVerDep "tacp"

buildDeps :: [RPMDependency]
buildDeps =
  [ boostDep
  , cmakeDep
  , jsoncppDep
  , gtestDep
  , libeventDep
  , libvirtDep
  , opensslDep
  , libxml2Dep
  , xzDep
  , gmockDep
  ]

spec :: Test.Spec
spec = do
  Test.describe "rpm specfile parser" $ do
    contents <- Test.runIO $ TIO.readFile "test/RPM/testdata/simple-test.spec"
    Test.it "should list all BuildRequires and Requires" $ do
      let deps = getSpecDeps contents
      depBuildRequires deps `Test.shouldMatchList` buildDeps
      listToMaybe (depRuntimeRequires deps) `Test.shouldBe` Just tacpDep
  --
  Test.describe "line parser" $
    Test.it "should parse single lines correctly" $
      do
        getTypeFromLine "BuildRequires: xz = 1" `Test.shouldBe` Just (BuildRequires $ mkVerDep "xz" $ CEq "1")
        getTypeFromLine "Requires: xz = 1" `Test.shouldBe` Just (RuntimeRequires $ mkVerDep "xz" $ CEq "1")
  --
  Test.describe "rpm dependency grapher" $
    Test.it "should produce flat RuntimeRequires" $ do
      let deps = Dependencies [boostDep, xzDep] [tacpDep, jsoncppDep]
          gr = buildGraph deps

      expectDeps (map toDependency [tacpDep, jsoncppDep]) gr
      expectDirect (map toDependency [tacpDep, jsoncppDep]) gr
      expectEdges [] gr
