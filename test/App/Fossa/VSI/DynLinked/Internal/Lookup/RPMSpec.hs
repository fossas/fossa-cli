{-# LANGUAGE QuasiQuotes #-}

module App.Fossa.VSI.DynLinked.Internal.Lookup.RPMSpec (spec) where

import App.Fossa.VSI.DynLinked.Internal.Lookup.RPM (rpmParseQueryPackageInfo)
import App.Fossa.VSI.DynLinked.Types (LinuxPackageMetadata (..))
import Data.Text (Text)
import Data.Void (Void)
import Test.Hspec (Expectation, Spec, describe, it)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (Parsec, parse)
import Text.RawString.QQ (r)

parseMatch :: (Show a, Eq a) => Parsec Void Text a -> Text -> a -> Expectation
parseMatch parser input expected = parse parser "" input `shouldParse` expected

shouldParseInto :: Text -> LinuxPackageMetadata -> Expectation
shouldParseInto = parseMatch rpmParseQueryPackageInfo

spec :: Spec
spec = do
  describe "parse rpm output" $ do
    it "parses rpm output without epoch" $ do
      rpmOutput `shouldParseInto` rpmOutputExpected

    it "parses rpm output with epoch" $ do
      rpmOutputEpoch `shouldParseInto` rpmOutputEpochExpected

rpmOutputExpected :: LinuxPackageMetadata
rpmOutputExpected = LinuxPackageMetadata "glibc" "2.28-151.el8" "x86_64" Nothing

rpmOutput :: Text
rpmOutput =
  [r|Name        : glibc
Version     : 2.28
Release     : 151.el8
Architecture: x86_64
Install Date: Wed Sep 15 14:17:28 2021
Group       : Unspecified
Size        : 15646740
License     : LGPLv2+ and LGPLv2+ with exceptions and GPLv2+ and GPLv2+ with exceptions and BSD and Inner-Net and ISC and Public Domain and GFDL
Signature   : RSA/SHA256, Thu Mar 11 21:46:42 2021, Key ID 05b555b38483c65d
Source RPM  : glibc-2.28-151.el8.src.rpm
Build Date  : Thu Mar 11 20:16:40 2021
Build Host  : x86-01.mbox.centos.org
Relocations : (not relocatable)
Packager    : CentOS Buildsys <bugs@centos.org>
Vendor      : CentOS
URL         : http://www.gnu.org/software/glibc/
Summary     : The GNU libc libraries
Description :
The glibc package contains standard libraries which are used by
multiple programs on the system. In order to save disk space and
memory, as well as to make upgrading easier, common system code is
kept in one place and shared between programs. This particular package
contains the most important sets of shared libraries: the standard C
library and the standard math library. Without these two libraries, a
Linux system will not function.
  |]

rpmOutputEpochExpected :: LinuxPackageMetadata
rpmOutputEpochExpected = LinuxPackageMetadata "device-mapper-libs" "1.02.175-5.el8" "x86_64" (Just "8")

rpmOutputEpoch :: Text
rpmOutputEpoch =
  [r|Name        : device-mapper-libs
Epoch       : 8
Version     : 1.02.175
Release     : 5.el8
Architecture: x86_64
Install Date: Wed Sep 15 14:17:37 2021
Group       : Unspecified
Size        : 415655
License     : LGPLv2
Signature   : RSA/SHA256, Wed Mar 10 22:00:57 2021, Key ID 05b555b38483c65d
Source RPM  : lvm2-2.03.11-5.el8.src.rpm
Build Date  : Wed Mar 10 21:50:25 2021
Build Host  : x86-02.mbox.centos.org
Relocations : (not relocatable)
Packager    : CentOS Buildsys <bugs@centos.org>
Vendor      : CentOS
URL         : http://sourceware.org/lvm2
Summary     : Device-mapper shared library
Description :
This package contains the device-mapper shared library, libdevmapper.
  |]
