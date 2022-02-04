{-# LANGUAGE QuasiQuotes #-}

module App.Fossa.VSI.DynLinked.Internal.Lookup.DEBSpec (spec) where

import App.Fossa.VSI.DynLinked.Internal.Lookup.DEB (dpkgParseQueryPackageInfo)
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
shouldParseInto = parseMatch dpkgParseQueryPackageInfo

spec :: Spec
spec = do
  describe "parse dpkg output" $ do
    it "parses dpkg output" $ do
      dpkgOutput `shouldParseInto` dpkgOutputExpected

dpkgOutputExpected :: LinuxPackageMetadata
dpkgOutputExpected = LinuxPackageMetadata "libc6" "2.31-0ubuntu9.2" "amd64" Nothing

dpkgOutput :: Text
dpkgOutput =
  [r|Package: libc6
Status: install ok installed
Priority: optional
Section: libs
Installed-Size: 13246
Maintainer: Ubuntu Developers <ubuntu-devel-discuss@lists.ubuntu.com>
Architecture: amd64
Multi-Arch: same
Source: glibc
Version: 2.31-0ubuntu9.2
Replaces: libc6-amd64
Depends: libgcc-s1, libcrypt1 (>= 1:4.4.10-10ubuntu4)
Recommends: libidn2-0 (>= 2.0.5~)
Suggests: glibc-doc, debconf | debconf-2.0, locales
Breaks: hurd (<< 1:0.9.git20170910-1), iraf-fitsutil (<< 2018.07.06-4), libtirpc1 (<< 0.2.3), locales (<< 2.31), locales-all (<< 2.31), nocache (<< 1.1-1~), nscd (<< 2.31), r-cran-later (<< 0.7.5+dfsg-2), wcc (<< 0.0.2+dfsg-3)
Conflicts: openrc (<< 0.27-2~)
Conffiles:
/etc/ld.so.conf.d/x86_64-linux-gnu.conf d4e7a7b88a71b5ffd9e2644e71a0cfab
Description: GNU C Library: Shared libraries
Contains the standard libraries that are used by nearly all programs on
the system. This package includes shared versions of the standard C library
and the standard math library, as well as many others.
Homepage: https://www.gnu.org/software/libc/libc.html
Original-Maintainer: GNU Libc Maintainers <debian-glibc@lists.debian.org>
Original-Vcs-Browser: https://salsa.debian.org/glibc-team/glibc
Original-Vcs-Git: https://salsa.debian.org/glibc-team/glibc.git  
  |]
