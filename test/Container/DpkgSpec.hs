{-# LANGUAGE QuasiQuotes #-}

module Container.DpkgSpec (spec) where

import Container.Dpkg (DpkgEntry (..), analyzeDpkgEntriesScoped, dpkgEntryParser)
import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Stack (runStack)
import Data.Text (Text)
import Data.Void (Void)
import Effect.ReadFS (runReadFSIO)
import Path (Abs, Dir, Path)
import Path.IO qualified as PIO
import ResultUtil (assertOnSuccess)
import Test.Hspec (Expectation, Spec, describe, it, runIO, shouldBe)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (Parsec, parse, some)
import Text.RawString.QQ (r)

spec :: Spec
spec = do
  describe "Container Dpkg Parser" $ do
    target <- runIO testDataDir
    result <- runIO . runStack . runDiagnostics . runReadFSIO $ analyzeDpkgEntriesScoped target

    it "parses a single simple entry" $
      simpleSingleEntry `shouldParseEntryInto` singleEntryExpected

    it "parses a single realistic entry" $
      singleEntry `shouldParseEntryInto` singleEntryExpected

    it "parses two entries" $
      doubleEntry `shouldParseEntriesInto` doubleEntryExpected

    it "parses var/lib/dpkg/status" $
      assertOnSuccess result $ \_ c ->
        c `shouldBe` expectedEntries

-- | Inside testdata, a file is stored at @var/lib/dpkg/status@.
-- This file is the exact contents of a file at @/var/lib/dpkg/status@ on a default install of @debian:bullseye@.
testDataDir :: IO (Path Abs Dir)
testDataDir = PIO.resolveDir' "test/Container/testdata/"

shouldParseEntryInto :: Text -> DpkgEntry -> Expectation
shouldParseEntryInto = parseMatch dpkgEntryParser

shouldParseEntriesInto :: Text -> [DpkgEntry] -> Expectation
shouldParseEntriesInto = parseMatch $ some dpkgEntryParser

parseMatch :: (Show a, Eq a) => Parsec Void Text a -> Text -> a -> Expectation
parseMatch parser input expected = parse parser "" input `shouldParse` expected

singleEntryExpected :: DpkgEntry
singleEntryExpected =
  DpkgEntry
    { dpkgEntryArch = "all"
    , dpkgEntryPackage = "adduser"
    , dpkgEntryVersion = "3.118"
    }

doubleEntryExpected :: [DpkgEntry]
doubleEntryExpected =
  [ DpkgEntry
      { dpkgEntryArch = "all"
      , dpkgEntryPackage = "adduser"
      , dpkgEntryVersion = "3.118"
      }
  , DpkgEntry
      { dpkgEntryArch = "arm64"
      , dpkgEntryPackage = "apt"
      , dpkgEntryVersion = "2.2.4"
      }
  ]

singleEntry :: Text
singleEntry =
  [r|Package: adduser
Status: install ok installed
Priority: important
Section: admin
Installed-Size: 849
Maintainer: Debian Adduser Developers <adduser@packages.debian.org>
Architecture: all
Multi-Arch: foreign
Version: 3.118
Depends: passwd, debconf (>= 0.5) | debconf-2.0
Suggests: liblocale-gettext-perl, perl
Conffiles:
 /etc/deluser.conf 773fb95e98a27947de4a95abb3d3f2a2
Description: add and remove users and groups
 This package includes the 'adduser' and 'deluser' commands for creating
 and removing users.
 .
  - 'adduser' creates new users and groups and adds existing users to
    existing groups;
  - 'deluser' removes users and groups and removes users from a given
    group.
 .
 Adding users with 'adduser' is much easier than adding them manually.
 Adduser will choose appropriate UID and GID values, create a home
 directory, copy skeletal user configuration, and automate setting
 initial values for the user's password, real name and so on.
 .
 Deluser can back up and remove users' home directories
 and mail spool or all the files they own on the system.
 .
 A custom script can be executed after each of the commands.
|]

simpleSingleEntry :: Text
simpleSingleEntry =
  [r|Package: adduser
Architecture: all
Version: 3.118
|]

doubleEntry :: Text
doubleEntry =
  [r|Package: adduser
Status: install ok installed
Priority: important
Section: admin
Installed-Size: 849
Maintainer: Debian Adduser Developers <adduser@packages.debian.org>
Architecture: all
Multi-Arch: foreign
Version: 3.118
Depends: passwd, debconf (>= 0.5) | debconf-2.0
Suggests: liblocale-gettext-perl, perl
Conffiles:
 /etc/deluser.conf 773fb95e98a27947de4a95abb3d3f2a2
Description: add and remove users and groups
 This package includes the 'adduser' and 'deluser' commands for creating
 and removing users.
 .
  - 'adduser' creates new users and groups and adds existing users to
    existing groups;
  - 'deluser' removes users and groups and removes users from a given
    group.
 .
 Adding users with 'adduser' is much easier than adding them manually.
 Adduser will choose appropriate UID and GID values, create a home
 directory, copy skeletal user configuration, and automate setting
 initial values for the user's password, real name and so on.
 .
 Deluser can back up and remove users' home directories
 and mail spool or all the files they own on the system.
 .
 A custom script can be executed after each of the commands.

Package: apt
Status: install ok installed
Priority: required
Section: admin
Installed-Size: 4181
Maintainer: APT Development Team <deity@lists.debian.org>
Architecture: arm64
Version: 2.2.4
Replaces: apt-transport-https (<< 1.5~alpha4~), apt-utils (<< 1.3~exp2~)
Provides: apt-transport-https (= 2.2.4)
Depends: adduser, gpgv | gpgv2 | gpgv1, libapt-pkg6.0 (>= 2.2.4), debian-archive-keyring, libc6 (>= 2.17), libgcc-s1 (>= 3.0), libgnutls30 (>= 3.7.0), libseccomp2 (>= 2.4.2), libstdc++6 (>= 9), libsystemd0
Recommends: ca-certificates
Suggests: apt-doc, aptitude | synaptic | wajig, dpkg-dev (>= 1.17.2), gnupg | gnupg2 | gnupg1, powermgmt-base
Breaks: apt-transport-https (<< 1.5~alpha4~), apt-utils (<< 1.3~exp2~), aptitude (<< 0.8.10)
Conffiles:
 /etc/apt/apt.conf.d/01autoremove ab6540f7278a05a4b7f9e58afcaa5f46
 /etc/cron.daily/apt-compat 49e9b2cfa17849700d4db735d04244f3
 /etc/kernel/postinst.d/apt-auto-removal 6486b24d4c496e7d6a443178869a019b
 /etc/logrotate.d/apt 179f2ed4f85cbaca12fa3d69c2a4a1c3
Description: commandline package manager
 This package provides commandline tools for searching and
 managing as well as querying information about packages
 as a low-level access to all features of the libapt-pkg library.
 .
 These include:
  * apt-get for retrieval of packages and information about them
    from authenticated sources and for installation, upgrade and
    removal of packages together with their dependencies
  * apt-cache for querying available information about installed
    as well as installable packages
  * apt-cdrom to use removable media as a source for packages
  * apt-config as an interface to the configuration settings
  * apt-key as an interface to manage authentication keys
|]

expectedEntries :: [DpkgEntry]
expectedEntries =
  [ DpkgEntry
      { dpkgEntryPackage = "adduser"
      , dpkgEntryArch = "all"
      , dpkgEntryVersion = "3.118"
      }
  , DpkgEntry
      { dpkgEntryPackage = "apt"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "2.2.4"
      }
  , DpkgEntry
      { dpkgEntryPackage = "base-files"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "11.1+deb11u4"
      }
  , DpkgEntry
      { dpkgEntryPackage = "base-passwd"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "3.5.51"
      }
  , DpkgEntry
      { dpkgEntryPackage = "bash"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "5.1-2+deb11u1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "bsdutils"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1:2.36.1-8+deb11u1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "coreutils"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "8.32-4"
      }
  , DpkgEntry
      { dpkgEntryPackage = "dash"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "0.5.11+git20200708+dd9ef66-5"
      }
  , DpkgEntry
      { dpkgEntryPackage = "debconf"
      , dpkgEntryArch = "all"
      , dpkgEntryVersion = "1.5.77"
      }
  , DpkgEntry
      { dpkgEntryPackage = "debian-archive-keyring"
      , dpkgEntryArch = "all"
      , dpkgEntryVersion = "2021.1.1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "debianutils"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "4.11.2"
      }
  , DpkgEntry
      { dpkgEntryPackage = "diffutils"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1:3.7-5"
      }
  , DpkgEntry
      { dpkgEntryPackage = "dpkg"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1.20.11"
      }
  , DpkgEntry
      { dpkgEntryPackage = "e2fsprogs"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1.46.2-2"
      }
  , DpkgEntry
      { dpkgEntryPackage = "findutils"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "4.8.0-1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "gcc-10-base"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "10.2.1-6"
      }
  , DpkgEntry
      { dpkgEntryPackage = "gcc-9-base"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "9.3.0-22"
      }
  , DpkgEntry
      { dpkgEntryPackage = "gpgv"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "2.2.27-2+deb11u2"
      }
  , DpkgEntry
      { dpkgEntryPackage = "grep"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "3.6-1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "gzip"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1.10-4+deb11u1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "hostname"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "3.23"
      }
  , DpkgEntry
      { dpkgEntryPackage = "init-system-helpers"
      , dpkgEntryArch = "all"
      , dpkgEntryVersion = "1.60"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libacl1"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "2.2.53-10"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libapt-pkg6.0"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "2.2.4"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libattr1"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1:2.4.48-6"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libaudit-common"
      , dpkgEntryArch = "all"
      , dpkgEntryVersion = "1:3.0-2"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libaudit1"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1:3.0-2"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libblkid1"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "2.36.1-8+deb11u1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libbz2-1.0"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1.0.8-4"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libc-bin"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "2.31-13+deb11u3"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libc6"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "2.31-13+deb11u3"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libcap-ng0"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "0.7.9-2.2+b1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libcom-err2"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1.46.2-2"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libcrypt1"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1:4.4.18-4"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libdb5.3"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "5.3.28+dfsg1-0.8"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libdebconfclient0"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "0.260"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libext2fs2"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1.46.2-2"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libffi7"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "3.3-6"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libgcc-s1"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "10.2.1-6"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libgcrypt20"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1.8.7-6"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libgmp10"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "2:6.2.1+dfsg-1+deb11u1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libgnutls30"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "3.7.1-5+deb11u1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libgpg-error0"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1.38-2"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libgssapi-krb5-2"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1.18.3-6+deb11u1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libhogweed6"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "3.7.3-1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libidn2-0"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "2.3.0-5"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libk5crypto3"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1.18.3-6+deb11u1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libkeyutils1"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1.6.1-2"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libkrb5-3"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1.18.3-6+deb11u1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libkrb5support0"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1.18.3-6+deb11u1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "liblz4-1"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1.9.3-2"
      }
  , DpkgEntry
      { dpkgEntryPackage = "liblzma5"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "5.2.5-2.1~deb11u1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libmount1"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "2.36.1-8+deb11u1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libnettle8"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "3.7.3-1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libnsl2"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1.3.0-2"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libp11-kit0"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "0.23.22-1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libpam-modules"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1.4.0-9+deb11u1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libpam-modules-bin"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1.4.0-9+deb11u1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libpam-runtime"
      , dpkgEntryArch = "all"
      , dpkgEntryVersion = "1.4.0-9+deb11u1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libpam0g"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1.4.0-9+deb11u1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libpcre2-8-0"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "10.36-2"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libpcre3"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "2:8.39-13"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libseccomp2"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "2.5.1-1+deb11u1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libselinux1"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "3.1-3"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libsemanage-common"
      , dpkgEntryArch = "all"
      , dpkgEntryVersion = "3.1-1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libsemanage1"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "3.1-1+b2"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libsepol1"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "3.1-1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libsmartcols1"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "2.36.1-8+deb11u1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libss2"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1.46.2-2"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libssl1.1"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1.1.1n-0+deb11u3"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libstdc++6"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "10.2.1-6"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libsystemd0"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "247.3-7"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libtasn1-6"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "4.16.0-2"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libtinfo6"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "6.2+20201114-2"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libtirpc-common"
      , dpkgEntryArch = "all"
      , dpkgEntryVersion = "1.3.1-1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libtirpc3"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1.3.1-1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libudev1"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "247.3-7"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libunistring2"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "0.9.10-4"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libuuid1"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "2.36.1-8+deb11u1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libxxhash0"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "0.8.0-2"
      }
  , DpkgEntry
      { dpkgEntryPackage = "libzstd1"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1.4.8+dfsg-2.1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "login"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1:4.8.1-1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "logsave"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1.46.2-2"
      }
  , DpkgEntry
      { dpkgEntryPackage = "lsb-base"
      , dpkgEntryArch = "all"
      , dpkgEntryVersion = "11.1.0"
      }
  , DpkgEntry
      { dpkgEntryPackage = "mawk"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1.3.4.20200120-2"
      }
  , DpkgEntry
      { dpkgEntryPackage = "mount"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "2.36.1-8+deb11u1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "ncurses-base"
      , dpkgEntryArch = "all"
      , dpkgEntryVersion = "6.2+20201114-2"
      }
  , DpkgEntry
      { dpkgEntryPackage = "ncurses-bin"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "6.2+20201114-2"
      }
  , DpkgEntry
      { dpkgEntryPackage = "passwd"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1:4.8.1-1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "perl-base"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "5.32.1-4+deb11u2"
      }
  , DpkgEntry
      { dpkgEntryPackage = "sed"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "4.7-1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "sysvinit-utils"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "2.96-7+deb11u1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "tar"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1.34+dfsg-1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "tzdata"
      , dpkgEntryArch = "all"
      , dpkgEntryVersion = "2021a-1+deb11u4"
      }
  , DpkgEntry
      { dpkgEntryPackage = "util-linux"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "2.36.1-8+deb11u1"
      }
  , DpkgEntry
      { dpkgEntryPackage = "zlib1g"
      , dpkgEntryArch = "arm64"
      , dpkgEntryVersion = "1:1.2.11.dfsg-2+deb11u1"
      }
  ]
