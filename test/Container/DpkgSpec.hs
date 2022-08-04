module Container.DpkgSpec (spec) where

import App.Fossa.VSI.DynLinked.Util (runningLinux)
import Container.Dpkg (DpkgEntry (..), analyzeDpkgEntriesScoped)
import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Stack (runStack)
import Effect.ReadFS (runReadFSIO)
import Path (Abs, Dir, Path)
import Path.IO qualified as PIO
import ResultUtil (assertOnSuccess)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

spec :: Spec
spec = do
  describe "Container Dpkg Parser" $ do
    target <- runIO testDataDir
    result <- runIO . runStack . runDiagnostics . runReadFSIO $ analyzeDpkgEntriesScoped target

    it "parses var/lib/dpkg/status" $
      assertOnSuccess result $ \_ c ->
        c `shouldBe` expectedEntries

-- | Inside testdata, a file is stored at @var/lib/dpkg/status@.
-- This file is the exact contents of a file at @/var/lib/dpkg/status@ on @debian:bullseye@.
testDataDir :: IO (Path Abs Dir)
testDataDir = PIO.resolveDir' "test/Container/testdata/"

expectedEntries :: [DpkgEntry]
expectedEntries =
  if not runningLinux
    then []
    else
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
