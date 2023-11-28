module BerkeleyDB.BerkeleyDBSpec (spec) where

import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Stack (runStack)
import Effect.Exec (runExecIO)
import Effect.Logger (Severity (SevError), withDefaultLogger)
import Effect.ReadFS (runReadFSIO)
import Path (Abs, File, Path)
import Path.IO qualified as PIO
import ResultUtil (assertOnSuccess)
import Strategy.BerkeleyDB.Internal (BdbEntry (..), readBerkeleyDB)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

spec :: Spec
spec = do
  describe "Container BerkeleyDB Parser" $ do
    target <- runIO testPackagesFile
    result <- runIO . runStack . runDiagnostics . runExecIO . runReadFSIO . withDefaultLogger SevError $ readBerkeleyDB target

    it "parses berkeleydb contents" $
      assertOnSuccess result $ \_ c ->
        c `shouldBe` expectedEntries

    missingEntriesTarget <- runIO testMissingEntriesPackagesFile
    missingEntriesResult <- runIO . runStack . runDiagnostics . runExecIO . runReadFSIO . withDefaultLogger SevError $ readBerkeleyDB missingEntriesTarget

    it "parses berkelydb contents with missing package attributes" $
      assertOnSuccess missingEntriesResult $ \_ c ->
        c `shouldBe` expectedMissingEntries

-- | 'extlib/berkeleydb/tests' contains a more complete set of tests for parsing various container formats.
-- The goal of this test is to prove end-to-end flow; for that reason, we just test with the 'CentOS5 Plain' package list
-- from https://github.com/jssblck/go-rpmdb/blob/9a3bd2ebb923399db2189c5d8963af27368ba2c8/pkg/rpmdb_test.go#L16-L20
testPackagesFile :: IO (Path Abs File)
testPackagesFile = PIO.resolveFile' "extlib/berkeleydb/testdata/centos5-plain/Packages"

testMissingEntriesPackagesFile :: IO (Path Abs File)
testMissingEntriesPackagesFile = PIO.resolveFile' "extlib/berkeleydb/testdata/sles12sp4/Packages"

-- docker run --rm -it centos:5 bash
-- rpm -qa --queryformat "BdbEntry \"%{ARCH}\"\ \"%{NAME}\" \"%{VERSION}-%{RELEASE}\" (Just \"%{EPOCH}\"),\n" | sed 's/(Just "(none)")/Nothing/g' | sed "s/(none)/0/g"
--
-- '{Version}-{Release}' is because of the way that RPM dependencies are formed; we need both and they're meant to be separated by a dash.
-- Reference: https://github.com/fossas/fossa-cli/blob/f25758ce3af7dd2992edd4cdd19ff84edd9b6c86/src/App/Fossa/VSI/DynLinked/Internal/Lookup/RPM.hs#L111-L118,
-- which creates well-formed RPM entries that Core can resolve.
--
-- Core's linux locator documentation (https://github.com/fossas/FOSSA/blob/e61713dec1ef80dc6b6114f79622c14df5278235/modules/fetchers/README.md#locators-for-linux-packages)
-- does not explicitly state this, although it is in their examples and I swear I read it somewhere that I can no longer find.
expectedEntries :: [BdbEntry]
expectedEntries =
  [ BdbEntry "noarch" "setup" "2.5.58-9.el5" Nothing
  , BdbEntry "noarch" "basesystem" "8.0-5.1.1.el5.centos" Nothing
  , BdbEntry "x86_64" "cracklib-dicts" "2.8.9-3.3" Nothing
  , BdbEntry "x86_64" "tzdata" "2016c-1.el5" Nothing
  , BdbEntry "x86_64" "glibc" "2.5-123.el5_11.3" Nothing
  , BdbEntry "x86_64" "zlib" "1.2.3-7.el5" Nothing
  , BdbEntry "x86_64" "popt" "1.10.2.3-36.el5_11" Nothing
  , BdbEntry "x86_64" "glib2" "2.12.3-4.el5_3.1" Nothing
  , BdbEntry "x86_64" "audit-libs" "1.8-2.el5" Nothing
  , BdbEntry "x86_64" "bash" "3.2-33.el5_11.4" Nothing
  , BdbEntry "x86_64" "info" "4.8-14.el5" Nothing
  , BdbEntry "x86_64" "readline" "5.1-3.el5" Nothing
  , BdbEntry "x86_64" "nss" "3.19.1-4.el5_11" Nothing
  , BdbEntry "x86_64" "elfutils-libelf" "0.137-3.el5" Nothing
  , BdbEntry "x86_64" "libattr" "2.4.32-1.1" Nothing
  , BdbEntry "x86_64" "libstdc++" "4.1.2-55.el5" Nothing
  , BdbEntry "x86_64" "iproute" "2.6.18-15.el5" Nothing
  , BdbEntry "x86_64" "grep" "2.5.1-55.el5" Nothing
  , BdbEntry "x86_64" "diffutils" "2.8.1-16.el5" Nothing
  , BdbEntry "x86_64" "gawk" "3.1.5-16.el5" Nothing
  , BdbEntry "x86_64" "less" "436-9.el5" Nothing
  , BdbEntry "x86_64" "procps" "3.2.7-26.el5" Nothing
  , BdbEntry "noarch" "crontabs" "1.10-11.el5" Nothing
  , BdbEntry "x86_64" "libxml2" "2.6.26-2.1.25.el5_11" Nothing
  , BdbEntry "x86_64" "sgpio" "1.2.0_10-2.el5" Nothing
  , BdbEntry "x86_64" "mingetty" "1.07-5.2.2" Nothing
  , BdbEntry "x86_64" "libcap" "1.10-26" Nothing
  , BdbEntry "x86_64" "keyutils-libs" "1.2-1.el5" Nothing
  , BdbEntry "x86_64" "centos-release" "5-11.el5.centos" (Just "10")
  , BdbEntry "x86_64" "python-libs" "2.4.3-56.el5" Nothing
  , BdbEntry "x86_64" "cracklib" "2.8.9-3.3" Nothing
  , BdbEntry "x86_64" "device-mapper-event" "1.02.67-2.el5_11.1" Nothing
  , BdbEntry "x86_64" "net-tools" "1.60-83.el5_10" Nothing
  , BdbEntry "x86_64" "libutempter" "1.1.4-4.el5" Nothing
  , BdbEntry "x86_64" "tar" "1.15.1-32.el5_8" (Just "2")
  , BdbEntry "x86_64" "SysVinit" "2.86-17.el5" Nothing
  , BdbEntry "x86_64" "e2fsprogs" "1.39-37.el5" Nothing
  , BdbEntry "x86_64" "kpartx" "0.4.7-64.el5_11" Nothing
  , BdbEntry "x86_64" "device-mapper-multipath" "0.4.7-64.el5_11" Nothing
  , BdbEntry "x86_64" "logrotate" "3.7.4-14" Nothing
  , BdbEntry "x86_64" "MAKEDEV" "3.23-1.2" Nothing
  , BdbEntry "x86_64" "coreutils" "5.97-34.el5_8.1" Nothing
  , BdbEntry "x86_64" "udev" "095-14.33.el5_11" Nothing
  , BdbEntry "x86_64" "module-init-tools" "3.3-0.pre3.1.63.el5" Nothing
  , BdbEntry "x86_64" "mcstrans" "0.2.11-3.el5" Nothing
  , BdbEntry "x86_64" "initscripts" "8.45.45-1.el5.centos" Nothing
  , BdbEntry "x86_64" "rpm-libs" "4.4.2.3-36.el5_11" Nothing
  , BdbEntry "x86_64" "bind-libs" "9.3.6-25.P1.el5_11.8" (Just "30")
  , BdbEntry "x86_64" "python-elementtree" "1.2.6-5" Nothing
  , BdbEntry "x86_64" "m2crypto" "0.16-9.el5" Nothing
  , BdbEntry "x86_64" "yum-metadata-parser" "1.1.2-4.el5" Nothing
  , BdbEntry "noarch" "yum" "3.2.22-40.el5.centos" Nothing
  , BdbEntry "x86_64" "libuser" "0.54.7-3.el5" Nothing
  , BdbEntry "x86_64" "bind-utils" "9.3.6-25.P1.el5_11.8" (Just "30")
  , BdbEntry "x86_64" "vim-minimal" "7.0.109-7.2.el5" (Just "2")
  , BdbEntry "noarch" "rootfiles" "8.1-1.1.1" Nothing
  , BdbEntry "x86_64" "libgcc" "4.1.2-55.el5" Nothing
  , BdbEntry "x86_64" "filesystem" "2.4.0-3.el5.centos" Nothing
  , BdbEntry "x86_64" "nash" "5.1.19.6-82.el5" Nothing
  , BdbEntry "noarch" "termcap" "5.5-1.20060701.1" (Just "1")
  , BdbEntry "x86_64" "glibc-common" "2.5-123.el5_11.3" Nothing
  , BdbEntry "x86_64" "mktemp" "1.5-24.el5" (Just "3")
  , BdbEntry "x86_64" "chkconfig" "1.3.30.2-2.el5" Nothing
  , BdbEntry "x86_64" "nspr" "4.10.8-2.el5_11" Nothing
  , BdbEntry "x86_64" "bzip2-libs" "1.0.3-6.el5_5" Nothing
  , BdbEntry "x86_64" "libtermcap" "2.0.8-46.1" Nothing
  , BdbEntry "x86_64" "ncurses" "5.5-24.20060715" Nothing
  , BdbEntry "x86_64" "libsepol" "1.15.2-3.el5" Nothing
  , BdbEntry "x86_64" "sqlite" "3.3.6-7" Nothing
  , BdbEntry "x86_64" "sed" "4.1.5-8.el5" Nothing
  , BdbEntry "x86_64" "expat" "1.95.8-11.el5_8" Nothing
  , BdbEntry "x86_64" "libacl" "2.2.39-8.el5" Nothing
  , BdbEntry "x86_64" "db4" "4.3.29-10.el5_5.2" Nothing
  , BdbEntry "x86_64" "pcre" "6.6-9.el5" Nothing
  , BdbEntry "x86_64" "hmaccalc" "0.9.6-4.el5" Nothing
  , BdbEntry "x86_64" "binutils" "2.17.50.0.6-26.el5" Nothing
  , BdbEntry "x86_64" "cpio" "2.6-26.el5" Nothing
  , BdbEntry "x86_64" "gzip" "1.3.5-13.el5.centos" Nothing
  , BdbEntry "x86_64" "iputils" "20020927-46.el5" Nothing
  , BdbEntry "x86_64" "libsysfs" "2.1.0-1.el5" Nothing
  , BdbEntry "x86_64" "cyrus-sasl-lib" "2.1.22-7.el5_8.1" Nothing
  , BdbEntry "x86_64" "gdbm" "1.8.0-28.el5" Nothing
  , BdbEntry "x86_64" "ethtool" "6-4.el5" Nothing
  , BdbEntry "x86_64" "centos-release-notes" "5.11-0" Nothing
  , BdbEntry "x86_64" "openssl" "0.9.8e-39.el5_11" Nothing
  , BdbEntry "x86_64" "python" "2.4.3-56.el5" Nothing
  , BdbEntry "x86_64" "iscsi-initiator-utils" "6.2.0.872-16.el5" Nothing
  , BdbEntry "x86_64" "dmraid-events" "1.0.0.rc13-65.el5" Nothing
  , BdbEntry "x86_64" "shadow-utils" "4.0.17-23.el5" (Just "2")
  , BdbEntry "x86_64" "psmisc" "22.2-11" Nothing
  , BdbEntry "x86_64" "findutils" "4.2.27-6.el5" (Just "1")
  , BdbEntry "x86_64" "e2fsprogs-libs" "1.39-37.el5" Nothing
  , BdbEntry "x86_64" "lvm2" "2.02.88-13.el5" Nothing
  , BdbEntry "x86_64" "dmraid" "1.0.0.rc13-65.el5" Nothing
  , BdbEntry "x86_64" "device-mapper" "1.02.67-2.el5_11.1" Nothing
  , BdbEntry "x86_64" "libselinux" "1.33.4-5.7.el5.centos" Nothing
  , BdbEntry "x86_64" "krb5-libs" "1.6.1-80.el5_11" Nothing
  , BdbEntry "x86_64" "pam" "0.99.6.2-14.el5_11" Nothing
  , BdbEntry "x86_64" "util-linux" "2.13-0.59.el5_8" Nothing
  , BdbEntry "x86_64" "mkinitrd" "5.1.19.6-82.el5" Nothing
  , BdbEntry "x86_64" "rsyslog" "3.22.1-7.el5" Nothing
  , BdbEntry "x86_64" "rpm" "4.4.2.3-36.el5_11" Nothing
  , BdbEntry "x86_64" "rpm-python" "4.4.2.3-36.el5_11" Nothing
  , BdbEntry "noarch" "python-iniparse" "0.2.3-6.el5" Nothing
  , BdbEntry "x86_64" "python-sqlite" "1.1.7-1.2.1" Nothing
  , BdbEntry "noarch" "python-urlgrabber" "3.1.0-6.el5" Nothing
  , BdbEntry "noarch" "yum-fastestmirror" "1.1.16-21.el5.centos" Nothing
  , BdbEntry "x86_64" "openldap" "2.3.43-29.el5_11" Nothing
  , BdbEntry "x86_64" "passwd" "0.73-2" Nothing
  , BdbEntry "x86_64" "libselinux-utils" "1.33.4-5.7.el5.centos" Nothing
  ]

expectedMissingEntries :: [BdbEntry]
expectedMissingEntries =
  [ BdbEntry "x86_64" "filesystem" "13.1-14.15" Nothing
  , BdbEntry "x86_64" "file-magic" "5.22-10.21.1" Nothing
  , BdbEntry "x86_64" "glibc" "2.22-114.22.1" Nothing
  , BdbEntry "x86_64" "libuuid1" "2.29.2-9.20.1" Nothing
  , BdbEntry "x86_64" "libsasl2-3" "2.1.26-14.5.1" Nothing
  , BdbEntry "x86_64" "libaudit1" "2.8.1-10.14.1" Nothing
  , BdbEntry "x86_64" "libblkid1" "2.29.2-9.20.1" Nothing
  , BdbEntry "x86_64" "libfdisk1" "2.29.2-9.20.1" Nothing
  , BdbEntry "x86_64" "libustr-1_0-1" "1.0.4-31.197" Nothing
  , BdbEntry "x86_64" "libgpg-error0" "1.13-1.79" Nothing
  , BdbEntry "x86_64" "libattr1" "2.4.47-3.143" Nothing
  , BdbEntry "x86_64" "libassuan0" "2.1.1-3.217" Nothing
  , BdbEntry "x86_64" "libpth20" "2.0.7-140.1" Nothing
  , BdbEntry "x86_64" "libpcre1" "8.45-8.12.1" Nothing
  , BdbEntry "x86_64" "liblua5_1" "5.1.5-8.3.1" Nothing
  , BdbEntry "x86_64" "libgmp10" "5.1.3-4.3.1" Nothing
  , BdbEntry "x86_64" "libexpat1" "2.1.0-21.28.1" Nothing
  , BdbEntry "x86_64" "libbz2-1" "1.0.6-30.14.1" Nothing
  , BdbEntry "x86_64" "libmagic1" "5.22-10.21.1" Nothing
  , BdbEntry "x86_64" "libksba8" "1.3.0-24.6.1" Nothing
  , BdbEntry "x86_64" "libacl1" "2.2.52-7.3.1" Nothing
  , BdbEntry "x86_64" "libstdc++6" "12.3.0+git1204-1.8.1" Nothing
  , BdbEntry "x86_64" "libebl1" "0.158-7.13.3" Nothing
  , BdbEntry "x86_64" "libncurses5" "5.9-81.1" Nothing
  , BdbEntry "x86_64" "libsepol1" "2.5-3.143" Nothing
  , BdbEntry "x86_64" "libverto1" "0.2.6-3.2.2" Nothing
  , BdbEntry "x86_64" "libxml2-2" "2.9.4-46.62.1" Nothing
  , BdbEntry "x86_64" "libudev1" "228-150.108.2" Nothing
  , BdbEntry "x86_64" "libsemanage1" "2.5-9.3.1" Nothing
  , BdbEntry "x86_64" "krb5" "1.12.5-40.49.1" Nothing
  , BdbEntry "x86_64" "libmodman1" "2.0.1-15.75" Nothing
  , BdbEntry "x86_64" "pinentry" "0.8.3-4.27" Nothing
  , BdbEntry "x86_64" "libmount1" "2.29.2-9.20.1" Nothing
  , BdbEntry "x86_64" "libcurl4" "7.60.0-4.56.1" Nothing
  , BdbEntry "x86_64" "libusb-1_0-0" "1.0.20-5.3" Nothing
  , BdbEntry "x86_64" "cracklib" "2.9.0-8.5.1" Nothing
  , BdbEntry "x86_64" "grep" "2.16-4.6.1" Nothing
  , BdbEntry "x86_64" "cpio" "2.11-36.15.1" Nothing
  , BdbEntry "x86_64" "coreutils" "8.25-13.13.1" Nothing
  , BdbEntry "x86_64" "libusb-0_1-4" "0.1.13-29.13" Nothing
  , BdbEntry "x86_64" "procps" "3.3.9-11.24.1" Nothing
  , BdbEntry "x86_64" "gpg2" "2.0.24-9.11.1" Nothing
  , BdbEntry "x86_64" "permissions" "20170707-3.30.1" Nothing
  , BdbEntry "x86_64" "libzypp" "16.22.7-48.2" Nothing
  , BdbEntry "x86_64" "libutempter0" "1.1.6-5.114" Nothing
  , BdbEntry "x86_64" "util-linux" "2.29.2-9.20.1" Nothing
  , BdbEntry "x86_64" "shadow" "4.2.1-27.22.1" Nothing
  , BdbEntry "x86_64" "libffi4" "5.3.1+r233831-12.1" Nothing
  , BdbEntry "x86_64" "libtasn1-6" "4.9-3.13.1" Nothing
  , BdbEntry "x86_64" "libp11-kit0" "0.20.7-3.6.1" Nothing
  , BdbEntry "x86_64" "p11-kit-tools" "0.20.7-3.6.1" Nothing
  , BdbEntry "x86_64" "container-suseconnect" "2.0.0-1.239" Nothing
  , BdbEntry "noarch" "ca-certificates-mozilla" "2.60-12.40.1" Nothing
  , BdbEntry "x86_64" "sles-release-POOL" "12.4-6.8.2" Nothing
  , BdbEntry "x86_64" "cracklib-dict-small" "2.9.0-8.5.1" Nothing
  , BdbEntry "x86_64" "terminfo-base" "5.9-81.1" Nothing
  , BdbEntry "x86_64" "libz1" "1.2.11-3.9.1" Nothing
  , BdbEntry "x86_64" "libsmartcols1" "2.29.2-9.20.1" Nothing
  , BdbEntry "x86_64" "libcom_err2" "1.43.8-3.17.1" Nothing
  , BdbEntry "x86_64" "libopenssl1_0_0" "1.0.2p-3.78.1" Nothing
  , BdbEntry "x86_64" "libldap-2_4-2" "2.4.41-22.19.1" Nothing
  , BdbEntry "noarch" "kubic-locale-archive" "2.22-4.5.3" Nothing
  , BdbEntry "x86_64" "libpopt0" "1.16-26.128" Nothing
  , BdbEntry "x86_64" "libcap-ng0" "0.7.3-4.125" Nothing
  , BdbEntry "x86_64" "fillup" "1.42-270.64" Nothing
  , BdbEntry "x86_64" "perl-base" "5.18.2-12.23.1" Nothing
  , BdbEntry "x86_64" "libprocps3" "3.3.9-11.24.1" Nothing
  , BdbEntry "x86_64" "liblzma5" "5.0.5-6.7.1" Nothing
  , BdbEntry "x86_64" "libkeyutils1" "1.5.9-5.3.1" Nothing
  , BdbEntry "x86_64" "libgcc_s1" "12.3.0+git1204-1.8.1" Nothing
  , BdbEntry "x86_64" "libcap2" "2.26-14.6.1" Nothing
  , BdbEntry "x86_64" "libadns1" "1.4-103.3.1" Nothing
  , BdbEntry "x86_64" "libssh4" "0.6.3-12.12.1" Nothing
  , BdbEntry "x86_64" "libgcrypt20" "1.6.1-16.83.1" Nothing
  , BdbEntry "noarch" "insserv-compat" "0.1-14.3.1" Nothing
  , BdbEntry "x86_64" "libdw1" "0.158-7.13.3" Nothing
  , BdbEntry "x86_64" "libelf1" "0.158-7.13.3" Nothing
  , BdbEntry "x86_64" "ncurses-utils" "5.9-81.1" Nothing
  , BdbEntry "x86_64" "libselinux1" "2.5-8.79" Nothing
  , BdbEntry "x86_64" "libnghttp2-14" "1.39.2-3.7.1" Nothing
  , BdbEntry "x86_64" "libreadline6" "6.3-83.33.1" Nothing
  , BdbEntry "x86_64" "libsystemd0" "228-150.108.2" Nothing
  , BdbEntry "x86_64" "bash" "4.3-83.33.1" Nothing
  , BdbEntry "x86_64" "libzio1" "1.00-9.188" Nothing
  , BdbEntry "x86_64" "info" "4.13a-37.229" Nothing
  , BdbEntry "x86_64" "diffutils" "3.3-5.40" Nothing
  , BdbEntry "x86_64" "openssl-1_0_0" "1.0.2p-3.78.1" Nothing
  , BdbEntry "x86_64" "libaugeas0" "1.2.0-17.12.1" Nothing
  , BdbEntry "x86_64" "libcrack2" "2.9.0-8.5.1" Nothing
  , BdbEntry "x86_64" "sed" "4.2.2-7.3.1" Nothing
  , BdbEntry "x86_64" "findutils" "4.5.12-7.1" Nothing
  , BdbEntry "x86_64" "libproxy1" "0.4.13-18.3.1" Nothing
  , BdbEntry "noarch" "openssl" "1.0.2p-1.13" Nothing
  , BdbEntry "x86_64" "rpm" "4.11.2-16.26.1" Nothing
  , BdbEntry "x86_64" "dirmngr" "1.1.1-13.1" Nothing
  , BdbEntry "x86_64" "sles-release" "12.4-6.8.2" Nothing
  , BdbEntry "x86_64" "libsolv-tools" "0.6.39-2.39.2" Nothing
  , BdbEntry "x86_64" "zypper" "1.13.64-21.55.2" Nothing
  , BdbEntry "x86_64" "pam" "1.1.8-24.49.1" Nothing
  , BdbEntry "x86_64" "aaa_base" "13.2+git20140911.61c1681-38.22.1" Nothing
  , BdbEntry "noarch" "netcfg" "11.5-29.1" Nothing
  , BdbEntry "noarch" "suse-build-key" "12.0-7.15.1" Nothing
  , BdbEntry "x86_64" "libtasn1" "4.9-3.13.1" Nothing
  , BdbEntry "x86_64" "p11-kit" "0.20.7-3.6.1" Nothing
  , BdbEntry "x86_64" "base-container-licenses" "3.0-1.357" Nothing
  , BdbEntry "noarch" "ca-certificates" "1_201403302107-6.2" Nothing
  ]
