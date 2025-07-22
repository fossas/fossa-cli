module Ndb.NdbSpec (spec) where

import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Stack (runStack)
import Data.Foldable (for_)
import Effect.Exec (runExecIO)
import Effect.ReadFS (runReadFSIO)
import Path.IO qualified as PIO
import ResultUtil (assertOnSuccess)
import Strategy.NDB.Internal (NdbEntry (..), readNDB)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

spec :: Spec
spec = do
  describe "RPM NDB parser" $ do
    for_ fixtures $ \(name, dbFilePath, expected) -> do
      path <- runIO $ PIO.resolveFile' $ "test/Ndb/testdata/" <> dbFilePath
      actual <- runIO . runStack . runDiagnostics . runExecIO . runReadFSIO $ readNDB path
      it ("parses fixture from " <> name) $ assertOnSuccess actual $ \_ c -> c `shouldBe` expected

fixtures :: [(String, FilePath, [NdbEntry])]
fixtures =
  [ ("go-rpmdb", "go-rpmdb/sle15-bci/Packages.db", goRpmdb)
  , ("registry.suse.com/bci/bci-base:15.3", "docker/registry.suse.com/bci/bci-base/15.3/Packages.db", bciBaseClean)
  , ("registry.suse.com/bci/bci-base:15.3-dirty", "docker/registry.suse.com/bci/bci-base/15.3-dirty/Packages.db", bciBaseDirty)
  , ("registry.suse.com/bci/bci-minimal:15.3", "docker/registry.suse.com/bci/bci-minimal/15.3/Packages.db", bciMinimal)
  , ("opensuse/leap:15.5", "docker/opensuse/leap/15.5/Packages.db", openSuseLeap)
  ]
  where
    -- This fixture is from: https://github.com/jssblck/go-rpmdb/blob/9a3bd2ebb923399db2189c5d8963af27368ba2c8/pkg/rpmdb_test.go#L72-L74
    -- The expected results are from: https://github.com/jssblck/go-rpmdb/blob/d637bcc368602fb1dbd7a017c58e979266e01cd5/pkg/rpmdb_testcase_test.go#L2985-L3021
    --
    -- Note that we combine '{Version}-{Release}' in the package's "version".
    -- This is for locator construction. See also: https://github.com/fossas/fossa-cli/blob/f25758ce3af7dd2992edd4cdd19ff84edd9b6c86/src/App/Fossa/VSI/DynLinked/Internal/Lookup/RPM.hs#L111-L118,
    --
    -- Core's linux locator documentation (https://github.com/fossas/FOSSA/blob/e61713dec1ef80dc6b6114f79622c14df5278235/modules/fetchers/README.md#locators-for-linux-packages)
    -- does not explicitly state this, although it is in their examples and I swear I read it somewhere that I can no longer find.
    goRpmdb =
      [ NdbEntry "noarch" "system-user-root" "20190513-3.3.1" Nothing Nothing
      , NdbEntry "x86_64" "filesystem" "15.0-11.3.2" Nothing Nothing
      , NdbEntry "x86_64" "glibc" "2.31-9.3.2" Nothing Nothing
      , NdbEntry "x86_64" "libpcre1" "8.45-20.10.1" Nothing Nothing
      , NdbEntry "x86_64" "libgmp10" "6.1.2-4.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libgcc_s1" "11.2.1+git610-1.3.9" Nothing Nothing
      , NdbEntry "x86_64" "libcap2" "2.26-4.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libstdc++6" "11.2.1+git610-1.3.9" Nothing Nothing
      , NdbEntry "x86_64" "libncurses6" "6.1-5.9.1" Nothing Nothing
      , NdbEntry "x86_64" "terminfo-base" "6.1-5.9.1" Nothing Nothing
      , NdbEntry "x86_64" "libattr1" "2.4.47-2.19" Nothing Nothing
      , NdbEntry "x86_64" "libselinux1" "3.0-1.31" Nothing Nothing
      , NdbEntry "x86_64" "libreadline7" "7.0-19.6.1" Nothing Nothing
      , NdbEntry "x86_64" "bash" "4.4-19.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libacl1" "2.2.52-4.3.1" Nothing Nothing
      , NdbEntry "x86_64" "coreutils" "8.32-3.2.1" Nothing Nothing
      , NdbEntry "x86_64" "sles-release" "15.3-55.4.1" Nothing Nothing
      , NdbEntry "noarch" "ca-certificates-mozilla-prebuilt" "2.44-21.1" Nothing Nothing
      , NdbEntry "x86_64" "libgpg-error0" "1.29-1.8" Nothing Nothing
      , NdbEntry "x86_64" "libpopt0" "1.16-3.22" Nothing Nothing
      , NdbEntry "noarch" "file-magic" "5.32-7.14.1" Nothing Nothing
      , NdbEntry "x86_64" "libbz2-1" "1.0.6-5.11.1" Nothing Nothing
      , NdbEntry "x86_64" "liblua5_3-5" "5.3.6-3.6.1" Nothing Nothing
      , NdbEntry "x86_64" "liblzma5" "5.2.3-4.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libz1" "1.2.11-3.21.1" Nothing Nothing
      , NdbEntry "x86_64" "libzstd1" "1.4.4-1.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libmagic1" "5.32-7.14.1" Nothing Nothing
      , NdbEntry "x86_64" "libdw1" "0.168-4.5.3" Nothing Nothing
      , NdbEntry "x86_64" "libebl-plugins" "0.168-4.5.3" Nothing Nothing
      , NdbEntry "x86_64" "libelf1" "0.168-4.5.3" Nothing Nothing
      , NdbEntry "x86_64" "libcrypt1" "4.4.15-2.51" Nothing Nothing
      , NdbEntry "x86_64" "perl-base" "5.26.1-15.87" Nothing Nothing
      , NdbEntry "x86_64" "libgcrypt20" "1.8.2-8.39.1" Nothing Nothing
      , NdbEntry "noarch" "rpm-config-SUSE" "1-5.6.1" Nothing Nothing
      , NdbEntry "x86_64" "rpm-ndb" "4.14.3-40.1" Nothing Nothing
      ]

    -- This fixture is pulled from a container image. To generate this fixture:
    --
    --     docker run -it --rm --name fixture registry.suse.com/bci/bci-base:15.3
    --
    -- To get the database file, run this from a separate terminal:
    --
    --     docker cp fixture:/var/lib/rpm/Packages.db ./test/Ndb/testdata/docker/registry.suse.com/bci/bci-base/15.3/Packages.db
    --
    -- To get the list of expected installed packages, run this from inside the
    -- container:
    --
    --     rpm -qa --queryformat ', NdbEntry "%{ARCH}" "%{NAME}" "%{VERSION}-%{RELEASE}" Nothing\n' | grep -v '(none)'
    bciBaseClean =
      [ NdbEntry "noarch" "file-magic" "5.32-7.14.1" Nothing Nothing
      , NdbEntry "noarch" "system-user-root" "20190513-3.3.1" Nothing Nothing
      , NdbEntry "x86_64" "filesystem" "15.0-11.8.1" Nothing Nothing
      , NdbEntry "noarch" "boost-license1_66_0" "1.66.0-12.3.1" Nothing Nothing
      , NdbEntry "x86_64" "cracklib-dict-small" "2.9.7-11.6.1" Nothing Nothing
      , NdbEntry "noarch" "libldap-data" "2.4.46-150200.14.17.1" Nothing Nothing
      , NdbEntry "x86_64" "libtirpc-netconfig" "1.2.6-150300.3.17.1" Nothing Nothing
      , NdbEntry "x86_64" "glibc" "2.31-150300.52.2" Nothing Nothing
      , NdbEntry "x86_64" "libuuid1" "2.36.2-150300.4.35.1" Nothing Nothing
      , NdbEntry "x86_64" "libsmartcols1" "2.36.2-150300.4.35.1" Nothing Nothing
      , NdbEntry "x86_64" "libsasl2-3" "2.1.27-150300.4.6.1" Nothing Nothing
      , NdbEntry "x86_64" "liblz4-1" "1.9.2-3.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libgpg-error0" "1.42-150300.9.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libeconf0" "0.4.4+git20220104.962774f-150300.3.8.1" Nothing Nothing
      , NdbEntry "x86_64" "libcrypt1" "4.4.15-150300.4.4.3" Nothing Nothing
      , NdbEntry "x86_64" "libblkid1" "2.36.2-150300.4.35.1" Nothing Nothing
      , NdbEntry "x86_64" "perl-base" "5.26.1-150300.17.14.1" Nothing Nothing
      , NdbEntry "x86_64" "libfdisk1" "2.36.2-150300.4.35.1" Nothing Nothing
      , NdbEntry "x86_64" "libnghttp2-14" "1.40.0-6.1" Nothing Nothing
      , NdbEntry "x86_64" "libsepol1" "3.0-1.31" Nothing Nothing
      , NdbEntry "x86_64" "libcap-ng0" "0.7.9-4.37" Nothing Nothing
      , NdbEntry "x86_64" "libunistring2" "0.9.10-1.1" Nothing Nothing
      , NdbEntry "x86_64" "libaudit1" "2.8.5-3.43" Nothing Nothing
      , NdbEntry "noarch" "kubic-locale-archive" "2.31-10.36" Nothing Nothing
      , NdbEntry "x86_64" "libzstd1" "1.4.4-150000.1.9.1" Nothing Nothing
      , NdbEntry "x86_64" "libz1" "1.2.11-150000.3.45.1" Nothing Nothing
      , NdbEntry "x86_64" "libsqlite3-0" "3.39.3-150000.3.20.1" Nothing Nothing
      , NdbEntry "x86_64" "libpcre1" "8.45-150000.20.13.1" Nothing Nothing
      , NdbEntry "x86_64" "liblzma5" "5.2.3-150000.4.7.1" Nothing Nothing
      , NdbEntry "x86_64" "liblua5_3-5" "5.3.6-3.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libkeyutils1" "1.6.3-5.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libgmp10" "6.1.2-4.9.1" Nothing Nothing
      , NdbEntry "x86_64" "libgcc_s1" "12.3.0+git1204-150000.1.10.1" Nothing Nothing
      , NdbEntry "x86_64" "libcom_err2" "1.43.8-150000.4.33.1" Nothing Nothing
      , NdbEntry "x86_64" "libcap2" "2.26-150000.4.9.1" Nothing Nothing
      , NdbEntry "x86_64" "libbz2-1" "1.0.6-5.11.1" Nothing Nothing
      , NdbEntry "x86_64" "libksba8" "1.3.5-150000.4.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libassuan0" "2.5.5-150000.4.5.2" Nothing Nothing
      , NdbEntry "x86_64" "libidn2-0" "2.2.0-3.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libmagic1" "5.32-7.14.1" Nothing Nothing
      , NdbEntry "x86_64" "libxml2-2" "2.9.7-150000.3.57.1" Nothing Nothing
      , NdbEntry "x86_64" "libstdc++6" "12.3.0+git1204-150000.1.10.1" Nothing Nothing
      , NdbEntry "x86_64" "libpsl5" "0.20.1-150000.3.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libaugeas0" "1.10.1-150000.3.12.1" Nothing Nothing
      , NdbEntry "x86_64" "libyaml-cpp0_6" "0.6.1-4.5.1" Nothing Nothing
      , NdbEntry "x86_64" "libncurses6" "6.1-150000.5.15.1" Nothing Nothing
      , NdbEntry "x86_64" "terminfo-base" "6.1-150000.5.15.1" Nothing Nothing
      , NdbEntry "x86_64" "ncurses-utils" "6.1-150000.5.15.1" Nothing Nothing
      , NdbEntry "x86_64" "libverto1" "0.2.6-3.20" Nothing Nothing
      , NdbEntry "x86_64" "libpopt0" "1.16-3.22" Nothing Nothing
      , NdbEntry "x86_64" "libnpth0" "1.5-2.11" Nothing Nothing
      , NdbEntry "x86_64" "libattr1" "2.4.47-2.19" Nothing Nothing
      , NdbEntry "x86_64" "fillup" "1.42-2.18" Nothing Nothing
      , NdbEntry "x86_64" "libzio1" "1.06-2.20" Nothing Nothing
      , NdbEntry "x86_64" "libmodman1" "2.0.1-1.27" Nothing Nothing
      , NdbEntry "x86_64" "libgcrypt20" "1.8.2-8.42.1" Nothing Nothing
      , NdbEntry "x86_64" "libgcrypt20-hmac" "1.8.2-8.42.1" Nothing Nothing
      , NdbEntry "x86_64" "libopenssl1_1" "1.1.1d-150200.11.75.1" Nothing Nothing
      , NdbEntry "x86_64" "libopenssl1_1-hmac" "1.1.1d-150200.11.75.1" Nothing Nothing
      , NdbEntry "x86_64" "libglib-2_0-0" "2.62.6-150200.3.15.1" Nothing Nothing
      , NdbEntry "x86_64" "libprotobuf-lite20" "3.9.2-150200.4.21.1" Nothing Nothing
      , NdbEntry "x86_64" "libboost_system1_66_0" "1.66.0-12.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libldap-2_4-2" "2.4.46-150200.14.17.1" Nothing Nothing
      , NdbEntry "x86_64" "libboost_thread1_66_0" "1.66.0-12.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libselinux1" "3.0-1.31" Nothing Nothing
      , NdbEntry "x86_64" "libsigc-2_0-0" "2.10.2-1.18" Nothing Nothing
      , NdbEntry "x86_64" "libsemanage1" "3.0-1.27" Nothing Nothing
      , NdbEntry "x86_64" "libdw1" "0.177-150300.11.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libelf1" "0.177-150300.11.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libebl-plugins" "0.177-150300.11.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libreadline7" "7.0-19.6.1" Nothing Nothing
      , NdbEntry "x86_64" "patterns-base-fips" "20200124-10.5.1" Nothing Nothing
      , NdbEntry "x86_64" "libudev1" "246.16-150300.7.57.1" Nothing Nothing
      , NdbEntry "x86_64" "libsystemd0" "246.16-150300.7.57.1" Nothing Nothing
      , NdbEntry "x86_64" "libmount1" "2.36.2-150300.4.35.1" Nothing Nothing
      , NdbEntry "x86_64" "krb5" "1.19.2-150300.10.1" Nothing Nothing
      , NdbEntry "x86_64" "bash" "4.4-19.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libtirpc3" "1.2.6-150300.3.17.1" Nothing Nothing
      , NdbEntry "noarch" "login_defs" "4.8.1-150300.4.9.1" Nothing Nothing
      , NdbEntry "x86_64" "libacl1" "2.2.52-4.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libusb-1_0-0" "1.0.21-150000.3.5.1" Nothing Nothing
      , NdbEntry "x86_64" "libprocps7" "3.3.15-150000.7.31.1" Nothing Nothing
      , NdbEntry "x86_64" "procps" "3.3.15-150000.7.31.1" Nothing Nothing
      , NdbEntry "x86_64" "libproxy1" "0.4.15-12.41" Nothing Nothing
      , NdbEntry "x86_64" "findutils" "4.8.0-1.20" Nothing Nothing
      , NdbEntry "x86_64" "libssh4" "0.8.7-10.12.1" Nothing Nothing
      , NdbEntry "x86_64" "libcrack2" "2.9.7-11.6.1" Nothing Nothing
      , NdbEntry "x86_64" "cracklib" "2.9.7-11.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libcurl4" "7.66.0-150200.4.57.1" Nothing Nothing
      , NdbEntry "x86_64" "info" "6.5-4.17" Nothing Nothing
      , NdbEntry "x86_64" "libnsl2" "1.2.0-2.44" Nothing Nothing
      , NdbEntry "x86_64" "coreutils" "8.32-150300.3.5.1" Nothing Nothing
      , NdbEntry "x86_64" "sles-release" "15.3-55.4.1" Nothing Nothing
      , NdbEntry "x86_64" "sed" "4.4-11.6" Nothing Nothing
      , NdbEntry "x86_64" "pinentry" "1.1.0-4.3.1" Nothing Nothing
      , NdbEntry "x86_64" "grep" "3.1-150000.4.6.1" Nothing Nothing
      , NdbEntry "x86_64" "diffutils" "3.6-4.3.1" Nothing Nothing
      , NdbEntry "x86_64" "cpio" "2.12-3.9.1" Nothing Nothing
      , NdbEntry "x86_64" "gpg2" "2.2.27-150300.3.5.1" Nothing Nothing
      , NdbEntry "noarch" "rpm-config-SUSE" "1-5.6.1" Nothing Nothing
      , NdbEntry "x86_64" "rpm-ndb" "4.14.3-150300.55.1" Nothing Nothing
      , NdbEntry "x86_64" "permissions" "20181225-150200.23.23.1" Nothing Nothing
      , NdbEntry "x86_64" "libgpgme11" "1.13.1-4.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libsolv-tools" "0.7.24-150200.20.2" Nothing Nothing
      , NdbEntry "x86_64" "libzypp" "17.31.14-150200.70.1" Nothing Nothing
      , NdbEntry "x86_64" "zypper" "1.14.61-150200.54.1" Nothing Nothing
      , NdbEntry "x86_64" "pam" "1.3.0-150000.6.61.1" Nothing Nothing
      , NdbEntry "x86_64" "shadow" "4.8.1-150300.4.9.1" Nothing Nothing
      , NdbEntry "noarch" "sysuser-shadow" "2.0-4.2.8" Nothing Nothing
      , NdbEntry "noarch" "system-group-hardware" "20170617-17.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libutempter0" "1.1.6-3.42" Nothing Nothing
      , NdbEntry "x86_64" "util-linux" "2.36.2-150300.4.35.1" Nothing Nothing
      , NdbEntry "x86_64" "aaa_base" "84.87+git20180409.04c9dae-150300.10.3.1" Nothing Nothing
      , NdbEntry "x86_64" "container-suseconnect" "2.4.0-150000.4.34.1" Nothing Nothing
      , NdbEntry "x86_64" "libtasn1-6" "4.13-150000.4.8.1" Nothing Nothing
      , NdbEntry "x86_64" "libtasn1" "4.13-150000.4.8.1" Nothing Nothing
      , NdbEntry "noarch" "netcfg" "11.6-3.3.1" Nothing Nothing
      , NdbEntry "noarch" "suse-build-key" "12.0-150000.8.31.1" Nothing Nothing
      , NdbEntry "x86_64" "timezone" "2023c-150000.75.23.1" Nothing Nothing
      , NdbEntry "x86_64" "libffi7" "3.2.1.git259-10.8" Nothing Nothing
      , NdbEntry "x86_64" "curl" "7.66.0-150200.4.57.1" Nothing Nothing
      , NdbEntry "x86_64" "openssl-1_1" "1.1.1d-150200.11.75.1" Nothing Nothing
      , NdbEntry "x86_64" "skelcd-EULA-bci" "2021.05.14-150300.4.8.1" Nothing Nothing
      , NdbEntry "x86_64" "libp11-kit0" "0.23.2-150000.4.16.1" Nothing Nothing
      , NdbEntry "x86_64" "p11-kit" "0.23.2-150000.4.16.1" Nothing Nothing
      , NdbEntry "x86_64" "p11-kit-tools" "0.23.2-150000.4.16.1" Nothing Nothing
      , NdbEntry "noarch" "ca-certificates" "2+git20210309.21162a6-2.1" Nothing Nothing
      , NdbEntry "noarch" "ca-certificates-mozilla" "2.60-150200.27.1" Nothing Nothing
      ]

    -- This fixture is generated like bciBaseClean, but inside the container,
    -- run these commands before copying out the database to 15.3-dirty:
    --
    --     zypper install less
    --     zypper update
    --     zypper rm sed
    --     zypper install nodejs
    --     zypper install sed
    --
    -- This fixture contains some more weirdness (empty slots, block gaps, etc.)
    -- that only occur after usage of the database.
    bciBaseDirty =
      [ NdbEntry "noarch" "file-magic" "5.32-7.14.1" Nothing Nothing
      , NdbEntry "noarch" "system-user-root" "20190513-3.3.1" Nothing Nothing
      , NdbEntry "x86_64" "filesystem" "15.0-11.8.1" Nothing Nothing
      , NdbEntry "noarch" "boost-license1_66_0" "1.66.0-12.3.1" Nothing Nothing
      , NdbEntry "x86_64" "cracklib-dict-small" "2.9.7-11.6.1" Nothing Nothing
      , NdbEntry "noarch" "libldap-data" "2.4.46-150200.14.17.1" Nothing Nothing
      , NdbEntry "x86_64" "libtirpc-netconfig" "1.2.6-150300.3.17.1" Nothing Nothing
      , NdbEntry "x86_64" "glibc" "2.31-150300.52.2" Nothing Nothing
      , NdbEntry "x86_64" "libuuid1" "2.36.2-150300.4.35.1" Nothing Nothing
      , NdbEntry "x86_64" "libsmartcols1" "2.36.2-150300.4.35.1" Nothing Nothing
      , NdbEntry "x86_64" "libsasl2-3" "2.1.27-150300.4.6.1" Nothing Nothing
      , NdbEntry "x86_64" "liblz4-1" "1.9.2-3.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libgpg-error0" "1.42-150300.9.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libeconf0" "0.4.4+git20220104.962774f-150300.3.8.1" Nothing Nothing
      , NdbEntry "x86_64" "libcrypt1" "4.4.15-150300.4.4.3" Nothing Nothing
      , NdbEntry "x86_64" "libblkid1" "2.36.2-150300.4.35.1" Nothing Nothing
      , NdbEntry "x86_64" "perl-base" "5.26.1-150300.17.14.1" Nothing Nothing
      , NdbEntry "x86_64" "libfdisk1" "2.36.2-150300.4.35.1" Nothing Nothing
      , NdbEntry "x86_64" "libnghttp2-14" "1.40.0-6.1" Nothing Nothing
      , NdbEntry "x86_64" "libsepol1" "3.0-1.31" Nothing Nothing
      , NdbEntry "x86_64" "libcap-ng0" "0.7.9-4.37" Nothing Nothing
      , NdbEntry "x86_64" "libunistring2" "0.9.10-1.1" Nothing Nothing
      , NdbEntry "x86_64" "libaudit1" "2.8.5-3.43" Nothing Nothing
      , NdbEntry "noarch" "kubic-locale-archive" "2.31-10.36" Nothing Nothing
      , NdbEntry "x86_64" "libzstd1" "1.4.4-150000.1.9.1" Nothing Nothing
      , NdbEntry "x86_64" "libz1" "1.2.11-150000.3.45.1" Nothing Nothing
      , NdbEntry "x86_64" "libsqlite3-0" "3.39.3-150000.3.20.1" Nothing Nothing
      , NdbEntry "x86_64" "libpcre1" "8.45-150000.20.13.1" Nothing Nothing
      , NdbEntry "x86_64" "liblzma5" "5.2.3-150000.4.7.1" Nothing Nothing
      , NdbEntry "x86_64" "liblua5_3-5" "5.3.6-3.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libkeyutils1" "1.6.3-5.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libgmp10" "6.1.2-4.9.1" Nothing Nothing
      , NdbEntry "x86_64" "libgcc_s1" "12.3.0+git1204-150000.1.10.1" Nothing Nothing
      , NdbEntry "x86_64" "libcom_err2" "1.43.8-150000.4.33.1" Nothing Nothing
      , NdbEntry "x86_64" "libcap2" "2.26-150000.4.9.1" Nothing Nothing
      , NdbEntry "x86_64" "libbz2-1" "1.0.6-5.11.1" Nothing Nothing
      , NdbEntry "x86_64" "libksba8" "1.3.5-150000.4.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libassuan0" "2.5.5-150000.4.5.2" Nothing Nothing
      , NdbEntry "x86_64" "libidn2-0" "2.2.0-3.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libmagic1" "5.32-7.14.1" Nothing Nothing
      , NdbEntry "x86_64" "libxml2-2" "2.9.7-150000.3.57.1" Nothing Nothing
      , NdbEntry "x86_64" "libstdc++6" "12.3.0+git1204-150000.1.10.1" Nothing Nothing
      , NdbEntry "x86_64" "libpsl5" "0.20.1-150000.3.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libaugeas0" "1.10.1-150000.3.12.1" Nothing Nothing
      , NdbEntry "x86_64" "libyaml-cpp0_6" "0.6.1-4.5.1" Nothing Nothing
      , NdbEntry "x86_64" "libncurses6" "6.1-150000.5.15.1" Nothing Nothing
      , NdbEntry "x86_64" "terminfo-base" "6.1-150000.5.15.1" Nothing Nothing
      , NdbEntry "x86_64" "ncurses-utils" "6.1-150000.5.15.1" Nothing Nothing
      , NdbEntry "x86_64" "libverto1" "0.2.6-3.20" Nothing Nothing
      , NdbEntry "x86_64" "libpopt0" "1.16-3.22" Nothing Nothing
      , NdbEntry "x86_64" "libnpth0" "1.5-2.11" Nothing Nothing
      , NdbEntry "x86_64" "libattr1" "2.4.47-2.19" Nothing Nothing
      , NdbEntry "x86_64" "fillup" "1.42-2.18" Nothing Nothing
      , NdbEntry "x86_64" "libzio1" "1.06-2.20" Nothing Nothing
      , NdbEntry "x86_64" "libmodman1" "2.0.1-1.27" Nothing Nothing
      , NdbEntry "x86_64" "libgcrypt20" "1.8.2-8.42.1" Nothing Nothing
      , NdbEntry "x86_64" "libgcrypt20-hmac" "1.8.2-8.42.1" Nothing Nothing
      , NdbEntry "x86_64" "libopenssl1_1" "1.1.1d-150200.11.75.1" Nothing Nothing
      , NdbEntry "x86_64" "libopenssl1_1-hmac" "1.1.1d-150200.11.75.1" Nothing Nothing
      , NdbEntry "x86_64" "libglib-2_0-0" "2.62.6-150200.3.15.1" Nothing Nothing
      , NdbEntry "x86_64" "libprotobuf-lite20" "3.9.2-150200.4.21.1" Nothing Nothing
      , NdbEntry "x86_64" "libboost_system1_66_0" "1.66.0-12.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libldap-2_4-2" "2.4.46-150200.14.17.1" Nothing Nothing
      , NdbEntry "x86_64" "libboost_thread1_66_0" "1.66.0-12.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libselinux1" "3.0-1.31" Nothing Nothing
      , NdbEntry "x86_64" "libsigc-2_0-0" "2.10.2-1.18" Nothing Nothing
      , NdbEntry "x86_64" "libsemanage1" "3.0-1.27" Nothing Nothing
      , NdbEntry "x86_64" "libdw1" "0.177-150300.11.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libelf1" "0.177-150300.11.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libebl-plugins" "0.177-150300.11.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libreadline7" "7.0-19.6.1" Nothing Nothing
      , NdbEntry "x86_64" "patterns-base-fips" "20200124-10.5.1" Nothing Nothing
      , NdbEntry "x86_64" "libudev1" "246.16-150300.7.57.1" Nothing Nothing
      , NdbEntry "x86_64" "libsystemd0" "246.16-150300.7.57.1" Nothing Nothing
      , NdbEntry "x86_64" "libmount1" "2.36.2-150300.4.35.1" Nothing Nothing
      , NdbEntry "x86_64" "krb5" "1.19.2-150300.10.1" Nothing Nothing
      , NdbEntry "x86_64" "bash" "4.4-19.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libtirpc3" "1.2.6-150300.3.17.1" Nothing Nothing
      , NdbEntry "noarch" "login_defs" "4.8.1-150300.4.9.1" Nothing Nothing
      , NdbEntry "x86_64" "libacl1" "2.2.52-4.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libusb-1_0-0" "1.0.21-150000.3.5.1" Nothing Nothing
      , NdbEntry "x86_64" "libprocps7" "3.3.15-150000.7.31.1" Nothing Nothing
      , NdbEntry "x86_64" "procps" "3.3.15-150000.7.31.1" Nothing Nothing
      , NdbEntry "x86_64" "libproxy1" "0.4.15-12.41" Nothing Nothing
      , NdbEntry "x86_64" "findutils" "4.8.0-1.20" Nothing Nothing
      , NdbEntry "x86_64" "libssh4" "0.8.7-10.12.1" Nothing Nothing
      , NdbEntry "x86_64" "libcrack2" "2.9.7-11.6.1" Nothing Nothing
      , NdbEntry "x86_64" "cracklib" "2.9.7-11.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libcurl4" "7.66.0-150200.4.57.1" Nothing Nothing
      , NdbEntry "x86_64" "info" "6.5-4.17" Nothing Nothing
      , NdbEntry "x86_64" "libnsl2" "1.2.0-2.44" Nothing Nothing
      , NdbEntry "x86_64" "coreutils" "8.32-150300.3.5.1" Nothing Nothing
      , NdbEntry "x86_64" "sles-release" "15.3-55.4.1" Nothing Nothing
      , NdbEntry "x86_64" "nodejs-common" "4.0-1.9" Nothing Nothing
      , NdbEntry "x86_64" "pinentry" "1.1.0-4.3.1" Nothing Nothing
      , NdbEntry "x86_64" "grep" "3.1-150000.4.6.1" Nothing Nothing
      , NdbEntry "x86_64" "diffutils" "3.6-4.3.1" Nothing Nothing
      , NdbEntry "x86_64" "cpio" "2.12-3.9.1" Nothing Nothing
      , NdbEntry "x86_64" "gpg2" "2.2.27-150300.3.5.1" Nothing Nothing
      , NdbEntry "noarch" "rpm-config-SUSE" "1-5.6.1" Nothing Nothing
      , NdbEntry "x86_64" "rpm-ndb" "4.14.3-150300.55.1" Nothing Nothing
      , NdbEntry "x86_64" "permissions" "20181225-150200.23.23.1" Nothing Nothing
      , NdbEntry "x86_64" "libgpgme11" "1.13.1-4.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libsolv-tools" "0.7.24-150200.20.2" Nothing Nothing
      , NdbEntry "x86_64" "libzypp" "17.31.14-150200.70.1" Nothing Nothing
      , NdbEntry "x86_64" "zypper" "1.14.61-150200.54.1" Nothing Nothing
      , NdbEntry "x86_64" "pam" "1.3.0-150000.6.61.1" Nothing Nothing
      , NdbEntry "x86_64" "shadow" "4.8.1-150300.4.9.1" Nothing Nothing
      , NdbEntry "noarch" "sysuser-shadow" "2.0-4.2.8" Nothing Nothing
      , NdbEntry "noarch" "system-group-hardware" "20170617-17.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libutempter0" "1.1.6-3.42" Nothing Nothing
      , NdbEntry "x86_64" "util-linux" "2.36.2-150300.4.35.1" Nothing Nothing
      , NdbEntry "x86_64" "libcares2" "1.19.1-150000.3.23.1" Nothing Nothing
      , NdbEntry "noarch" "libicu69-ledata" "69.1-7.3.2" Nothing Nothing
      , NdbEntry "x86_64" "container-suseconnect" "2.4.0-150000.4.34.1" Nothing Nothing
      , NdbEntry "x86_64" "libtasn1-6" "4.13-150000.4.8.1" Nothing Nothing
      , NdbEntry "x86_64" "libtasn1" "4.13-150000.4.8.1" Nothing Nothing
      , NdbEntry "noarch" "netcfg" "11.6-3.3.1" Nothing Nothing
      , NdbEntry "noarch" "suse-build-key" "12.0-150000.8.31.1" Nothing Nothing
      , NdbEntry "x86_64" "timezone" "2023c-150000.75.23.1" Nothing Nothing
      , NdbEntry "x86_64" "libffi7" "3.2.1.git259-10.8" Nothing Nothing
      , NdbEntry "x86_64" "curl" "7.66.0-150200.4.57.1" Nothing Nothing
      , NdbEntry "x86_64" "openssl-1_1" "1.1.1d-150200.11.75.1" Nothing Nothing
      , NdbEntry "x86_64" "skelcd-EULA-bci" "2021.05.14-150300.4.8.1" Nothing Nothing
      , NdbEntry "x86_64" "libp11-kit0" "0.23.2-150000.4.16.1" Nothing Nothing
      , NdbEntry "x86_64" "p11-kit" "0.23.2-150000.4.16.1" Nothing Nothing
      , NdbEntry "x86_64" "p11-kit-tools" "0.23.2-150000.4.16.1" Nothing Nothing
      , NdbEntry "noarch" "ca-certificates" "2+git20210309.21162a6-2.1" Nothing Nothing
      , NdbEntry "noarch" "ca-certificates-mozilla" "2.60-150200.27.1" Nothing Nothing
      , NdbEntry "x86_64" "file" "5.32-7.14.1" Nothing Nothing
      , NdbEntry "x86_64" "which" "2.21-2.20" Nothing Nothing
      , NdbEntry "x86_64" "less" "530-3.3.2" Nothing Nothing
      , NdbEntry "x86_64" "update-alternatives" "1.19.0.4-150000.4.4.1" Nothing Nothing
      , NdbEntry "x86_64" "libicu69" "69.1-7.3.2" Nothing Nothing
      , NdbEntry "x86_64" "nodejs16" "16.20.1-150300.7.24.2" Nothing Nothing
      , NdbEntry "x86_64" "sed" "4.4-11.6" Nothing Nothing
      ]

    -- This fixture is generated like bciBaseClean, but don't use grep when
    -- running rpm -qa, since grep is neither installed or needed.
    bciMinimal =
      [ NdbEntry "noarch" "system-user-root" "20190513-3.3.1" Nothing Nothing
      , NdbEntry "x86_64" "filesystem" "15.0-11.8.1" Nothing Nothing
      , NdbEntry "x86_64" "skelcd-EULA-bci" "2021.05.14-150300.4.8.1" Nothing Nothing
      , NdbEntry "x86_64" "glibc" "2.31-150300.41.1" Nothing Nothing
      , NdbEntry "x86_64" "libpcre1" "8.45-150000.20.13.1" Nothing Nothing
      , NdbEntry "x86_64" "libgmp10" "6.1.2-4.9.1" Nothing Nothing
      , NdbEntry "x86_64" "libgcc_s1" "12.2.1+git416-150000.1.5.1" Nothing Nothing
      , NdbEntry "x86_64" "libcap2" "2.26-4.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libstdc++6" "12.2.1+git416-150000.1.5.1" Nothing Nothing
      , NdbEntry "x86_64" "libncurses6" "6.1-150000.5.12.1" Nothing Nothing
      , NdbEntry "x86_64" "terminfo-base" "6.1-150000.5.12.1" Nothing Nothing
      , NdbEntry "x86_64" "libattr1" "2.4.47-2.19" Nothing Nothing
      , NdbEntry "x86_64" "libselinux1" "3.0-1.31" Nothing Nothing
      , NdbEntry "x86_64" "libreadline7" "7.0-19.6.1" Nothing Nothing
      , NdbEntry "x86_64" "bash" "4.4-19.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libacl1" "2.2.52-4.3.1" Nothing Nothing
      , NdbEntry "x86_64" "coreutils" "8.32-150300.3.5.1" Nothing Nothing
      , NdbEntry "x86_64" "sles-release" "15.3-55.4.1" Nothing Nothing
      , NdbEntry "noarch" "ca-certificates-mozilla-prebuilt" "2.60-150200.27.1" Nothing Nothing
      , NdbEntry "x86_64" "libpopt0" "1.16-3.22" Nothing Nothing
      , NdbEntry "noarch" "file-magic" "5.32-7.14.1" Nothing Nothing
      , NdbEntry "x86_64" "libbz2-1" "1.0.6-5.11.1" Nothing Nothing
      , NdbEntry "x86_64" "liblua5_3-5" "5.3.6-3.6.1" Nothing Nothing
      , NdbEntry "x86_64" "liblzma5" "5.2.3-150000.4.7.1" Nothing Nothing
      , NdbEntry "x86_64" "libz1" "1.2.11-150000.3.36.1" Nothing Nothing
      , NdbEntry "x86_64" "libzstd1" "1.4.4-1.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libmagic1" "5.32-7.14.1" Nothing Nothing
      , NdbEntry "x86_64" "libcrypt1" "4.4.15-150300.4.4.3" Nothing Nothing
      , NdbEntry "x86_64" "libgpg-error0" "1.42-150300.9.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libdw1" "0.177-150300.11.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libebl-plugins" "0.177-150300.11.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libelf1" "0.177-150300.11.3.1" Nothing Nothing
      , NdbEntry "x86_64" "perl-base" "5.26.1-150300.17.11.1" Nothing Nothing
      , NdbEntry "x86_64" "libgcrypt20" "1.8.2-8.42.1" Nothing Nothing
      , NdbEntry "noarch" "rpm-config-SUSE" "1-5.6.1" Nothing Nothing
      , NdbEntry "x86_64" "rpm-ndb" "4.14.3-150300.52.1" Nothing Nothing
      ]

    -- This fixture is generated like bciBaseClean.
    openSuseLeap =
      [ NdbEntry "noarch" "boost-license1_66_0" "1.66.0-12.3.1" Nothing Nothing
      , NdbEntry "x86_64" "cracklib-dict-small" "2.9.7-11.6.1" Nothing Nothing
      , NdbEntry "noarch" "libldap-data" "2.4.46-150200.14.17.1" Nothing Nothing
      , NdbEntry "noarch" "file-magic" "5.32-7.14.1" Nothing Nothing
      , NdbEntry "noarch" "system-user-root" "20190513-3.3.1" Nothing Nothing
      , NdbEntry "x86_64" "filesystem" "15.0-11.8.1" Nothing Nothing
      , NdbEntry "x86_64" "libtirpc-netconfig" "1.2.6-150300.3.17.1" Nothing Nothing
      , NdbEntry "x86_64" "glibc" "2.31-150300.52.2" Nothing Nothing
      , NdbEntry "x86_64" "libcrypt1" "4.4.15-150300.4.4.3" Nothing Nothing
      , NdbEntry "x86_64" "perl-base" "5.26.1-150300.17.14.1" Nothing Nothing
      , NdbEntry "x86_64" "libssh-config" "0.9.6-150400.1.5" Nothing Nothing
      , NdbEntry "x86_64" "libsepol1" "3.1-150400.1.70" Nothing Nothing
      , NdbEntry "x86_64" "liblz4-1" "1.9.3-150400.1.7" Nothing Nothing
      , NdbEntry "x86_64" "libgpg-error0" "1.42-150400.1.101" Nothing Nothing
      , NdbEntry "x86_64" "libbz2-1" "1.0.8-150400.1.122" Nothing Nothing
      , NdbEntry "x86_64" "openSUSE-release-appliance-docker" "15.5-lp155.288.1" Nothing Nothing
      , NdbEntry "x86_64" "libcap-ng0" "0.7.9-4.37" Nothing Nothing
      , NdbEntry "x86_64" "libunistring2" "0.9.10-1.1" Nothing Nothing
      , NdbEntry "noarch" "kubic-locale-archive" "2.31-10.36" Nothing Nothing
      , NdbEntry "x86_64" "libzstd1" "1.5.0-150400.3.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libudev1" "249.16-150400.8.28.3" Nothing Nothing
      , NdbEntry "x86_64" "libeconf0" "0.4.6+git20220427.3016f4e-150400.3.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libcom_err2" "1.46.4-150400.3.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libcap2" "2.63-150400.3.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libaudit1" "3.0.6-150400.4.10.1" Nothing Nothing
      , NdbEntry "x86_64" "libusb-1_0-0" "1.0.24-150400.3.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libz1" "1.2.13-150500.2.3" Nothing Nothing
      , NdbEntry "x86_64" "libuuid1" "2.37.4-150500.7.16" Nothing Nothing
      , NdbEntry "x86_64" "libsmartcols1" "2.37.4-150500.7.16" Nothing Nothing
      , NdbEntry "x86_64" "libsasl2-3" "2.1.28-150500.1.1" Nothing Nothing
      , NdbEntry "x86_64" "libblkid1" "2.37.4-150500.7.16" Nothing Nothing
      , NdbEntry "x86_64" "libgcrypt20" "1.9.4-150500.10.19" Nothing Nothing
      , NdbEntry "x86_64" "libgcrypt20-hmac" "1.9.4-150500.10.19" Nothing Nothing
      , NdbEntry "x86_64" "libfdisk1" "2.37.4-150500.7.16" Nothing Nothing
      , NdbEntry "x86_64" "libverto1" "0.2.6-3.20" Nothing Nothing
      , NdbEntry "x86_64" "libpopt0" "1.16-3.22" Nothing Nothing
      , NdbEntry "x86_64" "libnpth0" "1.5-2.11" Nothing Nothing
      , NdbEntry "x86_64" "libattr1" "2.4.47-2.19" Nothing Nothing
      , NdbEntry "x86_64" "fillup" "1.42-2.18" Nothing Nothing
      , NdbEntry "x86_64" "update-alternatives" "1.19.0.4-150000.4.4.1" Nothing Nothing
      , NdbEntry "x86_64" "libsqlite3-0" "3.39.3-150000.3.20.1" Nothing Nothing
      , NdbEntry "x86_64" "libpcre1" "8.45-150000.20.13.1" Nothing Nothing
      , NdbEntry "x86_64" "liblzma5" "5.2.3-150000.4.7.1" Nothing Nothing
      , NdbEntry "x86_64" "liblua5_3-5" "5.3.6-3.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libkeyutils1" "1.6.3-5.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libjitterentropy3" "3.4.0-150000.1.9.1" Nothing Nothing
      , NdbEntry "x86_64" "libgmp10" "6.1.2-4.9.1" Nothing Nothing
      , NdbEntry "x86_64" "libgcc_s1" "12.3.0+git1204-150000.1.10.1" Nothing Nothing
      , NdbEntry "x86_64" "libksba8" "1.3.5-150000.4.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libassuan0" "2.5.5-150000.4.5.2" Nothing Nothing
      , NdbEntry "x86_64" "libidn2-0" "2.2.0-3.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libmagic1" "5.32-7.14.1" Nothing Nothing
      , NdbEntry "x86_64" "libacl1" "2.2.52-4.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libstdc++6" "12.3.0+git1204-150000.1.10.1" Nothing Nothing
      , NdbEntry "x86_64" "libpsl5" "0.20.1-150000.3.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libncurses6" "6.1-150000.5.15.1" Nothing Nothing
      , NdbEntry "x86_64" "terminfo-base" "6.1-150000.5.15.1" Nothing Nothing
      , NdbEntry "x86_64" "ncurses-utils" "6.1-150000.5.15.1" Nothing Nothing
      , NdbEntry "x86_64" "libnghttp2-14" "1.40.0-6.1" Nothing Nothing
      , NdbEntry "x86_64" "libbrotlicommon1" "1.0.7-3.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libprotobuf-lite20" "3.9.2-150200.4.21.1" Nothing Nothing
      , NdbEntry "x86_64" "libboost_system1_66_0" "1.66.0-12.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libbrotlidec1" "1.0.7-3.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libboost_thread1_66_0" "1.66.0-12.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libelf1" "0.185-150400.5.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libglib-2_0-0" "2.70.5-150400.3.8.1" Nothing Nothing
      , NdbEntry "x86_64" "libsystemd0" "249.16-150400.8.28.3" Nothing Nothing
      , NdbEntry "x86_64" "libyaml-cpp0_6" "0.6.3-150400.4.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libsigc-2_0-0" "2.10.7-150400.3.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libdw1" "0.185-150400.5.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libselinux1" "3.1-150400.1.69" Nothing Nothing
      , NdbEntry "x86_64" "libproxy1" "0.4.17-150400.1.8" Nothing Nothing
      , NdbEntry "x86_64" "libreadline7" "7.0-150400.25.22" Nothing Nothing
      , NdbEntry "x86_64" "libsemanage1" "3.1-150400.1.65" Nothing Nothing
      , NdbEntry "x86_64" "bash" "4.4-150400.25.22" Nothing Nothing
      , NdbEntry "x86_64" "bash-sh" "4.4-150400.25.22" Nothing Nothing
      , NdbEntry "x86_64" "cpio" "2.13-150400.1.98" Nothing Nothing
      , NdbEntry "x86_64" "coreutils" "8.32-150400.7.5" Nothing Nothing
      , NdbEntry "x86_64" "libxml2-2" "2.10.3-150500.5.5.1" Nothing Nothing
      , NdbEntry "x86_64" "libopenssl1_1" "1.1.1l-150500.17.12.1" Nothing Nothing
      , NdbEntry "x86_64" "libopenssl1_1-hmac" "1.1.1l-150500.17.12.1" Nothing Nothing
      , NdbEntry "x86_64" "libzio1" "1.06-2.20" Nothing Nothing
      , NdbEntry "x86_64" "info" "6.5-4.17" Nothing Nothing
      , NdbEntry "x86_64" "gawk" "4.2.1-1.41" Nothing Nothing
      , NdbEntry "x86_64" "libprocps7" "3.3.15-150000.7.31.1" Nothing Nothing
      , NdbEntry "x86_64" "pinentry" "1.1.0-4.3.1" Nothing Nothing
      , NdbEntry "x86_64" "grep" "3.1-150000.4.6.1" Nothing Nothing
      , NdbEntry "x86_64" "diffutils" "3.6-4.3.1" Nothing Nothing
      , NdbEntry "x86_64" "procps" "3.3.15-150000.7.31.1" Nothing Nothing
      , NdbEntry "x86_64" "findutils" "4.8.0-1.20" Nothing Nothing
      , NdbEntry "x86_64" "sed" "4.4-11.6" Nothing Nothing
      , NdbEntry "x86_64" "libmount1" "2.37.4-150500.7.16" Nothing Nothing
      , NdbEntry "x86_64" "krb5" "1.20.1-150500.1.2" Nothing Nothing
      , NdbEntry "noarch" "login_defs" "4.8.1-150400.10.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libaugeas0" "1.12.0-150400.3.3.6" Nothing Nothing
      , NdbEntry "x86_64" "libzck1" "1.1.16-150400.3.4.1" Nothing Nothing
      , NdbEntry "noarch" "rpm-config-SUSE" "1-150400.14.3.1" Nothing Nothing
      , NdbEntry "x86_64" "permissions" "20201225-150400.5.16.1" Nothing Nothing
      , NdbEntry "x86_64" "cracklib" "2.9.7-11.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libcrack2" "2.9.7-11.6.1" Nothing Nothing
      , NdbEntry "x86_64" "libldap-2_4-2" "2.4.46-150200.14.17.1" Nothing Nothing
      , NdbEntry "x86_64" "patterns-base-fips" "20200505-lp155.10.5" Nothing Nothing
      , NdbEntry "x86_64" "openSUSE-release" "15.5-lp155.288.1" Nothing Nothing
      , NdbEntry "x86_64" "gpg2" "2.2.27-150300.3.5.1" Nothing Nothing
      , NdbEntry "x86_64" "libtirpc3" "1.2.6-150300.3.17.1" Nothing Nothing
      , NdbEntry "x86_64" "rpm-ndb" "4.14.3-150300.55.1" Nothing Nothing
      , NdbEntry "x86_64" "libssh4" "0.9.6-150400.1.5" Nothing Nothing
      , NdbEntry "x86_64" "libgpgme11" "1.16.0-150400.1.80" Nothing Nothing
      , NdbEntry "x86_64" "libnsl2" "1.2.0-2.44" Nothing Nothing
      , NdbEntry "x86_64" "libsolv-tools" "0.7.24-150400.3.8.1" Nothing Nothing
      , NdbEntry "x86_64" "libcurl4" "8.0.1-150400.5.26.1" Nothing Nothing
      , NdbEntry "x86_64" "libzypp" "17.31.14-150400.3.35.1" Nothing Nothing
      , NdbEntry "x86_64" "zypper" "1.14.61-150400.3.24.1" Nothing Nothing
      , NdbEntry "x86_64" "pam" "1.3.0-150000.6.61.1" Nothing Nothing
      , NdbEntry "x86_64" "shadow" "4.8.1-150400.10.6.1" Nothing Nothing
      , NdbEntry "noarch" "sysuser-shadow" "3.1-150400.1.35" Nothing Nothing
      , NdbEntry "noarch" "system-group-hardware" "20170617-150400.22.33" Nothing Nothing
      , NdbEntry "x86_64" "libutempter0" "1.1.6-3.42" Nothing Nothing
      , NdbEntry "x86_64" "util-linux" "2.37.4-150500.7.16" Nothing Nothing
      , NdbEntry "x86_64" "aaa_base" "84.87+git20180409.04c9dae-150300.10.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libffi7" "3.2.1.git259-10.8" Nothing Nothing
      , NdbEntry "x86_64" "libtasn1-6" "4.13-150000.4.8.1" Nothing Nothing
      , NdbEntry "x86_64" "libtasn1" "4.13-150000.4.8.1" Nothing Nothing
      , NdbEntry "noarch" "netcfg" "11.6-3.3.1" Nothing Nothing
      , NdbEntry "noarch" "crypto-policies" "20210917.c9d86d1-150400.1.7" Nothing Nothing
      , NdbEntry "noarch" "openSUSE-build-key" "1.0-lp155.7.3.1" Nothing Nothing
      , NdbEntry "x86_64" "libp11-kit0" "0.23.22-150500.6.1" Nothing Nothing
      , NdbEntry "x86_64" "p11-kit" "0.23.22-150500.6.1" Nothing Nothing
      , NdbEntry "x86_64" "p11-kit-tools" "0.23.22-150500.6.1" Nothing Nothing
      , NdbEntry "x86_64" "openssl-1_1" "1.1.1l-150500.17.12.1" Nothing Nothing
      , NdbEntry "noarch" "ca-certificates" "2+git20210309.21162a6-2.1" Nothing Nothing
      , NdbEntry "noarch" "ca-certificates-mozilla" "2.60-150200.27.1" Nothing Nothing
      ]
