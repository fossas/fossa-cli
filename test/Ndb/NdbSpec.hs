module Ndb.NdbSpec (spec) where

import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Stack (runStack)
import Effect.Exec (runExecIO)
import Effect.ReadFS (runReadFSIO)
import Path (Abs, File, Path)
import Path.IO qualified as PIO
import ResultUtil (assertOnSuccess)
import Strategy.NDB.Internal (NdbEntry (..), readNDB)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

spec :: Spec
spec = do
  describe "Container NDB parser" $ do
    target <- runIO testDbFile
    result <- runIO . runStack . runDiagnostics . runExecIO . runReadFSIO $ readNDB target

    it "parses ndb contents" $
      assertOnSuccess result $ \_ c ->
        c `shouldBe` expectedEntries

-- | Test file from: https://github.com/jssblck/go-rpmdb/blob/9a3bd2ebb923399db2189c5d8963af27368ba2c8/pkg/rpmdb_test.go#L72-L74
testDbFile :: IO (Path Abs File)
testDbFile = PIO.resolveFile' "test/Ndb/testdata/sle15-bci/Packages.db"

-- | Adapted from https://github.com/jssblck/go-rpmdb/blob/d637bcc368602fb1dbd7a017c58e979266e01cd5/pkg/rpmdb_testcase_test.go#L2985-L3021.
-- Steps for VS Code users:
-- 1. Copy each line beginning with '{intRef()'
-- 2. Search and replace regexp '\{intRef\(\), ([^,]+), "([^"]+)", "([^"]+)", ([^,]+).+' with '  , NdbEntry $4 $1 "$2-$3" Nothing'
-- 3. Drop the first line comma, save to autoformat.
--
-- '{Version}-{Release}' is because of the way that RPM dependencies are formed; we need both and they're meant to be separated by a dash.
-- Reference: https://github.com/fossas/fossa-cli/blob/f25758ce3af7dd2992edd4cdd19ff84edd9b6c86/src/App/Fossa/VSI/DynLinked/Internal/Lookup/RPM.hs#L111-L118,
-- which creates well-formed RPM entries that Core can resolve.
--
-- Core's linux locator documentation (https://github.com/fossas/FOSSA/blob/e61713dec1ef80dc6b6114f79622c14df5278235/modules/fetchers/README.md#locators-for-linux-packages)
-- does not explicitly state this, although it is in their examples and I swear I read it somewhere that I can no longer find.
expectedEntries :: [NdbEntry]
expectedEntries =
  [ NdbEntry "noarch" "system-user-root" "20190513-3.3.1" Nothing
  , NdbEntry "x86_64" "filesystem" "15.0-11.3.2" Nothing
  , NdbEntry "x86_64" "glibc" "2.31-9.3.2" Nothing
  , NdbEntry "x86_64" "libpcre1" "8.45-20.10.1" Nothing
  , NdbEntry "x86_64" "libgmp10" "6.1.2-4.6.1" Nothing
  , NdbEntry "x86_64" "libgcc_s1" "11.2.1+git610-1.3.9" Nothing
  , NdbEntry "x86_64" "libcap2" "2.26-4.6.1" Nothing
  , NdbEntry "x86_64" "libstdc++6" "11.2.1+git610-1.3.9" Nothing
  , NdbEntry "x86_64" "libncurses6" "6.1-5.9.1" Nothing
  , NdbEntry "x86_64" "terminfo-base" "6.1-5.9.1" Nothing
  , NdbEntry "x86_64" "libattr1" "2.4.47-2.19" Nothing
  , NdbEntry "x86_64" "libselinux1" "3.0-1.31" Nothing
  , NdbEntry "x86_64" "libreadline7" "7.0-19.6.1" Nothing
  , NdbEntry "x86_64" "bash" "4.4-19.6.1" Nothing
  , NdbEntry "x86_64" "libacl1" "2.2.52-4.3.1" Nothing
  , NdbEntry "x86_64" "coreutils" "8.32-3.2.1" Nothing
  , NdbEntry "x86_64" "sles-release" "15.3-55.4.1" Nothing
  , NdbEntry "noarch" "ca-certificates-mozilla-prebuilt" "2.44-21.1" Nothing
  , NdbEntry "x86_64" "libgpg-error0" "1.29-1.8" Nothing
  , NdbEntry "x86_64" "libpopt0" "1.16-3.22" Nothing
  , NdbEntry "noarch" "file-magic" "5.32-7.14.1" Nothing
  , NdbEntry "x86_64" "libbz2-1" "1.0.6-5.11.1" Nothing
  , NdbEntry "x86_64" "liblua5_3-5" "5.3.6-3.6.1" Nothing
  , NdbEntry "x86_64" "liblzma5" "5.2.3-4.3.1" Nothing
  , NdbEntry "x86_64" "libz1" "1.2.11-3.21.1" Nothing
  , NdbEntry "x86_64" "libzstd1" "1.4.4-1.6.1" Nothing
  , NdbEntry "x86_64" "libmagic1" "5.32-7.14.1" Nothing
  , NdbEntry "x86_64" "libdw1" "0.168-4.5.3" Nothing
  , NdbEntry "x86_64" "libebl-plugins" "0.168-4.5.3" Nothing
  , NdbEntry "x86_64" "libelf1" "0.168-4.5.3" Nothing
  , NdbEntry "x86_64" "libcrypt1" "4.4.15-2.51" Nothing
  , NdbEntry "x86_64" "perl-base" "5.26.1-15.87" Nothing
  , NdbEntry "x86_64" "libgcrypt20" "1.8.2-8.39.1" Nothing
  , NdbEntry "noarch" "rpm-config-SUSE" "1-5.6.1" Nothing
  , NdbEntry "x86_64" "rpm-ndb" "4.14.3-40.1" Nothing
  ]
