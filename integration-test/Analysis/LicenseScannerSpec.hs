{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.LicenseScannerSpec (spec) where

import Analysis.FixtureUtils (
  FixtureArtifact (FixtureArtifact),
  getArtifact,
 )
import App.Fossa.LicenseScanner (scanVendoredDep)
import App.Fossa.VendoredDependency (VendoredDependency (VendoredDependency))
import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Stack (runStack)
import Control.Carrier.StickyLogger (ignoreStickyLogger)
import Data.List.Extra (head')
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Diag.Result (Result (Failure, Success), renderFailure)
import Effect.Exec (runExecIO)
import Effect.ReadFS (runReadFSIO)
import Path (reldir, (</>))
import Path.IO qualified as PIO
import Srclib.Types (
  LicenseSourceUnit (licenseSourceUnitLicenseUnits),
  LicenseUnit (licenseUnitFiles, licenseUnitName),
  emptyLicenseUnit,
 )
import Test.Hspec (Spec, describe, it, shouldBe)

recursiveArchive :: FixtureArtifact
recursiveArchive =
  FixtureArtifact
    "https://github.com/fossas/cli-license-scan-integration-test-fixtures/archive/refs/heads/main.zip"
    [reldir|manual-deps/license-scanner/|]
    [reldir|recursive-archive|]

vendoredDep :: VendoredDependency
vendoredDep =
  VendoredDependency
    "recursive-archive-test"
    "vendor/foo.tar.gz"
    (Just "0.0.1")

-- the contents of the archive look like this. The `FOO_LICENSE` files contain an MIT license.
-- `bar_apache.rb` and `something.rb` contain an Apache-2.0 license.
-- The quux directory is compressed into quux.tar.gz, and then baz into baz.tar.gz, bar into bar.tar.gz
-- and foo into foo.tar.gz
-- The script that creates this nested archive is here: https://github.com/fossas/example-projects/tree/main/recursive-archive-generator
--
-- recursive-archive
-- └── vendor
--     └── foo
--         ├── VERSION
--         └── bar
--             ├── MIT_LICENSE
--             ├── bar_apache.rb
--             └── baz
--                 ├── SOMETHING_LICENSE
--                 ├── quux
--                 │   └── QUUX_LICENSE
--                 └── something.rb
spec :: Spec
spec = do
  describe "scanVendoredDep" $ do
    it "should find licenses in nested archives" $ do
      extractedDir <- getArtifact recursiveArchive
      let scanDir = extractedDir </> [reldir|cli-license-scan-integration-test-fixtures-main/recursive-archive|]
      units <- runStack . runDiagnostics . ignoreStickyLogger . runExecIO . runReadFSIO . fmap licenseSourceUnitLicenseUnits $ scanVendoredDep scanDir Nothing vendoredDep
      PIO.removeDirRecur extractedDir
      case units of
        Failure ws eg -> fail (show (renderFailure ws eg "An issue occurred"))
        Success _ us -> do
          length us `shouldBe` 3
          NE.sort (NE.map licenseUnitName us) `shouldBe` NE.fromList ["No_license_found", "apache-2.0", "mit"]
          NE.sort (licenseUnitFiles mitUnit) `shouldBe` NE.fromList ["vendor/foo/bar/MIT_LICENSE", "vendor/foo/bar/baz/SOMETHING_LICENSE", "vendor/foo/bar/baz/quux/QUUX_LICENSE"]
          NE.sort (licenseUnitFiles apacheUnit) `shouldBe` NE.fromList ["vendor/foo/bar/bar_apache.rb", "vendor/foo/bar/baz/something.rb"]
          where
            mitUnit :: LicenseUnit
            mitUnit = fromMaybe emptyLicenseUnit (head' $ NE.filter (\u -> licenseUnitName u == "mit") us)
            apacheUnit :: LicenseUnit
            apacheUnit = fromMaybe emptyLicenseUnit (head' $ NE.filter (\u -> licenseUnitName u == "apache-2.0") us)
