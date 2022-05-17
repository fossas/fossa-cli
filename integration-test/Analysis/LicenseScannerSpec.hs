{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.LicenseScannerSpec (spec) where

import Analysis.FixtureUtils (
  FixtureArtifact (FixtureArtifact),
  getArtifact,
 )
import App.Fossa.LicenseScanner (licenseScanSourceUnit, scanVendoredDep)
import App.Fossa.VendoredDependency (VendoredDependency (VendoredDependency))
import Control.Carrier.Diagnostics (Has, runDiagnostics)
import Control.Carrier.Stack (runStack)
import Control.Carrier.StickyLogger (ignoreStickyLogger)
import Data.List.Extra (head')
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Diag.Result (Result (Failure, Success), renderFailure)
import Effect.Exec (runExecIO)
import Effect.Logger (ignoreLogger, runLogger)
import Effect.ReadFS (runReadFSIO)
import Fossa.API.Types (RevisionInfo)
import Path (reldir, (</>))
import Path.IO qualified as PIO
import Srclib.Types (
  LicenseSourceUnit (licenseSourceUnitLicenseUnits),
  LicenseUnit (licenseUnitFiles, licenseUnitName),
  Locator (Locator),
  emptyLicenseUnit,
 )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.MockApi (MockApi, alwaysReturns, returnsOnce)

recursiveArchive :: FixtureArtifact
recursiveArchive =
  FixtureArtifact
    "https://github.com/fossas/cli-license-scan-integration-test-fixtures/archive/refs/heads/main.zip"
    [reldir|manual-deps/license-scanner/|]
    [reldir|recursive-archive|]

firstVendoredDep :: VendoredDependency
firstVendoredDep =
  VendoredDependency
    "recursive-archive-test"
    "vendor/foo.tar.gz"
    (Just "0.0.1")

secondVendoredDep :: VendoredDependency
secondVendoredDep =
  VendoredDependency
    "another-archive-test"
    "vendor/bar.tar.gz"
    (Just "0.0.1")

vendoredDeps :: NonEmpty VendoredDependency
vendoredDeps = firstVendoredDep :| [secondVendoredDep]

firstLocator :: Locator
firstLocator =
  Locator
    "archive"
    "recursive-archive-test"
    "0.0.1"

secondLocator :: Locator
secondLocator =
  Locator
    "archive"
    "another-archive-test"
    "0.0.1"

firstRevision :: RevisionInfo
firstRevision =
  Revision
    "archive+1/recursive-archive-test$0.0.1"
    True

secondRevision :: RevisionInfo
secondRevision =
  Revision
    "archive+1/another-archive-test$0.0.1"
    True

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
      units <- runStack . runDiagnostics . ignoreStickyLogger . runExecIO . runReadFSIO . fmap licenseSourceUnitLicenseUnits $ scanVendoredDep scanDir firstVendoredDep
      PIO.removeDirRecur extractedDir
      case units of
        Failure ws eg -> fail (show (renderFailure ws eg "An issue occurred"))
        Success _ us -> do
          (length us) `shouldBe` 3
          NE.sort (NE.map licenseUnitName us) `shouldBe` "No_license_found" :| ["apache-2.0", "mit"]
          NE.sort (licenseUnitFiles mitUnit) `shouldBe` "vendor/foo/bar/MIT_LICENSE" :| ["vendor/foo/bar/baz/SOMETHING_LICENSE", "vendor/foo/bar/baz/quux/QUUX_LICENSE"]
          NE.sort (licenseUnitFiles apacheUnit) `shouldBe` "vendor/foo/bar/bar_apache.rb" :| ["vendor/foo/bar/baz/something.rb"]
          where
            mitUnit :: LicenseUnit
            mitUnit = fromMaybe emptyLicenseUnit (head' $ NE.filter (\u -> licenseUnitName u == "mit") us)
            apacheUnit :: LicenseUnit
            apacheUnit = fromMaybe emptyLicenseUnit (head' $ NE.filter (\u -> licenseUnitName u == "apache-2.0") us)

  describe "licenseScanSourceUnits" $ do
    it "should skip all if Core knows about all of the revisions" $ do
      extractedDir <- getArtifact recursiveArchive
      let scanDir = extractedDir </> [reldir|cli-license-scan-integration-test-fixtures-main/recursive-archive|]
      expectScannedAlready

      locators <- runStack . ignoreLogger . runDiagnostics . ignoreStickyLogger . runExecIO . runReadFSIO $ licenseScanSourceUnit scanDir vendoredDeps
      case locators of
        Failure ws eg -> fail (show (renderFailure ws eg "An issue occurred"))
        Success _ ls -> do
          ls `shouldBe` expectedLocators
  where
    expectedLocators = firstLocator :| [secondLocator]

expectEverythingScannedAlready :: Has MockApi sig m => m ()
expectEverythingScannedAlready =
  (GetRevisionInfo vendoredDeps)
    `returnsOnce` firstRevision :| [secondRevision]
