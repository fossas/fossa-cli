{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.LicenseScannerSpec (spec) where

import App.Fossa.LicenseScanner (combineLicenseUnits, licenseScanSourceUnit)
import App.Fossa.VendoredDependency (VendoredDependencyScanMode (..))
import Control.Algebra (Has)
import Control.Effect.FossaApiClient (FossaApiClientF (..), PackageRevision (..))
import Data.List.NonEmpty qualified as NE
import Fossa.API.Types (Archive, ArchiveComponents (ArchiveComponents, archives, forceRebuild, fullFiles), Organization (orgRequiresFullFileUploads))
import Path (Dir, Path, Rel, mkRelDir, (</>))
import Path.IO (getCurrentDir)
import Srclib.Types (
  LicenseSourceUnit,
  LicenseUnit (..),
  LicenseUnitData (..),
  LicenseUnitInfo (..),
  emptyLicenseUnitData,
  renderLocator,
 )
import Test.Effect (it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.MockApi (MockApi, alwaysReturns, returnsOnce, returnsOnceForAnyRequest)
import App.Types (FullFileUploads(..))

-- test data for combineLicenseUnits tests
info :: LicenseUnitInfo
info = LicenseUnitInfo{licenseUnitInfoDescription = Just ""}

unitOne :: LicenseUnit
unitOne =
  LicenseUnit
    { licenseUnitName = "MIT"
    , licenseUnitType = "LicenseUnit"
    , licenseUnitDir = ""
    , licenseUnitData =
        NE.fromList
          [ emptyLicenseUnitData{licenseUnitDataPath = "foo/bar/LICENSE"}
          , emptyLicenseUnitData{licenseUnitDataPath = "foo/bar/one.txt"}
          ]
    , licenseUnitFiles =
        NE.fromList ["foo/bar/LICENSE", "foo/bar/one.txt"]
    , licenseUnitInfo = info
    }

unitTwo :: LicenseUnit
unitTwo =
  LicenseUnit
    { licenseUnitName = "MIT"
    , licenseUnitType = "LicenseUnit"
    , licenseUnitDir = ""
    , licenseUnitData = NE.fromList [emptyLicenseUnitData{licenseUnitDataPath = "foo/bar/baz/ANOTHER_LICENSE"}, emptyLicenseUnitData{licenseUnitDataPath = "foo/bar/baz/two.txt"}]
    , licenseUnitFiles = NE.fromList ["foo/bar/baz/ANOTHER_LICENSE", "foo/bar/baz/two.txt"]
    , licenseUnitInfo = info
    }
expectedCombinedUnit :: LicenseUnit
expectedCombinedUnit =
  LicenseUnit
    { licenseUnitName = "MIT"
    , licenseUnitType = "LicenseUnit"
    , licenseUnitDir = ""
    , licenseUnitData =
        NE.fromList
          [ emptyLicenseUnitData{licenseUnitDataPath = "foo/bar/LICENSE"}
          , emptyLicenseUnitData{licenseUnitDataPath = "foo/bar/baz/ANOTHER_LICENSE"}
          , emptyLicenseUnitData{licenseUnitDataPath = "foo/bar/baz/two.txt"}
          , emptyLicenseUnitData{licenseUnitDataPath = "foo/bar/one.txt"}
          ]
    , licenseUnitFiles =
        NE.fromList
          [ "foo/bar/LICENSE"
          , "foo/bar/baz/ANOTHER_LICENSE"
          , "foo/bar/baz/two.txt"
          , "foo/bar/one.txt"
          ]
    , licenseUnitInfo = info
    }

fixtureDir :: Path Rel Dir
fixtureDir = $(mkRelDir "test/App/Fossa/VendoredDependency/testdata/repo")

spec :: Spec
spec = do
  describe "combineLicenseUnits" $ do
    it "should combine two MIT units" $
      combineLicenseUnits [unitOne, unitTwo] `shouldBe` [expectedCombinedUnit]
    it "should not combine two units with different licenses" $
      combineLicenseUnits [unitOne, unitTwo{licenseUnitName = "AGPL"}] `shouldBe` [unitTwo{licenseUnitName = "AGPL"}, unitOne]

  describe "licenseScanSourceUnits" $ do
    currDir <- runIO getCurrentDir
    let scanDir = currDir </> fixtureDir

    it' "should skip all if Core knows about all of the revisions" $ do
      expectGetApiOpts
      expectGetOrganization
      expectEverythingScannedAlready
      expectFinalizeScan Fixtures.archives
      locators <- licenseScanSourceUnit SkipPreviouslyScanned Nothing (FullFileUploads False) scanDir Fixtures.vendoredDeps
      locators `shouldBe'` Fixtures.locators

    it' "should scan all if Core does not know about the revisions" $ do
      expectGetApiOpts
      expectGetOrganization
      expectNothingScannedYet
      expectGetSignedUrl PackageRevision{packageName = "first-archive-test", packageVersion = "0.0.1"}
      expectUploadLicenseScanResult Fixtures.firstLicenseSourceUnit
      expectGetSignedUrl PackageRevision{packageName = "second-archive-test", packageVersion = "0.0.1"}
      expectUploadLicenseScanResult Fixtures.secondLicenseSourceUnit
      expectFinalizeScan Fixtures.archives
      locators <- licenseScanSourceUnit SkipPreviouslyScanned Nothing (FullFileUploads False) scanDir Fixtures.vendoredDeps
      locators `shouldBe'` Fixtures.locators

    it' "should scan all if the revisions are still being scanned" $ do
      expectGetApiOpts
      expectGetOrganization
      expectAllScansInProgress
      expectGetSignedUrl PackageRevision{packageName = "first-archive-test", packageVersion = "0.0.1"}
      expectUploadLicenseScanResult Fixtures.firstLicenseSourceUnit
      expectGetSignedUrl PackageRevision{packageName = "second-archive-test", packageVersion = "0.0.1"}
      expectUploadLicenseScanResult Fixtures.secondLicenseSourceUnit
      expectFinalizeScan Fixtures.archives
      locators <- licenseScanSourceUnit SkipPreviouslyScanned Nothing (FullFileUploads False) scanDir Fixtures.vendoredDeps
      locators `shouldBe'` Fixtures.locators

    it' "should scan one if one revision is still being scanned" $ do
      expectGetApiOpts
      expectGetOrganization
      expectOneScanInProgress
      expectGetSignedUrl PackageRevision{packageName = "first-archive-test", packageVersion = "0.0.1"}
      expectUploadLicenseScanResult Fixtures.firstLicenseSourceUnit
      expectFinalizeScan Fixtures.archives
      locators <- licenseScanSourceUnit SkipPreviouslyScanned Nothing (FullFileUploads False) scanDir Fixtures.vendoredDeps
      locators `shouldBe'` Fixtures.locators

    it' "should always scan all if vendor dependency skipping is not supported" $ do
      expectGetApiOpts
      expectGetOrganization
      expectGetSignedUrl PackageRevision{packageName = "first-archive-test", packageVersion = "0.0.1"}
      expectUploadLicenseScanResult Fixtures.firstLicenseSourceUnit
      expectGetSignedUrl PackageRevision{packageName = "second-archive-test", packageVersion = "0.0.1"}
      expectUploadLicenseScanResult Fixtures.secondLicenseSourceUnit
      expectFinalizeScan Fixtures.archives
      locators <- licenseScanSourceUnit SkippingNotSupported Nothing (FullFileUploads False) scanDir Fixtures.vendoredDeps
      locators `shouldBe'` Fixtures.locators

    it' "should always scan all if the --force-vendor-dependency-rescans flag is used" $ do
      expectGetApiOpts
      expectGetOrganization
      expectGetSignedUrl PackageRevision{packageName = "first-archive-test", packageVersion = "0.0.1"}
      expectUploadLicenseScanResult Fixtures.firstLicenseSourceUnit
      expectGetSignedUrl PackageRevision{packageName = "second-archive-test", packageVersion = "0.0.1"}
      expectUploadLicenseScanResult Fixtures.secondLicenseSourceUnit
      expectFinalizeScanWithForceRebuild Fixtures.archives
      locators <- licenseScanSourceUnit SkippingDisabledViaFlag Nothing (FullFileUploads False) scanDir Fixtures.vendoredDeps
      locators `shouldBe'` Fixtures.locators

    it' "should upload with the fullFiles flag if the org requires full files" $ do
      expectGetApiOpts
      expectGetOrganizationThatRequiresFullFiles
      expectNothingScannedYet
      expectGetSignedUrl PackageRevision{packageName = "first-archive-test", packageVersion = "0.0.1"}
      expectUploadLicenseScanResult Fixtures.firstLicenseSourceUnit
      expectGetSignedUrl PackageRevision{packageName = "second-archive-test", packageVersion = "0.0.1"}
      expectUploadLicenseScanResult Fixtures.secondLicenseSourceUnit
      expectFinalizeScanWithFullFiles Fixtures.archives
      locators <- licenseScanSourceUnit SkipPreviouslyScanned Nothing (FullFileUploads True) scanDir Fixtures.vendoredDeps
      locators `shouldBe'` Fixtures.locators


expectGetApiOpts :: Has MockApi sig m => m ()
expectGetApiOpts =
  GetApiOpts `alwaysReturns` Fixtures.apiOpts

expectGetOrganization :: Has MockApi sig m => m ()
expectGetOrganization = GetOrganization `alwaysReturns` Fixtures.organization

expectGetOrganizationThatRequiresFullFiles :: Has MockApi sig m => m ()
expectGetOrganizationThatRequiresFullFiles = GetOrganization `alwaysReturns` Fixtures.organization{orgRequiresFullFileUploads = True}

expectGetSignedUrl :: Has MockApi sig m => PackageRevision -> m ()
expectGetSignedUrl packageRevision = GetSignedLicenseScanUrl packageRevision `alwaysReturns` Fixtures.signedUrl

expectEverythingScannedAlready :: Has MockApi sig m => m ()
expectEverythingScannedAlready =
  GetAnalyzedRevisions Fixtures.vendoredDeps
    `returnsOnce` map renderLocator (NE.toList Fixtures.locators)

expectNothingScannedYet :: Has MockApi sig m => m ()
expectNothingScannedYet =
  GetAnalyzedRevisions Fixtures.vendoredDeps
    `returnsOnce` []

expectAllScansInProgress :: Has MockApi sig m => m ()
expectAllScansInProgress =
  GetAnalyzedRevisions Fixtures.vendoredDeps
    `returnsOnce` []

expectOneScanInProgress :: Has MockApi sig m => m ()
expectOneScanInProgress =
  (GetAnalyzedRevisions Fixtures.vendoredDeps)
    `returnsOnce` [renderLocator Fixtures.secondLocator]

expectUploadLicenseScanResult :: Has MockApi sig m => LicenseSourceUnit -> m ()
expectUploadLicenseScanResult licenseUnit =
  (UploadLicenseScanResult Fixtures.signedUrl licenseUnit) `returnsOnceForAnyRequest` ()

expectFinalizeScan :: Has MockApi sig m => [Archive] -> m ()
expectFinalizeScan as =
  (FinalizeLicenseScan ArchiveComponents{archives = as, forceRebuild = False, fullFiles = (FullFileUploads False)}) `returnsOnce` ()

expectFinalizeScanWithForceRebuild :: Has MockApi sig m => [Archive] -> m ()
expectFinalizeScanWithForceRebuild as =
  (FinalizeLicenseScan ArchiveComponents{archives = as, forceRebuild = True, fullFiles = (FullFileUploads False)}) `returnsOnce` ()

expectFinalizeScanWithFullFiles :: Has MockApi sig m => [Archive] -> m ()
expectFinalizeScanWithFullFiles as =
  (FinalizeLicenseScan ArchiveComponents{archives = as, forceRebuild = False, fullFiles = (FullFileUploads True)}) `returnsOnce` ()
