{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.LicenseScanner.LicenseScannerSpec (spec) where

import App.Fossa.LicenseScanner (combineLicenseUnits, licenseScanSourceUnit)
import App.Fossa.VendoredDependency (VendoredDependency (..))
import Control.Algebra (Has)
import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Stack (runStack)
import Control.Carrier.StickyLogger (ignoreStickyLogger)
import Control.Effect.FossaApiClient (FossaApiClientF (..), PackageRevision (..))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Diag.Result (Result (..))
import Effect.Exec (runExecIO)
import Effect.Logger (ignoreLogger)
import Effect.ReadFS (runReadFSIO)
import Fossa.API.Types (Archive (Archive), ArchiveComponents (ArchiveComponents, archives), RevisionInfo (..))
import Path (Dir, Path, Rel, mkRelDir, (</>))
import Path.IO (getCurrentDir)
import Srclib.Types (
  LicenseSourceUnit,
  LicenseUnit (..),
  LicenseUnitData (..),
  LicenseUnitInfo (..),
  Locator (Locator),
  emptyLicenseUnitData,
 )
import Test.Effect (expectationFailure', it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.MockApi (MockApi, alwaysReturns, returnsOnce)

-- test data for combineLicenseUnits tests
info :: LicenseUnitInfo
info = LicenseUnitInfo{licenseUnitInfoDescription = Just ""}

unitOne :: LicenseUnit
unitOne =
  LicenseUnit
    { licenseUnitName = "MIT"
    , licenseUnitType = "LicenseUnit"
    , licenseUnitDir = ""
    , licenseUnitData = emptyLicenseUnitData{licenseUnitDataPath = "foo/bar/LICENSE"} :| [emptyLicenseUnitData{licenseUnitDataPath = "foo/bar/one.txt"}]
    , licenseUnitFiles =
        "foo/bar/LICENSE" :| ["foo/bar/one.txt"]
    , licenseUnitInfo = info
    }

unitTwo :: LicenseUnit
unitTwo =
  LicenseUnit
    { licenseUnitName = "MIT"
    , licenseUnitType = "LicenseUnit"
    , licenseUnitDir = ""
    , licenseUnitData = emptyLicenseUnitData{licenseUnitDataPath = "foo/bar/baz/ANOTHER_LICENSE"} :| [emptyLicenseUnitData{licenseUnitDataPath = "foo/bar/baz/two.txt"}]
    , licenseUnitFiles = "foo/bar/baz/ANOTHER_LICENSE" :| ["foo/bar/baz/two.txt"]
    , licenseUnitInfo = info
    }
expectedCombinedUnit :: LicenseUnit
expectedCombinedUnit =
  LicenseUnit
    { licenseUnitName = "MIT"
    , licenseUnitType = "LicenseUnit"
    , licenseUnitDir = ""
    , licenseUnitData =
        emptyLicenseUnitData{licenseUnitDataPath = "foo/bar/LICENSE"}
          :| [ emptyLicenseUnitData{licenseUnitDataPath = "foo/bar/baz/ANOTHER_LICENSE"}
             , emptyLicenseUnitData{licenseUnitDataPath = "foo/bar/baz/two.txt"}
             , emptyLicenseUnitData{licenseUnitDataPath = "foo/bar/one.txt"}
             ]
    , licenseUnitFiles =
        "foo/bar/LICENSE"
          :| [ "foo/bar/baz/ANOTHER_LICENSE"
             , "foo/bar/baz/two.txt"
             , "foo/bar/one.txt"
             ]
    , licenseUnitInfo = info
    }

-- Test data for licenseScanSourceUnits tests
firstVendoredDep :: VendoredDependency
firstVendoredDep =
  VendoredDependency
    "first-archive-test"
    "vendor/foo"
    (Just "0.0.1")

secondVendoredDep :: VendoredDependency
secondVendoredDep =
  VendoredDependency
    "second-archive-test"
    "vendor/bar"
    (Just "0.0.1")

vendoredDeps :: NonEmpty VendoredDependency
vendoredDeps = firstVendoredDep :| [secondVendoredDep]

firstLocator :: Locator
firstLocator =
  Locator
    "archive"
    "42/first-archive-test"
    (Just "0.0.1")

secondLocator :: Locator
secondLocator =
  Locator
    "archive"
    "42/second-archive-test"
    (Just "0.0.1")

firstRevision :: RevisionInfo
firstRevision =
  RevisionInfo
    "archive+42/first-archive-test$0.0.1"
    True

secondRevision :: RevisionInfo
secondRevision =
  RevisionInfo
    "archive+42/second-archive-test$0.0.1"
    True

firstArchive :: Archive
firstArchive =
  Archive
    "first-archive-test"
    "0.0.1"

secondArchive :: Archive
secondArchive =
  Archive
    "second-archive-test"
    "0.0.1"

arcs :: [Archive]
arcs = [firstArchive, secondArchive]

expectedLocators :: NonEmpty Locator
expectedLocators = firstLocator :| [secondLocator]

fixtureDir :: Path Rel Dir
fixtureDir = $(mkRelDir "test/App/Fossa/LicenseScanner/testdata/repo")

spec :: Spec
spec = do
  -- this test only exists to prevent merging the commented out analyzers
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
      expectFinalizeScan arcs
      locators <- runStack . ignoreLogger . runDiagnostics . ignoreStickyLogger . runExecIO . runReadFSIO $ licenseScanSourceUnit scanDir vendoredDeps
      case locators of
        Failure ws eg -> expectationFailure' $ "Could not license scan source unit. Warnings = " <> show ws <> ", error group: " <> show eg
        Success _ ls -> do
          ls `shouldBe'` expectedLocators

    it' "should scan all if Core does not know about the revisions" $ do
      expectGetApiOpts
      expectGetOrganization
      expectNothingScannedYet
      expectGetSignedUrl PackageRevision{packageName = "first-archive-test", packageVersion = "0.0.1"}
      expectUploadLicenseScanResult Fixtures.firstLicenseSourceUnit
      expectGetSignedUrl PackageRevision{packageName = "second-archive-test", packageVersion = "0.0.1"}
      expectUploadLicenseScanResult Fixtures.secondLicenseSourceUnit
      expectFinalizeScan arcs
      locators <- runStack . ignoreLogger . runDiagnostics . ignoreStickyLogger . runExecIO . runReadFSIO $ licenseScanSourceUnit scanDir vendoredDeps
      case locators of
        Failure ws eg -> expectationFailure' $ "Could not license scan source unit. Warnings = " <> show ws <> ", error group: " <> show eg
        Success _ ls -> do
          ls `shouldBe'` expectedLocators

    it' "should scan all if the revisions are still being scanned" $ do
      expectGetApiOpts
      expectGetOrganization
      expectAllScansInProgress
      expectGetSignedUrl PackageRevision{packageName = "first-archive-test", packageVersion = "0.0.1"}
      expectUploadLicenseScanResult Fixtures.firstLicenseSourceUnit
      expectGetSignedUrl PackageRevision{packageName = "second-archive-test", packageVersion = "0.0.1"}
      expectUploadLicenseScanResult Fixtures.secondLicenseSourceUnit
      expectFinalizeScan arcs
      locators <- runStack . ignoreLogger . runDiagnostics . ignoreStickyLogger . runExecIO . runReadFSIO $ licenseScanSourceUnit scanDir vendoredDeps
      case locators of
        Failure ws eg -> expectationFailure' $ "Could not license scan source unit. Warnings = " <> show ws <> ", error group: " <> show eg
        Success _ ls -> do
          ls `shouldBe'` expectedLocators

    it' "should scan one if one revision is still being scanned" $ do
      expectGetApiOpts
      expectGetOrganization
      expectOneScanInProgress
      expectGetSignedUrl PackageRevision{packageName = "first-archive-test", packageVersion = "0.0.1"}
      expectUploadLicenseScanResult Fixtures.firstLicenseSourceUnit
      expectFinalizeScan arcs
      locators <- runStack . ignoreLogger . runDiagnostics . ignoreStickyLogger . runExecIO . runReadFSIO $ licenseScanSourceUnit scanDir vendoredDeps
      case locators of
        Failure ws eg -> expectationFailure' $ "Could not license scan source unit. Warnings = " <> show ws <> ", error group: " <> show eg
        Success _ ls -> do
          ls `shouldBe'` expectedLocators

expectGetApiOpts :: Has MockApi sig m => m ()
expectGetApiOpts =
  GetApiOpts `alwaysReturns` Fixtures.apiOpts

expectGetOrganization :: Has MockApi sig m => m ()
expectGetOrganization = GetOrganization `alwaysReturns` Fixtures.organization

expectGetSignedUrl :: Has MockApi sig m => PackageRevision -> m ()
expectGetSignedUrl packageRevision = GetSignedLicenseScanUrl packageRevision `alwaysReturns` Fixtures.signedUrl

expectEverythingScannedAlready :: Has MockApi sig m => m ()
expectEverythingScannedAlready =
  (GetRevisionInfo vendoredDeps)
    `returnsOnce` [firstRevision, secondRevision]

expectNothingScannedYet :: Has MockApi sig m => m ()
expectNothingScannedYet =
  (GetRevisionInfo vendoredDeps)
    `returnsOnce` []

expectAllScansInProgress :: Has MockApi sig m => m ()
expectAllScansInProgress =
  (GetRevisionInfo vendoredDeps)
    `returnsOnce` [firstRevision{revisionInfoResolved = False}, secondRevision{revisionInfoResolved = False}]

expectOneScanInProgress :: Has MockApi sig m => m ()
expectOneScanInProgress =
  (GetRevisionInfo vendoredDeps)
    `returnsOnce` [firstRevision{revisionInfoResolved = False}, secondRevision]

expectUploadLicenseScanResult :: Has MockApi sig m => LicenseSourceUnit -> m ()
expectUploadLicenseScanResult licenseUnit =
  (UploadLicenseScanResult Fixtures.signedUrl licenseUnit) `returnsOnce` ()

expectFinalizeScan :: Has MockApi sig m => [Archive] -> m ()
expectFinalizeScan as =
  (FinalizeLicenseScan ArchiveComponents{archives = as}) `returnsOnce` ()
