{-# LANGUAGE QuasiQuotes #-}

module App.Fossa.LicenseScanner.LicenseScannerSpec (spec) where

import App.Fossa.LicenseScanner (combineLicenseUnits, licenseScanSourceUnit)
import App.Fossa.ManualDeps (VendoredDependency (..))
import Control.Algebra (Has)
import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Stack (runStack)
import Control.Carrier.StickyLogger (ignoreStickyLogger)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Control.Effect.Lift (Lift)
import Control.Monad (void)
import Control.Timeout (Cancel, Duration (Seconds), timeout')
import Data.List.NonEmpty (NonEmpty ((:|)))
import Diag.Result (Result (..), renderFailure)
import Effect.Exec (runExecIO)
import Effect.Logger (ignoreLogger)
import Effect.ReadFS (runReadFSIO)
import Fossa.API.Types (RevisionInfo (RevisionInfo))
import Path (reldir, (</>))
import Path.IO (getCurrentDir)
import Srclib.Types (
  LicenseUnit (..),
  LicenseUnitData (..),
  LicenseUnitInfo (..),
  Locator (Locator),
  emptyLicenseUnitData,
 )
import Test.Effect (expectationFailure', it', shouldBe')
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
    "first-archive-test"
    (Just "0.0.1")

secondLocator :: Locator
secondLocator =
  Locator
    "archive"
    "second-archive-test"
    (Just "0.0.1")

firstRevision :: RevisionInfo
firstRevision =
  RevisionInfo
    "archive+1/first-archive-test$0.0.1"
    True

secondRevision :: RevisionInfo
secondRevision =
  RevisionInfo
    "archive+1/second-archive-test$0.0.1"
    True

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
    it' "should skip all if Core knows about all of the revisions" $ do
      let scanDir = currDir </> [reldir|testdata/repo|]
      expectEverythingScannedAlready
      locators <- runStack . ignoreLogger . runDiagnostics . ignoreStickyLogger . runExecIO . runReadFSIO $ licenseScanSourceUnit scanDir vendoredDeps
      case locators of
        Failure _ _ -> expectationFailure' "Could not license scan source unit"
        Success _ ls -> do
          ls `shouldBe'` expectedLocators
  where
    expectedLocators = firstLocator :| [secondLocator]

expectEverythingScannedAlready :: Has MockApi sig m => m ()
expectEverythingScannedAlready =
  (GetRevisionInfo vendoredDeps)
    `returnsOnce` [firstRevision, secondRevision]
