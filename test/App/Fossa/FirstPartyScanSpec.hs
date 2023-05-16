{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.FirstPartyScanSpec (spec) where

import App.Fossa.Config.Analyze (StandardAnalyzeConfig (..))
import App.Fossa.FirstPartyScan (firstPartyScanWithOrgInfo)
import App.Types (FirstPartyScansFlag (..))
import Control.Algebra (Has)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust)
import Fossa.API.Types (Organization (..))
import Path (Dir, Path, Rel, mkRelDir, (</>))
import Path.IO (getCurrentDir)
import Srclib.Types (LicenseSourceUnit (..), LicenseUnit (licenseUnitData), LicenseUnitData (licenseUnitDataContents))
import Test.Effect (expectFatal', expectationFailure', it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe, runIO)
import Test.MockApi (MockApi, alwaysReturns)

fixtureDir :: Path Rel Dir
fixtureDir = $(mkRelDir "test/App/Fossa/VendoredDependency/testdata/repo")

spec :: Spec
spec = do
  describe "runFirstPartyScan" $ do
    currDir <- runIO getCurrentDir
    let scanDir = currDir </> fixtureDir

    it' "should fail if the organization does not support first party scans and you force it on" $ do
      expectGetOrganizationThatDoesNotSupportFirstPartyScans
      expectFatal' $ firstPartyScanWithOrgInfo scanDir Fixtures.standardAnalyzeConfig{firstPartyScansFlag = FirstPartyScansOnFromFlag}

    it' "should not run if the organization does not support first-party scans and it is not forced on" $ do
      expectGetOrganizationThatDoesNotSupportFirstPartyScans
      licenseSourceUnit <- firstPartyScanWithOrgInfo scanDir Fixtures.standardAnalyzeConfig
      licenseSourceUnit `shouldBe'` Nothing

    it' "should not run if the organization defaults to no first-party scans and it is not forced on" $ do
      expectGetOrganizationThatDefaultsToNoFirstPartyScans
      licenseSourceUnit <- firstPartyScanWithOrgInfo scanDir Fixtures.standardAnalyzeConfig
      licenseSourceUnit `shouldBe'` Nothing

    it' "should run if the organization defaults to not running first-party scans but is forced on" $ do
      expectGetOrganizationThatDefaultsToNoFirstPartyScans
      licenseSourceUnit <- firstPartyScanWithOrgInfo scanDir Fixtures.standardAnalyzeConfig{firstPartyScansFlag = FirstPartyScansOnFromFlag}
      case licenseSourceUnit of
        Nothing -> expectationFailure' "first party scan should have run"
        Just LicenseSourceUnit{licenseSourceUnitLicenseUnits = units} -> length units `shouldBe'` 2

    it' "should run if the organization defaults to running first-party scans and is not forced on" $ do
      expectGetOrganizationThatDefaultsToFirstPartyScans
      licenseSourceUnit <- firstPartyScanWithOrgInfo scanDir Fixtures.standardAnalyzeConfig
      case licenseSourceUnit of
        Nothing -> expectationFailure' "first party scan should have run"
        Just LicenseSourceUnit{licenseSourceUnitLicenseUnits = units} -> length units `shouldBe'` 2

    it' "should not run if the organization defaults to running first-party scans and is forced off" $ do
      expectGetOrganizationThatDefaultsToFirstPartyScans
      licenseSourceUnit <- firstPartyScanWithOrgInfo scanDir Fixtures.standardAnalyzeConfig{firstPartyScansFlag = FirstPartyScansOffFromFlag}
      licenseSourceUnit `shouldBe'` Nothing

    it' "should do full file uploads if the org defaults to full file uploads" $ do
      expectGetOrganizationThatDefaultsToFirstPartyScansAndFullFileUploads
      licenseSourceUnit <- firstPartyScanWithOrgInfo scanDir Fixtures.standardAnalyzeConfig
      case licenseSourceUnit of
        Nothing -> expectationFailure' "first party scan should have run"
        Just LicenseSourceUnit{licenseSourceUnitLicenseUnits = units} -> do
          length units `shouldBe'` 2
          let unitData = NE.head . licenseUnitData $ NE.head units
          isJust (licenseUnitDataContents unitData) `shouldBe'` True

    it' "should upload matchData if the org does not default to full-file uploads" $ do
      expectGetOrganizationThatDefaultsToFirstPartyScans
      licenseSourceUnit <- firstPartyScanWithOrgInfo scanDir Fixtures.standardAnalyzeConfig
      case licenseSourceUnit of
        Nothing -> expectationFailure' "first party scan should have run"
        Just LicenseSourceUnit{licenseSourceUnitLicenseUnits = units} -> do
          length units `shouldBe'` 2
          let unitData = NE.head . licenseUnitData $ NE.head units
          licenseUnitDataContents unitData `shouldBe'` Nothing

-- The default org defaults to not running first party scans but has first-party scans enabled
expectGetOrganizationThatDefaultsToNoFirstPartyScans :: Has MockApi sig m => m ()
expectGetOrganizationThatDefaultsToNoFirstPartyScans = GetOrganization `alwaysReturns` Fixtures.organization

expectGetOrganizationThatDoesNotSupportFirstPartyScans :: Has MockApi sig m => m ()
expectGetOrganizationThatDoesNotSupportFirstPartyScans = GetOrganization `alwaysReturns` Fixtures.organization{orgSupportsFirstPartyScans = False}

expectGetOrganizationThatDefaultsToFirstPartyScans :: Has MockApi sig m => m ()
expectGetOrganizationThatDefaultsToFirstPartyScans = GetOrganization `alwaysReturns` Fixtures.organization{orgDefaultsToFirstPartyScans = True}

expectGetOrganizationThatDefaultsToFirstPartyScansAndFullFileUploads :: Has MockApi sig m => m ()
expectGetOrganizationThatDefaultsToFirstPartyScansAndFullFileUploads = GetOrganization `alwaysReturns` Fixtures.organization{orgDefaultsToFirstPartyScans = True, orgRequiresFullFileUploads = True}
