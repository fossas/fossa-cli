{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.FirstPartyScanSpec (spec) where

import App.Fossa.FirstPartyScan (firstPartyScanWithOrgInfo)
import App.Types (FirstPartyScansFlag (..))
import Control.Algebra (Has)
import Path (Dir, Path, Rel, mkRelDir, (</>))
import Path.IO (getCurrentDir)
import Test.Effect (it', shouldBe', expectationFailure', expectFatal')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe, runIO)
import Test.MockApi (MockApi, alwaysReturns)
import Fossa.API.Types (Organization(..))
import Control.Effect.FossaApiClient (FossaApiClientF(..))
import Srclib.Types (LicenseSourceUnit(..))

fixtureDir :: Path Rel Dir
fixtureDir = $(mkRelDir "test/App/Fossa/VendoredDependency/testdata/repo")

spec :: Spec
spec = do
  describe "runFirstPartyScan" $ do
    currDir <- runIO getCurrentDir
    let scanDir = currDir </> fixtureDir

    it' "should fail if the organization does not support first party scans and you force it on" $ do
      expectGetOrganizationThatDoesNotSupportFirstPartyScans
      expectFatal' $ firstPartyScanWithOrgInfo scanDir FirstPartyScansOnFromFlag

    it' "should not run if the organization does not support first-party scans and it is not forced on" $ do
      expectGetOrganizationThatDoesNotSupportFirstPartyScans
      licenseSourceUnit <- firstPartyScanWithOrgInfo scanDir FirstPartyScansUseDefault
      licenseSourceUnit `shouldBe'` Nothing

    it' "should not run if the organization defaults to no first-party scans and it is not forced on" $ do
      expectGetOrganizationThatDefaultsToNoFirstPartyScans
      licenseSourceUnit <- firstPartyScanWithOrgInfo scanDir FirstPartyScansUseDefault
      licenseSourceUnit `shouldBe'` Nothing

    it' "should run if the organization defaults to not running first-party scans but is forced on" $ do
      expectGetOrganizationThatDefaultsToNoFirstPartyScans
      licenseSourceUnit <- firstPartyScanWithOrgInfo scanDir FirstPartyScansOnFromFlag
      case licenseSourceUnit of
        Nothing -> expectationFailure' "first party scan should have run"
        Just LicenseSourceUnit{ licenseSourceUnitLicenseUnits = units} -> length units `shouldBe'` 2

    it' "should run if the organization defaults to running first-party scans and is not forced on" $ do
      expectGetOrganizationThatDefaultsToFirstPartyScans
      licenseSourceUnit <- firstPartyScanWithOrgInfo scanDir FirstPartyScansUseDefault
      case licenseSourceUnit of
        Nothing -> expectationFailure' "first party scan should have run"
        Just LicenseSourceUnit{ licenseSourceUnitLicenseUnits = units} -> length units `shouldBe'` 2

    it' "should not run if the organization defaults to running first-party scans and is forced off" $ do
      expectGetOrganizationThatDefaultsToFirstPartyScans
      licenseSourceUnit <- firstPartyScanWithOrgInfo scanDir FirstPartyScansOffFromFlag
      licenseSourceUnit `shouldBe'` Nothing


-- The default org defaults to not running first party scans but has first-party scans enabled
expectGetOrganizationThatDefaultsToNoFirstPartyScans :: Has MockApi sig m => m ()
expectGetOrganizationThatDefaultsToNoFirstPartyScans = GetOrganization `alwaysReturns` Fixtures.organization

expectGetOrganizationThatDoesNotSupportFirstPartyScans :: Has MockApi sig m => m ()
expectGetOrganizationThatDoesNotSupportFirstPartyScans = GetOrganization `alwaysReturns` Fixtures.organization{orgSupportsFirstPartyScans = False}

expectGetOrganizationThatDefaultsToFirstPartyScans :: Has MockApi sig m => m ()
expectGetOrganizationThatDefaultsToFirstPartyScans = GetOrganization `alwaysReturns` Fixtures.organization{orgDefaultsToFirstPartyScans = True}
