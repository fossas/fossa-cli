{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Analyze.FilterSpec (spec) where

import App.Fossa.Analyze.Filter (
  CountedResult (..)
  , checkForEmptyUpload
  )
import App.Fossa.Analyze.Project (ProjectResult)
import App.Fossa.Analyze.Types (
  DiscoveredProjectIdentifier (..)
  , DiscoveredProjectScan (..)
  )
import App.Fossa.Analyze.Upload (ScanUnits (..))
import Data.List.NonEmpty qualified as NE
import Path (mkAbsDir)
import Srclib.Types (
  LicenseScanType (..)
  , LicenseSourceUnit (..)
  , LicenseUnit (..)
  , SourceUnit (..)
  , emptyLicenseUnit
  , emptyLicenseUnitData
  )
import Test.Hspec (Spec, describe, it, shouldBe)
import Types (
  DiscoveredProjectType (..)
  , GraphBreadth (..)
  )

-- ---------------------------------------------------------------------------
-- Minimal test data
-- ---------------------------------------------------------------------------

someDiscoveredScan :: DiscoveredProjectScan
someDiscoveredScan =
  SkippedDueToProvidedFilter $
    DiscoveredProjectIdentifier
      { dpiProjectPath = $(mkAbsDir "/")
      , dpiProjectType = MavenProjectType
      }

-- | A minimal SourceUnit.
someSourceUnit :: SourceUnit
someSourceUnit =
  SourceUnit
    { sourceUnitName = "test-unit"
    , sourceUnitType = "pip"
    , sourceUnitManifest = "requirements.txt"
    , sourceUnitBuild = Nothing
    , sourceUnitGraphBreadth = Complete
    , sourceUnitOriginPaths = []
    , sourceUnitNoticeFiles = []
    , additionalData = Nothing
    , sourceUnitLabels = Nothing
    }

-- | A LicenseSourceUnit whose unit has a real license name.
-- 'isActualLicense' passes because the name is not "No_license_found".
realLicenseSourceUnit :: LicenseSourceUnit
realLicenseSourceUnit =
  LicenseSourceUnit
    { licenseSourceUnitName = "test/path"
    , licenseSourceUnitType = CliLicenseScanned
    , licenseSourceUnitLicenseUnits =
        emptyLicenseUnit{licenseUnitName = "MIT", licenseUnitData = emptyLicenseUnitData NE.:| []} NE.:| []
    }

-- | A LicenseSourceUnit whose only unit is the "No_license_found" sentinel.
-- 'isActualLicense' fails for every unit, so 'licensesMaybeFound' resolves to Nothing.
sentinelLicenseSourceUnit :: LicenseSourceUnit
sentinelLicenseSourceUnit =
  LicenseSourceUnit
    { licenseSourceUnitName = "test/path"
    , licenseSourceUnitType = CliLicenseScanned
    , licenseSourceUnitLicenseUnits =
        emptyLicenseUnit{licenseUnitName = "No_license_found", licenseUnitData = emptyLicenseUnitData NE.:| []} NE.:| []
    }

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Phantom list of 'ProjectResult' with a given length.
-- Only 'length' is evaluated by the function under test.
fakeFiltered :: Int -> [ProjectResult]
fakeFiltered n = replicate n (error "ProjectResult value evaluated — should only be counted")

-- ---------------------------------------------------------------------------
-- Branch mapping (sourceUnits = []):
--
--   Branch 1: (0, _, Nothing)  -> NoneDiscovered
--   Branch 2: (_, 0, Nothing)  -> FilteredAll
--   Branch 3: (0, 0, Just lsu) -> LicenseSourceUnitOnly lsu
--   Branch 4: (0, _, Just lsu) -> LicenseSourceUnitOnly lsu   (discoveredLen = 0, filteredLen > 0)
--   Branch 5: (_, 0, Just lsu) -> LicenseSourceUnitOnly lsu   (discoveredLen > 0, filteredLen = 0)
--   Branch 6: (_, _, Just lsu) -> SourceAndLicenseUnits [] lsu
--   Branch 7: (_, _, Nothing)  -> SourceUnitOnly []
--
-- Branch mapping (sourceUnits = non-empty):
--
--   Branch 8: Nothing          -> SourceUnitOnly sourceUnits
--   Branch 9: Just lsu         -> SourceAndLicenseUnits sourceUnits lsu
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "checkForEmptyUpload" $ do
  describe "when sourceUnits is empty" $ do
    it "branch 1: NoneDiscovered when nothing was discovered and no licenses" $
      checkForEmptyUpload [] [] [] Nothing
        `shouldBe` NoneDiscovered

    it "branch 2: FilteredAll when projects were discovered, all filtered out, and no licenses" $
      checkForEmptyUpload [someDiscoveredScan] [] [] Nothing
        `shouldBe` FilteredAll

    it "branch 3: LicenseSourceUnitOnly when nothing discovered, nothing filtered, and real licenses found" $
      checkForEmptyUpload [] [] [] (Just realLicenseSourceUnit)
        `shouldBe` CountedScanUnits (LicenseSourceUnitOnly realLicenseSourceUnit)

    it "branch 4: LicenseSourceUnitOnly when nothing discovered, some filtered, and real licenses found" $
      -- discoveredLen = 0, filteredLen = 1 -> (0, _, Just) arm
      checkForEmptyUpload [] (fakeFiltered 1) [] (Just realLicenseSourceUnit)
        `shouldBe` CountedScanUnits (LicenseSourceUnitOnly realLicenseSourceUnit)

    it "branch 5: LicenseSourceUnitOnly when projects discovered, all filtered, and real licenses found" $
      -- discoveredLen = 1, filteredLen = 0 -> (_, 0, Just) arm
      checkForEmptyUpload [someDiscoveredScan] [] [] (Just realLicenseSourceUnit)
        `shouldBe` CountedScanUnits (LicenseSourceUnitOnly realLicenseSourceUnit)

    it "branch 6: SourceAndLicenseUnits (with empty source list) when projects discovered, some remain after filter, and real licenses found" $
      -- discoveredLen = 1, filteredLen = 1 -> (_, _, Just) arm
      checkForEmptyUpload [someDiscoveredScan] (fakeFiltered 1) [] (Just realLicenseSourceUnit)
        `shouldBe` CountedScanUnits (SourceAndLicenseUnits [] realLicenseSourceUnit)

    it "branch 7: SourceUnitOnly [] when projects discovered, some remain after filter, but no licenses" $
      -- discoveredLen = 1, filteredLen = 1 -> (_, _, Nothing) arm
      checkForEmptyUpload [someDiscoveredScan] (fakeFiltered 1) [] Nothing
        `shouldBe` CountedScanUnits (SourceUnitOnly [])

    it "treats a first-party scan with only 'No_license_found' as if no licenses exist (same as branch 1)" $
      -- isActualLicense is False for all units, so licensesMaybeFound = Nothing
      checkForEmptyUpload [] [] [] (Just sentinelLicenseSourceUnit)
        `shouldBe` NoneDiscovered

  describe "when sourceUnits is non-empty" $ do
    let units = [someSourceUnit]

    it "branch 8: SourceUnitOnly when there are no first-party licenses" $
      checkForEmptyUpload [] [] units Nothing
        `shouldBe` CountedScanUnits (SourceUnitOnly units)

    it "branch 9: SourceAndLicenseUnits when real first-party licenses are present" $
      checkForEmptyUpload [] [] units (Just realLicenseSourceUnit)
        `shouldBe` CountedScanUnits (SourceAndLicenseUnits units realLicenseSourceUnit)

    it "returns SourceUnitOnly when first-party scan has only sentinel license name" $
      checkForEmptyUpload [] [] units (Just sentinelLicenseSourceUnit)
        `shouldBe` CountedScanUnits (SourceUnitOnly units)
