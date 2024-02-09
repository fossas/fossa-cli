module App.Fossa.Analyze.Filter (
  CountedResult (..),
  checkForEmptyUpload,
) where

import App.Fossa.Analyze.Project (ProjectResult)
import App.Fossa.Analyze.Types (DiscoveredProjectScan)
import App.Fossa.Analyze.Upload (ScanUnits (..))
import App.Fossa.Config.Analyze (IncludeAll (..))
import Data.Flag (Flag, fromFlag)
import Srclib.Converter qualified as Srclib
import Srclib.Types (LicenseSourceUnit (licenseSourceUnitLicenseUnits), LicenseUnit (licenseUnitName), SourceUnit)

data CountedResult
  = NoneDiscovered
  | FilteredAll
  | CountedScanUnits ScanUnits

-- Takes a list of all projects analyzed, and the list after filtering.  We assume
-- that the smaller list is the latter, and return that list.  Starting with user-defined deps,
-- we also include a check for an additional source unit from fossa-deps.yml
-- and a check for any licenses found during the firstPartyScan
checkForEmptyUpload :: Flag IncludeAll -> [DiscoveredProjectScan] -> [ProjectResult] -> [SourceUnit] -> Maybe LicenseSourceUnit -> CountedResult
checkForEmptyUpload includeAll discovered filtered additionalUnits firstPartyScanResults = do
  if null additionalUnits
    then case (discoveredLen, filteredLen, licensesMaybeFound) of
      (0, _, Nothing) -> NoneDiscovered
      (_, 0, Nothing) -> FilteredAll
      (0, 0, Just licenseSourceUnit) -> CountedScanUnits $ LicenseSourceUnitOnly licenseSourceUnit
      (0, _, Just licenseSourceUnit) -> CountedScanUnits $ LicenseSourceUnitOnly licenseSourceUnit
      (_, 0, Just licenseSourceUnit) -> CountedScanUnits $ LicenseSourceUnitOnly licenseSourceUnit
      (_, _, Just licenseSourceUnit) -> CountedScanUnits $ SourceAndLicenseUnits discoveredUnits licenseSourceUnit
      (_, _, Nothing) -> CountedScanUnits . SourceUnitOnly $ discoveredUnits
    else -- If we have a additional source units, then there's always something to upload.
    case licensesMaybeFound of
      Nothing -> CountedScanUnits . SourceUnitOnly $ additionalUnits ++ discoveredUnits
      Just licenseSourceUnit -> CountedScanUnits $ SourceAndLicenseUnits (additionalUnits ++ discoveredUnits) licenseSourceUnit
  where
    discoveredLen = length discovered
    filteredLen = length filtered
    licensesMaybeFound = case firstPartyScanResults of
      Nothing -> Nothing
      Just scanResults ->
        if any isActualLicense $ licenseSourceUnitLicenseUnits scanResults
          then Just scanResults
          else Nothing

    -- The smaller list is the post-filter list, since filtering cannot add projects
    discoveredUnits = map (Srclib.projectToSourceUnit (fromFlag IncludeAll includeAll)) filtered
    isActualLicense :: LicenseUnit -> Bool
    isActualLicense licenseUnit = licenseUnitName licenseUnit /= "No_license_found"
