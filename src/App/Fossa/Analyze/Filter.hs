module App.Fossa.Analyze.Filter (
  CountedResult (..),
  checkForEmptyUpload,
) where

import App.Fossa.Analyze.Project (ProjectResult)
import App.Fossa.Analyze.Upload (ScanUnits (..))
import App.Fossa.Config.Analyze (IncludeAll (..))
import App.Fossa.Lernie.Types (LernieMatch)
import Data.Flag (Flag, fromFlag)
import Data.List.NonEmpty (fromList)
import Data.List.NonEmpty qualified as NE
import Srclib.Converter qualified as Srclib
import Srclib.Types (LicenseSourceUnit (licenseSourceUnitLicenseUnits), LicenseUnit (licenseUnitName), SourceUnit)

data CountedResult
  = NoneDiscovered
  | FilteredAll
  | CountedScanUnits ScanUnits
  | KeywordSearchOnly

-- | Return some state of the projects found, since we can't upload empty result arrays.
-- Takes a list of all projects analyzed, and the list after filtering.  We assume
-- that the smaller list is the latter, and return that list.  Starting with user-defined deps,
-- we also include a check for an additional source unit from fossa-deps.yml
-- and a check for any licenses found during the firstPartyScan
checkForEmptyUpload :: Flag IncludeAll -> [ProjectResult] -> [ProjectResult] -> [SourceUnit] -> Maybe LicenseSourceUnit -> Maybe [LernieMatch] -> CountedResult
checkForEmptyUpload includeAll xs ys additionalUnits firstPartyScanResults lernieMatches = do
  if null additionalUnits
    then case (xlen, ylen, licensesMaybeFound, lernieMatchesMaybeFound) of
      -- We didn't discover, so we also didn't filter
      (0, 0, Nothing, Nothing) ->
        NoneDiscovered
      (0, 0, Just licenseSourceUnit, _) ->
        CountedScanUnits $ LicenseSourceUnitOnly licenseSourceUnit
      -- only return KeywordSearchOnly if nothing else works
      (0, 0, Nothing, Just _) -> KeywordSearchOnly
      -- If either list is empty, we have nothing to upload
      (0, _, Nothing, Nothing) -> FilteredAll
      (_, 0, Nothing, Nothing) -> FilteredAll
      -- NE.fromList is a partial, but is safe since we confirm the length is > 0.
      (0, _, Just licenseSourceUnit, _) -> CountedScanUnits $ LicenseSourceUnitOnly licenseSourceUnit
      (_, 0, Just licenseSourceUnit, _) -> CountedScanUnits $ LicenseSourceUnitOnly licenseSourceUnit
      (_, _, Just licenseSourceUnit, _) -> CountedScanUnits $ SourceAndLicenseUnits (fromList discoveredUnits) licenseSourceUnit
      (_, _, Nothing, _) -> CountedScanUnits . SourceUnitOnly $ fromList discoveredUnits
    else -- If we have a additional source units, then there's always something to upload.
    case licensesMaybeFound of
      Nothing -> CountedScanUnits . SourceUnitOnly $ fromList (additionalUnits ++ discoveredUnits)
      Just licenseSourceUnit -> CountedScanUnits $ SourceAndLicenseUnits (fromList (additionalUnits ++ discoveredUnits)) licenseSourceUnit
  where
    lernieMatchesMaybeFound = NE.nonEmpty =<< lernieMatches
    xlen = length xs
    ylen = length ys
    licensesMaybeFound = case firstPartyScanResults of
      Nothing -> Nothing
      Just scanResults ->
        if any isActualLicense $ licenseSourceUnitLicenseUnits scanResults
          then Just scanResults
          else Nothing

    -- The smaller list is the post-filter list, since filtering cannot add projects
    filtered = if xlen > ylen then ys else xs
    discoveredUnits = map (Srclib.toSourceUnit (fromFlag IncludeAll includeAll)) filtered
    isActualLicense :: LicenseUnit -> Bool
    isActualLicense licenseUnit = licenseUnitName licenseUnit /= "No_license_found"
