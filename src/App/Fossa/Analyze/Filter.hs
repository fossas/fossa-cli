module App.Fossa.Analyze.Filter (
  CountedResult (..),
  checkForEmptyUpload,
) where

import App.Fossa.Analyze.Project (ProjectResult)
import App.Fossa.Config.Analyze (IncludeAll (..))
import Data.Flag (Flag, fromFlag)
import Data.List.NonEmpty (NonEmpty, fromList)
import Srclib.Converter qualified as Srclib
import Srclib.Types (LicenseSourceUnit (licenseSourceUnitLicenseUnits), LicenseUnit (licenseUnitName), SourceUnit)

data CountedResult
  = NoneDiscovered
  | FilteredAll
  | FoundSome (NonEmpty SourceUnit)
  | FoundLicensesOnly

-- | Return some state of the projects found, since we can't upload empty result arrays.
-- Takes a list of all projects analyzed, and the list after filtering.  We assume
-- that the smaller list is the latter, and return that list.  Starting with user-defined deps,
-- we also include a check for an additional source unit from fossa-deps.yml
-- and a check for any licenses found during the firstPartyScan
checkForEmptyUpload :: Flag IncludeAll -> [ProjectResult] -> [ProjectResult] -> [SourceUnit] -> Maybe LicenseSourceUnit -> CountedResult
checkForEmptyUpload includeAll xs ys additionalUnits firstPartyScanResults = do
  if null additionalUnits
    then case (xlen, ylen) of
      -- We didn't discover, so we also didn't filter
      (0, 0) ->
        if licensesFound
          then FoundLicensesOnly
          else NoneDiscovered
      -- If either list is empty, we have nothing to upload
      (0, _) -> FilteredAll
      (_, 0) -> FilteredAll
      -- NE.fromList is a partial, but is safe since we confirm the length is > 0.
      _ -> FoundSome $ fromList discoveredUnits
    else -- If we have a additional source units, then there's always something to upload.
      FoundSome $ fromList (additionalUnits ++ discoveredUnits)
  where
    xlen = length xs
    ylen = length ys

    -- The smaller list is the post-filter list, since filtering cannot add projects
    filtered = if xlen > ylen then ys else xs
    discoveredUnits = map (Srclib.toSourceUnit (fromFlag IncludeAll includeAll)) filtered
    licensesFound = case firstPartyScanResults of
      Nothing -> False
      Just scanResults -> any isActualLicense $ licenseSourceUnitLicenseUnits scanResults

    isActualLicense :: LicenseUnit -> Bool
    isActualLicense licenseUnit = licenseUnitName licenseUnit /= "No_license_found"
