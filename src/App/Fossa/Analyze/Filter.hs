module App.Fossa.Analyze.Filter (
  CountedResult (..),
  checkForEmptyUpload,
) where

import App.Fossa.Analyze.Project (ProjectResult)
import App.Fossa.Config.Analyze (IncludeAll (..))
import Data.Flag (Flag, fromFlag)
import Data.List.NonEmpty (NonEmpty, fromList)
import Srclib.Converter qualified as Srclib
import Srclib.Types (SourceUnit, LicenseSourceUnit (licenseSourceUnitLicenseUnits), LicenseUnit (licenseUnitName))
import qualified Data.List.NonEmpty as NE

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
      (0, 0) -> if licensesFound
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
      Just scanResults -> any isActualLicense (NE.toList (licenseSourceUnitLicenseUnits scanResults))

    isActualLicense :: LicenseUnit -> Bool
    isActualLicense licenseUnit = licenseUnitName licenseUnit /= "No_license_found"

-- | Skips projects by relabeling scan type, by applying production path filtering.
skipNonProdProjectsBasedOnPath :: BaseDir -> [DiscoveredProjectScan] -> [DiscoveredProjectScan]
skipNonProdProjectsBasedOnPath dir = map (applyDefaultProductionPathFilter dir)

-- For each of the projects, we strip the root directory path from the prefix of the project path.
-- We don't want parent directories of the scan root affecting "production path" filtering -- e.g.,
-- if we're running in a directory called "tmp", we still want results.
applyDefaultProductionPathFilter :: BaseDir -> DiscoveredProjectScan -> DiscoveredProjectScan
applyDefaultProductionPathFilter _ (SkippedDueToProvidedFilter a) = SkippedDueToProvidedFilter a
applyDefaultProductionPathFilter _ (SkippedDueToDefaultProductionFilter a) = SkippedDueToDefaultProductionFilter a
applyDefaultProductionPathFilter dir (Scanned dpi result) = case result of
  Failure _ _ ->
    if isInProdPath $ dpiProjectPath dpi
      then Scanned dpi result
      else SkippedDueToDefaultProductionFilter dpi
  Success _ res ->
    if isInProdPath $ projectResultPath res
      then Scanned dpi result
      else SkippedDueToDefaultProductionFilter dpi
  where
    isInProdPath :: Path Abs Dir -> Bool
    isInProdPath = isProductionPath . dropPrefix rootPath . fromAbsDir

    rootPath :: FilePath
    rootPath = fromAbsDir $ unBaseDir dir

    dropPrefix :: String -> String -> String
    dropPrefix prefix str = fromMaybe str (stripPrefix prefix str)

isProductionPath :: FilePath -> Bool
isProductionPath path = not $ any (`isInfixOf` path) ignoredPaths

ignoredPaths :: [[Char]]
ignoredPaths =
  [ "dist-newstyle"
  , "doc/"
  , "docs/"
  , "test/"
  , "example/"
  , "examples/"
  , "vendor/"
  , "node_modules/"
  , ".srclib-cache/"
  , "spec/"
  , "Godeps/"
  , ".git/"
  , "bower_components/"
  , "third_party/"
  , "third-party/"
  , "Carthage/"
  , "Checkouts/"
  ]
