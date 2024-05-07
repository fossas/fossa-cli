{-# LANGUAGE CPP #-}

module App.Fossa.FirstPartyScan (
  runFirstPartyScan,
  firstPartyScanWithOrgInfo,
) where

import App.Fossa.Config.Analyze (AnalyzeConfig (..), VendoredDependencyOptions (licenseScanPathFilters))
import App.Fossa.LicenseScanner (scanVendoredDep)
import App.Fossa.ManualDeps (ManualDependencies (vendoredDependencies), VendoredDependency (..), findAndReadFossaDepsFile)
import App.Types (FirstPartyScansFlag (..))
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Effect.Debug (Debug)
import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Effect.FossaApiClient (FossaApiClient, getOrganization)
import Control.Effect.Lift (Lift)
import Control.Effect.StickyLogger (StickyLogger)
import Control.Monad (foldM)
import Data.Maybe (fromMaybe)
import Data.String.Conversion (ToString (toString), ToText (toText))
import Data.Text (Text)
import Data.Text qualified as Text
import Diag.Result
import Effect.Exec (Exec)
import Effect.Logger (Logger, logDebug)
import Effect.ReadFS (Has, ReadFS, resolvePath')
import Fossa.API.Types (ApiOpts (..), Organization (..), blankOrganization, orgFileUpload)
import Path (Abs, Dir, Path, Rel, SomeBase (..))
import Path.Extra
import Path.IO
import Srclib.Types (LicenseSourceUnit)
import Types (GlobFilter (GlobFilter), LicenseScanPathFilters (..))

#ifdef mingw32_HOST_OS
import System.FilePath (splitPath)
import Path (toFilePath)
#endif

runFirstPartyScan ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Logger sig m
  , Has Exec sig m
  , Has Debug sig m
  , Has ReadFS sig m
  ) =>
  Path Abs Dir ->
  Maybe ApiOpts ->
  AnalyzeConfig ->
  m (Maybe LicenseSourceUnit)
runFirstPartyScan root maybeApiOpts cfg = do
  -- if we do not have api opts (i.e. the --output flag was used), then we act as if the org defaults to not running first-party scans
  -- but the FOSSA server supports first-party scans
  case maybeApiOpts of
    Nothing -> firstPartyScanMain root cfg defaultOrg
    Just apiOpts -> runFossaApiClient apiOpts $ firstPartyScanWithOrgInfo root cfg
  where
    defaultOrg = blankOrganization{orgDefaultsToFirstPartyScans = False, orgSupportsFirstPartyScans = True, orgRequiresFullFileUploads = False}

firstPartyScanWithOrgInfo ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Logger sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has FossaApiClient sig m
  ) =>
  Path Abs Dir ->
  AnalyzeConfig ->
  m (Maybe LicenseSourceUnit)
firstPartyScanWithOrgInfo root cfg = do
  org <- getOrganization
  firstPartyScanMain root cfg org

shouldRunFirstPartyScans :: (Has Diagnostics sig m) => AnalyzeConfig -> Organization -> m Bool
shouldRunFirstPartyScans cfg org =
  case (firstPartyScansFlag cfg, orgDefaultsToFirstPartyScans org, orgSupportsFirstPartyScans org) of
    (FirstPartyScansOnFromFlag, _, False) -> fatalText "You provided the --experimental-force-first-party-scans flag but the FOSSA server does not support first-party scans"
    (_, _, False) -> pure False
    (FirstPartyScansOnFromFlag, _, True) -> pure True
    (FirstPartyScansOffFromFlag, _, True) -> pure False
    (FirstPartyScansUseDefault, orgDefault, True) -> pure orgDefault

firstPartyScanMain ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Logger sig m
  , Has Exec sig m
  , Has ReadFS sig m
  ) =>
  Path Abs Dir ->
  AnalyzeConfig ->
  Organization ->
  m (Maybe LicenseSourceUnit)
firstPartyScanMain base cfg org = do
  runFirstPartyScans <- shouldRunFirstPartyScans cfg org
  manualDeps <- findAndReadFossaDepsFile base
  let vdep = VendoredDependency "first-party" "." Nothing
      uploadKind = orgFileUpload org
  pathFilters <- mergePathFilters base manualDeps (licenseScanPathFilters $ vendoredDeps cfg)
  case runFirstPartyScans of
    (True) -> do
      _ <- logDebug "Running a first-party license scan on the code in this repository. Licenses found in this repository will show up as 'Directly in code' in the FOSSA UI"
      Just <$> scanVendoredDep base pathFilters uploadKind vdep
    (False) -> pure Nothing

-- mergePathFilters takes the existing filters from the config and merges them with filters constructed by looking at the vendored dependencies
-- We do this because we want to skip scanning the contents of all directories that are vendored dependencies,
-- and we also want to add all vendored dependencies that point at files to `licenseScanPathFilterFileExclude`,
-- so that we do not decompress and license-scan them
mergePathFilters ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  Maybe ManualDependencies ->
  Maybe LicenseScanPathFilters ->
  m (Maybe LicenseScanPathFilters)
mergePathFilters base maybeManualDeps existingPathFilters = do
  case (maybeManualDeps, existingPathFilters) of
    (Nothing, Nothing) -> pure Nothing
    (Just manualDeps, Nothing) -> do
      fromManual <- filtersFromManualDeps base manualDeps
      pure $ Just fromManual
    (Nothing, Just existingFilters) -> pure $ Just existingFilters
    (Just manualDeps, Just existingFilters) -> do
      fromManual <- filtersFromManualDeps base manualDeps
      let merged = mergeFilters fromManual existingFilters
      pure $ Just merged
  where
    -- mergeFilters takes the filters from manualDeps and the existing filters from the config and merges them
    -- the existing filters are the only one of the two that can contain licenseScanPathFiltersOnly entries,
    -- but both can contain licenseScanPathFiltersExclude or licenseScanPathFilterFileExclude entries
    mergeFilters :: LicenseScanPathFilters -> LicenseScanPathFilters -> LicenseScanPathFilters
    mergeFilters manualDepsFilters existingFilters =
      existingFilters
        { licenseScanPathFiltersExclude = (licenseScanPathFiltersExclude manualDepsFilters) <> (licenseScanPathFiltersExclude existingFilters)
        , licenseScanPathFilterFileExclude = (licenseScanPathFilterFileExclude manualDepsFilters) <> (licenseScanPathFilterFileExclude existingFilters)
        }

-- create LicenseScanPathFilters by looking at the vendored dependencies
-- We want to skip scanning all directories that are vendored dependencies,
-- and we also want to add all vendored dependencies that point at files to `licenseScanCompressedFileExclude`,
-- so that we do not decompress and license-scan them
filtersFromManualDeps ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  ManualDependencies ->
  m LicenseScanPathFilters
filtersFromManualDeps base manualDeps = do
  let paths = map vendoredPath $ vendoredDependencies manualDeps
  let emptyFilter =
        LicenseScanPathFilters
          { licenseScanPathFiltersOnly = []
          , licenseScanPathFiltersExclude = []
          , licenseScanPathFilterFileExclude = []
          }
  foldM (addFilter base) emptyFilter paths

addFilter ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  LicenseScanPathFilters ->
  Text ->
  m LicenseScanPathFilters
addFilter root existingFilter path = do
  scanPath <- Diag.runDiagnostics $ resolvePath' root $ toString path
  case scanPath of
    -- if it is a file, add it to licenseScanPathFilterFileExclude
    Success _ (SomeFile (Abs p)) -> do
      let existing = licenseScanPathFilterFileExclude existingFilter
      pure existingFilter{licenseScanPathFilterFileExclude = p : existing}
    -- if it is a dir, add it to licenseScanPathFiltersExclude
    Success _ (SomeDir (Abs p)) -> do
      let existing = licenseScanPathFiltersExclude existingFilter
      case makeRelative root p of
        Nothing -> pure existingFilter
        Just relPath -> do
          let globs = [GlobFilter (globbablePath relPath <> "/*"), GlobFilter (globbablePath relPath <> "/**")]
          pure existingFilter{licenseScanPathFiltersExclude = existing <> globs}
    _ -> pure existingFilter

-- `toText Path Rel Dir` has a trailing /, but themis path filters need those removed
-- Also, Themis requires the path separator to be / even on Windows, so replace "\\" with /
-- if you are on windows
#ifdef mingw32_HOST_OS
globbablePath :: Path Rel Dir -> Text
globbablePath p = fromMaybe t $ Text.stripSuffix "/" t
  where
    elems = splitPath $ toFilePath p
    t = Text.intercalate "/" (map toText elems)
#else
globbablePath :: Path Rel Dir -> Text
globbablePath p = fromMaybe t $ Text.stripSuffix "/" t
  where
    t = toText p
#endif
