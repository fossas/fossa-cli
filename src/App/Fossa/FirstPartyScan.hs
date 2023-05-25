module App.Fossa.FirstPartyScan (
  runFirstPartyScan,
  firstPartyScanWithOrgInfo,
) where

import App.Fossa.Config.Analyze (AnalyzeConfig (..), VendoredDependencyOptions (licenseScanPathFilters))
import App.Fossa.LicenseScanner (scanVendoredDep)
import App.Fossa.ManualDeps (VendoredDependency (..))
import App.Types (FirstPartyScansFlag (..), FullFileUploads (FullFileUploads))
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Effect.FossaApiClient (FossaApiClient, getOrganization)
import Control.Effect.Lift (Lift)
import Control.Effect.StickyLogger (StickyLogger)
import Effect.Exec (Exec)
import Effect.Logger (Logger, logDebug)
import Effect.ReadFS (Has, ReadFS)
import Fossa.API.Types (ApiOpts (..), Organization (..), blankOrganization)
import Path (Abs, Dir, Path)
import Srclib.Types (LicenseSourceUnit)

runFirstPartyScan ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Logger sig m
  , Has Exec sig m
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
  let vdep = VendoredDependency "first-party" "." Nothing
      fullFileUploads = FullFileUploads $ orgRequiresFullFileUploads org
      pathFilters = licenseScanPathFilters $ vendoredDeps cfg
  case runFirstPartyScans of
    (True) -> do
      _ <- logDebug "Running a first-party license scan on the code in this repository. Licenses found in this repository will show up as 'Directly in code' in the FOSSA UI"
      Just <$> scanVendoredDep base pathFilters fullFileUploads vdep
    (False) -> pure Nothing
