module App.Fossa.FirstPartyScan (
  runFirstPartyScan,
) where
import Srclib.Types (LicenseSourceUnit)
import App.Types (FirstPartyScansFlag (..), FullFileUploads (FullFileUploads))
import Fossa.API.Types (ApiOpts(..), Organization (..))
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.FossaApiClient (FossaApiClient, getOrganization)
import Control.Effect.Lift (Lift)
import Control.Effect.StickyLogger (StickyLogger)
import Effect.Exec (Exec)
import Control.Carrier.FossaApiClient (runFossaApiClient)
import App.Fossa.ManualDeps (VendoredDependency(..))
import App.Fossa.LicenseScanner (scanVendoredDep)
import Effect.ReadFS (ReadFS, Has)
import Path (Abs, Path, Dir)

runFirstPartyScan ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Exec sig m
  , Has ReadFS sig m
  ) =>
  Path Abs Dir ->
  Maybe ApiOpts ->
  FirstPartyScansFlag ->
  m (Maybe LicenseSourceUnit)
runFirstPartyScan root maybeApiOpts firstPartyScanFlag = do
  -- if we do not have api opts, then we act as if the org defaults to not running first-party scans
  case maybeApiOpts of
    Nothing -> firstPartyScanMain root firstPartyScanFlag False True
    Just apiOpts -> runFossaApiClient apiOpts $ firstPartyScanWithOrgInfo root firstPartyScanFlag

firstPartyScanWithOrgInfo ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has FossaApiClient sig m
  ) =>
  Path Abs Dir ->
  FirstPartyScansFlag ->
  m (Maybe LicenseSourceUnit)
firstPartyScanWithOrgInfo root firstPartyScanFlag = do
  org <- getOrganization
  firstPartyScanMain root firstPartyScanFlag (orgDefaultsToFirstPartyScans org) (orgSupportsFirstPartyScans org)

shouldRunFirstPartyScans :: FirstPartyScansFlag -> Bool -> Bool -> Bool
shouldRunFirstPartyScans firstPartyScansFlag orgDefaultsToFirstParty instanceSupportsFirstPartyScans =
  case (firstPartyScansFlag, orgDefaultsToFirstParty, instanceSupportsFirstPartyScans) of
    (_, _, False) -> False
    (FirstPartyScansOnFromFlag, _, True) -> True
    (FirstPartyScansOffFromFlag, _, True) -> False
    (FirstPartyScansUseDefault, orgDefault, True) -> orgDefault

firstPartyScanMain ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Exec sig m
  , Has ReadFS sig m
  ) =>
  Path Abs Dir ->
  FirstPartyScansFlag ->
  Bool ->
  Bool ->
  m (Maybe LicenseSourceUnit)
firstPartyScanMain base firstPartyScansFlag orgDefaultsToFirstParty orgSupportsFirstPartyScans = do
  let runFirstPartyScans = shouldRunFirstPartyScans firstPartyScansFlag orgDefaultsToFirstParty orgSupportsFirstPartyScans
  let vdep = VendoredDependency "first-party" "." Nothing
  case runFirstPartyScans of
    (True) -> Just <$> scanVendoredDep base Nothing ( FullFileUploads False ) vdep
    (False) -> pure Nothing
