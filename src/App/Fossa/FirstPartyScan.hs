module App.Fossa.FirstPartyScan (
  runFirstPartyScan,
  firstPartyScanWithOrgInfo,
) where

import App.Fossa.LicenseScanner (scanVendoredDep)
import App.Fossa.ManualDeps (VendoredDependency (..))
import App.Types (FirstPartyScansFlag (..), FullFileUploads (FullFileUploads))
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Effect.FossaApiClient (FossaApiClient, getOrganization)
import Control.Effect.Lift (Lift)
import Control.Effect.StickyLogger (StickyLogger)
import Effect.Exec (Exec)
import Effect.ReadFS (Has, ReadFS)
import Fossa.API.Types (ApiOpts (..), Organization (..))
import Path (Abs, Dir, Path)
import Srclib.Types (LicenseSourceUnit)

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
  -- and the FOSSA server does support first-party scans
  case maybeApiOpts of
    Nothing -> firstPartyScanMain root firstPartyScanFlag False True $ FullFileUploads False
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
  let fullFileUploads = FullFileUploads $ orgRequiresFullFileUploads org
  firstPartyScanMain root firstPartyScanFlag (orgDefaultsToFirstPartyScans org) (orgSupportsFirstPartyScans org) fullFileUploads

shouldRunFirstPartyScans :: (Has Diagnostics sig m) => FirstPartyScansFlag -> Bool -> Bool -> m Bool
shouldRunFirstPartyScans firstPartyScansFlag orgDefaultsToFirstParty instanceSupportsFirstPartyScans =
  case (firstPartyScansFlag, orgDefaultsToFirstParty, instanceSupportsFirstPartyScans) of
    (FirstPartyScansOnFromFlag, _, False) -> fatalText "You provided the --experimental-force-first-party-scans flag but the FOSSA server does not support first-party scans"
    (_, _, False) -> pure False
    (FirstPartyScansOnFromFlag, _, True) -> pure True
    (FirstPartyScansOffFromFlag, _, True) -> pure False
    (FirstPartyScansUseDefault, orgDefault, True) -> pure orgDefault

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
  FullFileUploads ->
  m (Maybe LicenseSourceUnit)
firstPartyScanMain base firstPartyScansFlag orgDefaultsToFirstParty orgSupportsFirstPartyScans fullFileUploads = do
  runFirstPartyScans <- shouldRunFirstPartyScans firstPartyScansFlag orgDefaultsToFirstParty orgSupportsFirstPartyScans
  let vdep = VendoredDependency "first-party" "." Nothing
  case runFirstPartyScans of
    (True) -> Just <$> scanVendoredDep base Nothing fullFileUploads vdep
    (False) -> pure Nothing
