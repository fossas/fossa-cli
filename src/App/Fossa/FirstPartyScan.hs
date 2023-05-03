module App.Fossa.FirstPartyScan (
  runFirstPartyScan,
) where
import Srclib.Types (SourceUnit(..))
import Data.List.NonEmpty (NonEmpty)
import App.Types (FirstPartyScansFlag (..))
import Fossa.API.Types (ApiOpts(..), Organization (..))
import Control.Effect.Diagnostics (Diagnostics, context, fatalText)
import Control.Effect.FossaApiClient (FossaApiClient, getOrganization)
import Control.Effect.Lift (Has, Lift)
import Control.Effect.StickyLogger (StickyLogger)
import Effect.Exec (Exec)
import Effect.Logger (Logger)
import Effect.ReadFS (ReadFS, doesFileExist, readContentsJson, readContentsYaml)
import Path (Abs, Dir, File, Path, mkRelFile, (</>))
import Control.Carrier.FossaApiClient (runFossaApiClient)

runFirstPartyScan ::
  ( Has Diagnostics sig m
  , Has ReadFS sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Logger sig m
  , Has Exec sig m
  ) =>
  Path Abs Dir ->
  Maybe ApiOpts ->
  FirstPartyScansFlag ->
  m (Maybe ([]SourceUnit))
runFirstPartyScan root maybeApiOpts firstPartyScanFlag = do
  -- if we do not have api opts, then we act as if the org defaults to not running first-party scans
  case maybeApiOpts of
    Nothing -> firstPartyScanMain root $ shouldRunFirstPartyScans firstPartyScanFlag False
    Just apiOpts -> runFossaApiClient apiOpts $ firstPartyScanWithOrgInfo root firstPartyScanFlag

firstPartyScanWithOrgInfo ::
  ( Has Diagnostics sig m
  , Has ReadFS sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Logger sig m
  , Has Exec sig m
  , Has FossaApiClient sig m
  ) =>
  Path Abs Dir ->
  FirstPartyScansFlag ->
  m (Maybe ([]SourceUnit))
firstPartyScanWithOrgInfo root firstPartyScanFlags = do
  org <- getOrganization
  let runFirstPartyScans = shouldRunFirstPartyScans firstPartyScanFlags $ orgDefaultsToFirstPartyScans org
  firstPartyScanMain root runFirstPartyScans



shouldRunFirstPartyScans :: FirstPartyScansFlag -> Bool -> Bool
shouldRunFirstPartyScans firstPartyScansFlag orgDefaultsToFirstParty =
  case (firstPartyScansFlag, orgDefaultsToFirstParty) of
    (FirstPartyScansOnFromFlag, _) -> True
    (FirstPartyScansOffFromFlag, _) -> False
    (FirstPartyScansUseDefault, orgDefault) -> orgDefault

firstPartyScanMain ::
  ( Has Diagnostics sig m
  , Has ReadFS sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Logger sig m
  , Has Exec sig m
  ) =>
  Path Abs Dir ->
  Bool ->
  m (Maybe ([]SourceUnit))
firstPartyScanMain base runFirstPartyScans =
  pure Nothing
