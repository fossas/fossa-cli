module Control.Carrier.FossaApiClient.Internal.VSI (
  addFilesToVsiScan,
  assertRevisionBinaries,
  assertUserDefinedBinaries,
  completeVsiScan,
  createVsiScan,
  getVsiInferences,
  getVsiScanAnalysisStatus,
  resolveProjectDependencies,
  resolveUserDefinedBinary,
) where

import App.Fossa.FossaAPIV1 qualified as API
import App.Fossa.VSI.Fingerprint (Fingerprint, Raw)
import App.Fossa.VSI.Fingerprint qualified as Fingerprint
import App.Fossa.VSI.IAT.Types qualified as IAT
import App.Fossa.VSI.Types qualified as VSI
import App.Types (ProjectRevision)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Effect.Lift (Lift)
import Control.Effect.Reader (Reader, ask)
import Data.Map (Map)
import Fossa.API.Types (ApiOpts)
import Path (File, Path, Rel)
import Srclib.Types (Locator)

assertRevisionBinaries ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  Locator ->
  [Fingerprint Raw] ->
  m ()
assertRevisionBinaries meta fingerprints = do
  apiOpts <- ask
  API.assertRevisionBinaries apiOpts meta fingerprints

assertUserDefinedBinaries ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  IAT.UserDefinedAssertionMeta ->
  [Fingerprint Raw] ->
  m ()
assertUserDefinedBinaries meta fingerprints = do
  apiOpts <- ask
  API.assertUserDefinedBinaries apiOpts meta fingerprints

resolveUserDefinedBinary ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  IAT.UserDep ->
  m IAT.UserDefinedAssertionMeta
resolveUserDefinedBinary dep = do
  apiOpts <- ask
  API.resolveUserDefinedBinary apiOpts dep

resolveProjectDependencies ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  VSI.Locator ->
  m [VSI.Locator]
resolveProjectDependencies locator = do
  apiOpts <- ask
  API.resolveProjectDependencies apiOpts locator

createVsiScan ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  ProjectRevision ->
  m VSI.ScanID
createVsiScan rev = fatalText "Not implemented: createVsiScan"

addFilesToVsiScan ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  VSI.ScanID ->
  Map (Path Rel File) Fingerprint.Combined ->
  m ()
addFilesToVsiScan scanId files = fatalText "Not implemented: addFilesToVsiScan"

completeVsiScan ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  VSI.ScanID ->
  m ()
completeVsiScan scanId = fatalText "Not implemented: completeVsiScan"

getVsiScanAnalysisStatus ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  VSI.ScanID ->
  m VSI.AnalysisStatus
getVsiScanAnalysisStatus scanId = fatalText "Not implemented: getVsiScanAnalysisStatus"

getVsiInferences ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  VSI.ScanID ->
  m [Locator]
getVsiInferences scanId = fatalText "Not implemented: getVsiInferences"
