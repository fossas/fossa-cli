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

import App.Fossa.VSI.Fingerprint (Fingerprint, Raw)
import App.Fossa.VSI.Fingerprint qualified as Fingerprint
import App.Fossa.VSI.IAT.Types qualified as IAT
import App.Fossa.VSI.Types qualified as VSI
import App.Types (ProjectRevision)
import Control.Algebra (Has)
import Control.Carrier.FossaApiClient.Internal.FossaAPIV1 qualified as API
import Control.Effect.Reader (Reader, ask)
import Data.Map (Map)
import Fossa.API.Types (ApiOpts)
import Path (File, Path, Rel)
import Srclib.Types (Locator)

assertRevisionBinaries ::
  ( API.APIClientEffs sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  Locator ->
  [Fingerprint Raw] ->
  m ()
assertRevisionBinaries meta fingerprints = do
  apiOpts <- ask
  API.assertRevisionBinaries apiOpts meta fingerprints

assertUserDefinedBinaries ::
  ( API.APIClientEffs sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  IAT.UserDefinedAssertionMeta ->
  [Fingerprint Raw] ->
  m ()
assertUserDefinedBinaries meta fingerprints = do
  apiOpts <- ask
  API.assertUserDefinedBinaries apiOpts meta fingerprints

resolveUserDefinedBinary ::
  ( API.APIClientEffs sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  IAT.UserDep ->
  m IAT.UserDefinedAssertionMeta
resolveUserDefinedBinary dep = do
  apiOpts <- ask
  API.resolveUserDefinedBinary apiOpts dep

resolveProjectDependencies ::
  ( API.APIClientEffs sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  VSI.Locator ->
  m [VSI.Locator]
resolveProjectDependencies locator = do
  apiOpts <- ask
  API.resolveProjectDependencies apiOpts locator

createVsiScan ::
  ( API.APIClientEffs sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  ProjectRevision ->
  m VSI.ScanID
createVsiScan revision = do
  apiOpts <- ask
  API.vsiCreateScan apiOpts revision

addFilesToVsiScan ::
  ( API.APIClientEffs sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  VSI.ScanID ->
  Map (Path Rel File) Fingerprint.Combined ->
  m ()
addFilesToVsiScan scanId files = do
  apiOpts <- ask
  API.vsiAddFilesToScan apiOpts scanId files

completeVsiScan ::
  ( API.APIClientEffs sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  VSI.ScanID ->
  m ()
completeVsiScan scanId = do
  apiOpts <- ask
  API.vsiCompleteScan apiOpts scanId

getVsiScanAnalysisStatus ::
  ( API.APIClientEffs sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  VSI.ScanID ->
  m VSI.AnalysisStatus
getVsiScanAnalysisStatus scanId = do
  apiOpts <- ask
  API.vsiScanAnalysisStatus apiOpts scanId

getVsiInferences ::
  ( API.APIClientEffs sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  VSI.ScanID ->
  m VSI.VsiExportedInferencesBody
getVsiInferences scanId = do
  apiOpts <- ask
  API.vsiDownloadInferences apiOpts scanId
