module Control.Carrier.FossaApiClient.Internal.VSI (
  assertRevisionBinaries,
  assertUserDefinedBinaries,
  resolveProjectDependencies,
  resolveUserDefinedBinary,
) where

import App.Fossa.FossaAPIV1 qualified as API
import App.Fossa.VSI.Fingerprint (Fingerprint, Raw)
import App.Fossa.VSI.IAT.Types qualified as IAT
import App.Fossa.VSI.Types qualified as VSI
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Lift)
import Control.Effect.Reader (Reader, ask)
import Fossa.API.Types (ApiOpts)
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
