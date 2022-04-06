module Control.Carrier.FossaApiClient.Internal.VSI (assertUserDefinedBinaries) where

import App.Fossa.FossaAPIV1 qualified as API
import App.Fossa.VSI.Fingerprint (Fingerprint, Raw)
import App.Fossa.VSI.IAT.Types qualified as IAT
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Lift)
import Control.Effect.Reader (Reader, ask)
import Fossa.API.Types (ApiOpts)

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
