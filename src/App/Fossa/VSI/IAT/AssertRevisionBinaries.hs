module App.Fossa.VSI.IAT.AssertRevisionBinaries (
  assertRevisionBinaries,
) where

import App.Fossa.FossaAPIV1 qualified as Fossa
import App.Fossa.VSI.Fingerprint (fingerprintContentsRaw)
import Control.Algebra (Has)
import Control.Carrier.Diagnostics (Diagnostics)
import Control.Effect.Lift (Lift)
import Effect.Logger (Logger, logInfo)
import Effect.ReadFS (ReadFS)
import Fossa.API.Types (ApiOpts)
import Path (Abs, Dir, Path)
import Srclib.Types (Locator)

assertRevisionBinaries :: (Has Diagnostics sig m, Has ReadFS sig m, Has (Lift IO) sig m, Has Logger sig m) => Path Abs Dir -> ApiOpts -> Locator -> m ()
assertRevisionBinaries dir apiOpts locator = do
  logInfo "Fingerprinting assertion directory contents"
  fingerprints <- fingerprintContentsRaw dir

  logInfo "Uploading assertion to FOSSA"
  Fossa.assertRevisionBinaries apiOpts locator fingerprints

  pure ()
