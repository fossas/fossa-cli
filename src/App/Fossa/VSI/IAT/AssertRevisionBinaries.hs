module App.Fossa.VSI.IAT.AssertRevisionBinaries (
  assertRevisionBinaries,
) where

import App.Fossa.VSI.Fingerprint (fingerprintContentsRaw)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.FossaApiClient qualified as API
import Control.Effect.Lift (Lift)
import Effect.Logger (Logger, logInfo)
import Effect.ReadFS (ReadFS)
import Path (Abs, Dir, Path)
import Srclib.Types (Locator)

assertRevisionBinaries ::
  ( Has Diagnostics sig m
  , Has ReadFS sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has API.FossaApiClient sig m
  ) =>
  Path Abs Dir ->
  Locator ->
  m ()
assertRevisionBinaries dir locator = do
  logInfo "Fingerprinting assertion directory contents"
  fingerprints <- fingerprintContentsRaw dir

  logInfo "Uploading assertion to FOSSA"
  API.assertRevisionBinaries locator fingerprints

  pure ()
