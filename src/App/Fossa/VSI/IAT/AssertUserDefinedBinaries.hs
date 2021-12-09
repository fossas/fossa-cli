module App.Fossa.VSI.IAT.AssertUserDefinedBinaries (
  assertUserDefinedBinariesMain,
) where

import App.Fossa.FossaAPIV1 qualified as Fossa
import App.Fossa.VSI.Fingerprint (fingerprintContentsRaw)
import App.Fossa.VSI.IAT.Types (UserDefinedAssertionMeta)
import App.Types (BaseDir (..))
import Control.Algebra (Has)
import Control.Carrier.Diagnostics (Diagnostics, logWithExit_)
import Control.Effect.Lift (Lift)
import Effect.Logger (Logger, Severity, logInfo, withDefaultLogger)
import Effect.ReadFS (ReadFS, runReadFSIO)
import Fossa.API.Types (ApiOpts)
import Path (Abs, Dir, Path)

assertUserDefinedBinariesMain :: Severity -> BaseDir -> ApiOpts -> UserDefinedAssertionMeta -> IO ()
assertUserDefinedBinariesMain logSeverity (BaseDir dir) apiOpts assertion = withDefaultLogger logSeverity . logWithExit_ . runReadFSIO $ do
  assertUserDefinedBinaries dir apiOpts assertion

assertUserDefinedBinaries :: (Has Diagnostics sig m, Has ReadFS sig m, Has (Lift IO) sig m, Has Logger sig m) => Path Abs Dir -> ApiOpts -> UserDefinedAssertionMeta -> m ()
assertUserDefinedBinaries dir apiOpts assertionMeta = do
  logInfo "Fingerprinting directory contents"
  fingerprints <- fingerprintContentsRaw dir

  logInfo "Uploading assertion to FOSSA"
  Fossa.assertUserDefinedBinaries apiOpts assertionMeta fingerprints

  pure ()
