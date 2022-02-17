
module App.Fossa.LicenseScanner (
  licenseScanSourceUnit,
  licenseNoScanSourceUnit,
) where

import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.StickyLogger (StickyLogger, logSticky)
import Control.Effect.Lift

import Effect.Logger (Logger, logDebug)

import App.Fossa.ArchiveUploader
import Srclib.Types (Locator (..))
import Fossa.API.Types
import Path hiding ((</>))

import Data.Text (Text)
import Data.Text qualified as Text

-- archiveUploadSourceUnit receives a list of vendored dependencies, a root path, and API settings.
-- Using this information, it uploads each vendored dependency and queues a build for the dependency.
licenseScanSourceUnit :: (Has Diag.Diagnostics sig m, Has (Lift IO) sig m, Has StickyLogger sig m, Has Logger sig m) => Path Abs Dir -> ApiOpts -> [VendoredDependency] -> m [Locator]
licenseScanSourceUnit baseDir apiOpts vendoredDeps = do
  pure []

-- licenseNoScanSourceUnit exists for when users run `fossa analyze -o` and do not upload their source units.
licenseNoScanSourceUnit :: [VendoredDependency] -> [Locator]
licenseNoScanSourceUnit = map (arcToLocator . forceVendoredToArchive)