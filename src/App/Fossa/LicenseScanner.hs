
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


scanAndUploadVendoredDeps :: (Has Diag.Diagnostics sig m, Has (Lift IO) sig m, Has StickyLogger sig m, Has Logger sig m) => ApiOpts -> [VendoredDependency] -> Path Abs Dir -> Path Abs Dir -> m [Archive]
scanAndUploadVendoredDeps apiOpts deps arcDir tmpDir = traverse (scanAndUpload apiOpts arcDir tmpDir) deps

scanAndUpload :: (Has Diag.Diagnostics sig m, Has (Lift IO) sig m, Has StickyLogger sig m, Has Logger sig m) => ApiOpts -> Path Abs Dir -> Path Abs Dir -> VendoredDependency -> m Archive
scanAndUpload apiOpts arcDir tmpDir VendoredDependency{..} = context "compressing and uploading vendored deps" $ do
  logSticky $ "Scanning '" <> vendoredName <> "' at '" <> vendoredPath <> "'"
  scanResult <- licenseScanDir (toString vendoredPath)

  depVersion <- case vendoredVersion of
    Nothing -> sendIO $ hashFile scanResult
    Just version -> pure version

  signedURL <- Fossa.getSignedURL apiOpts depVersion vendoredName

  logSticky $ "Uploading '" <> vendoredName <> "' to secure S3 bucket"
  res <- Fossa.archiveUpload signedURL scanResult
  logDebug $ pretty $ show res

  pure $ Archive vendoredName depVersion

scanResult :: Path -> LicenseScanResult
scanResult vendoredPath = do
  pure []

-- archiveUploadSourceUnit receives a list of vendored dependencies, a root path, and API settings.
-- Using this information, it uploads each vendored dependency and queues a build for the dependency.
licenseScanSourceUnit :: (Has Diag.Diagnostics sig m, Has (Lift IO) sig m, Has StickyLogger sig m, Has Logger sig m) => Path Abs Dir -> ApiOpts -> [VendoredDependency] -> m [Locator]
licenseScanSourceUnit baseDir apiOpts vendoredDeps = do
  -- Users with many instances of vendored dependencies may accidentally have complete duplicates. Remove them.
  let uniqDeps = nub vendoredDeps

  -- However, users may also have vendored dependencies that have duplicate names but are not complete duplicates.
  -- These aren't valid and can't be automatically handled, so fail the scan with them.
  let duplicates = duplicateNames uniqDeps
  unless (null duplicates) $ Diag.fatalText $ duplicateFailureBundle duplicates

  -- At this point, we have a good list of deps, so go for it.
  archives <- withSystemTempDir "fossa-temp" (scanAndUploadVendoredDeps apiOpts uniqDeps baseDir)

  -- archiveBuildUpload takes archives without Organization information. This orgID is appended when creating the build on the backend.
  -- We don't care about the response here because if the build has already been queued, we get a 401 response.
  _ <- Fossa.archiveBuildUpload apiOpts (ArchiveComponents archives)

  -- The organizationID is needed to prefix each locator name. The FOSSA API automatically prefixes the locator when queuing the build
  -- but not when reading from a source unit.
  Fossa.Organization orgId _ <- Fossa.getOrganization apiOpts

  let updateArcName :: Text -> Archive -> Archive
      updateArcName updateText arc = arc{archiveName = updateText <> "/" <> archiveName arc}
      archivesWithOrganization = updateArcName (toText $ show orgId) <$> archives

  pure $ arcToLocator <$> archivesWithOrganization

-- licenseNoScanSourceUnit exists for when users run `fossa analyze -o` and do not upload their source units.
licenseNoScanSourceUnit :: [VendoredDependency] -> [Locator]
licenseNoScanSourceUnit = map (arcToLocator . forceVendoredToArchive)