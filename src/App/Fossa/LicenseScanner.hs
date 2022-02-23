{-# LANGUAGE RecordWildCards #-}

module App.Fossa.LicenseScanner (
  licenseScanSourceUnit,
  licenseNoScanSourceUnit,
) where

import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.StickyLogger (StickyLogger, logSticky)
import Control.Effect.Lift
import Crypto.Hash (hash)
import Effect.Logger (Logger, logDebug)

import App.Fossa.FossaAPIV1 qualified as Fossa
import App.Fossa.ArchiveUploader
import Srclib.Types (Locator (..))
import Fossa.API.Types
import Path hiding ((</>))
import Control.Effect.Diagnostics (context)
import Control.Monad (unless)
import Data.List (nub)
import Prettyprinter (Pretty (pretty))

import Data.Text (Text)
import Data.Text qualified as Text
import Data.String.Conversion (encodeUtf8, toText)

runLicenseScanOnDir :: (Has Diag.Diagnostics sig m, Has (Lift IO) sig m, Has StickyLogger sig m, Has Logger sig m) => Text -> m Text
runLicenseScanOnDir dir = pure dir

scanAndUploadVendoredDeps :: (Has Diag.Diagnostics sig m, Has (Lift IO) sig m, Has StickyLogger sig m, Has Logger sig m) => ApiOpts -> [VendoredDependency] -> m [Archive]
scanAndUploadVendoredDeps apiOpts = traverse (scanAndUpload apiOpts)

scanAndUpload :: (Has Diag.Diagnostics sig m, Has (Lift IO) sig m, Has StickyLogger sig m, Has Logger sig m) => ApiOpts -> VendoredDependency -> m Archive
scanAndUpload apiOpts VendoredDependency{..} = context "compressing and uploading vendored deps" $ do
  logSticky $ "Scanning '" <> vendoredName <> "' at '" <> vendoredPath <> "'"
  scanResult <- runLicenseScanOnDir vendoredPath
  -- let scanResultBS = encodeUtf8 scanResult
  --     scanHash = hash scanResultBS

  depVersion <- case vendoredVersion of
    -- Nothing -> pure (toText (show (hash (encodeUtf8 scanResult))))
    -- Nothing -> pure "aaa"
    -- TODO: this is not correct
    -- Nothing -> pure (toText (show scanHash))
    Nothing -> pure (toText (show "aaa"))
    Just version -> pure version

  signedURL <- Fossa.getSignedURL apiOpts depVersion vendoredName

  logSticky $ "Uploading '" <> vendoredName <> "' to secure S3 bucket"
  res <- Fossa.licenseScanResultUpload signedURL scanResult
  logDebug $ pretty $ show res

  pure $ Archive vendoredName depVersion

scanResult :: (Has Diag.Diagnostics sig m, Has (Lift IO) sig m, Has StickyLogger sig m, Has Logger sig m) => Text -> m Text
scanResult vendoredPath = do
  pure ""

-- archiveUploadSourceUnit receives a list of vendored dependencies, a root path, and API settings.
-- Using this information, it license scans each vendored dependency, uploads the license scan results and then queues a build for the dependency.
licenseScanSourceUnit :: (Has Diag.Diagnostics sig m, Has (Lift IO) sig m, Has StickyLogger sig m, Has Logger sig m) => Path Abs Dir -> ApiOpts -> [VendoredDependency] -> m [Locator]
licenseScanSourceUnit baseDir apiOpts vendoredDeps = do
  -- Users with many instances of vendored dependencies may accidentally have complete duplicates. Remove them.
  let uniqDeps = nub vendoredDeps

  -- However, users may also have vendored dependencies that have duplicate names but are not complete duplicates.
  -- These aren't valid and can't be automatically handled, so fail the scan with them.
  let duplicates = duplicateNames uniqDeps
  unless (null duplicates) $ Diag.fatalText $ duplicateFailureBundle duplicates

  -- At this point, we have a good list of deps, so go for it.
  archives <- scanAndUploadVendoredDeps apiOpts uniqDeps

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
