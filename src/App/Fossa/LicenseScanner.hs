{-# LANGUAGE RecordWildCards #-}

module App.Fossa.LicenseScanner (
  licenseScanSourceUnit,
  licenseNoScanSourceUnit,
) where

import App.Fossa.RunThemis (
  ThemisCLIOpts (..),
  execThemis,
  generateThemisOpts,
 )

import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.StickyLogger (StickyLogger, logSticky)
import Control.Effect.Lift

import Crypto.Hash (Digest, MD5, hash)
import Data.ByteString qualified as B
import Effect.Exec (Exec, runExecIO)
import Effect.Logger (Logger, logDebug)

import App.Fossa.ArchiveUploader
import App.Fossa.FossaAPIV1 qualified as Fossa
import Control.Effect.Diagnostics (Diagnostics, context, fatalText)
import Control.Monad (unless)
import Data.List (nub)
import Fossa.API.Types
import Path hiding ((</>))
import Prettyprinter (Pretty (pretty))
import Srclib.Types (
  LicenseSourceUnit (..),
  LicenseUnit (..),
  Locator (..),
 )

import Data.String.Conversion (encodeUtf8, toString, toText)
import Data.Text (Text)

import App.Fossa.EmbeddedBinary (BinaryPaths, ThemisBins, withThemisAndIndex)

runLicenseScanOnDir ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  ThemisCLIOpts ->
  m [LicenseUnit]
runLicenseScanOnDir opts = withThemisAndIndex (themisRunner opts)

themisRunner ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  ThemisCLIOpts ->
  ThemisBins ->
  m [LicenseUnit]
themisRunner opts themisBins = do
  res <- runExecIO $ runThemis themisBins opts
  logDebug $ pretty $ "themis JSON:\n" ++ show res
  pure res

runThemis :: (Has Exec sig m, Has Diagnostics sig m, Has Logger sig m) => ThemisBins -> ThemisCLIOpts -> m [LicenseUnit]
runThemis themisBins opts = do
  context "Running license scan binary" $ execThemis themisBins opts

scanAndUploadVendoredDeps :: (Has Diag.Diagnostics sig m, Has (Lift IO) sig m, Has StickyLogger sig m, Has Logger sig m) => ApiOpts -> Path Abs Dir -> [VendoredDependency] -> m [Archive]
scanAndUploadVendoredDeps apiOpts baseDir = traverse (scanAndUpload apiOpts baseDir)

md5 :: B.ByteString -> Digest MD5
md5 = hash

scanAndUpload :: (Has Diag.Diagnostics sig m, Has (Lift IO) sig m, Has StickyLogger sig m, Has Logger sig m) => ApiOpts -> Path Abs Dir -> VendoredDependency -> m Archive
scanAndUpload apiOpts baseDir VendoredDependency{..} = context "compressing and uploading vendored deps" $ do
  logSticky $ "Scanning '" <> vendoredName <> "' at '" <> vendoredPath <> "'"
  vendoredDepDir <- case parseRelDir (toString vendoredPath) of
    Left _ -> fatalText "Error constructing scan dir for vendored scan"
    Right val -> pure val
  let cliOpts = generateThemisOpts baseDir vendoredDepDir
  logDebug "about to start themis scan"
  themisScanResult <- runLicenseScanOnDir cliOpts
  logDebug "back from themis scan"
  logDebug "themis scan results: "
  logDebug $ pretty $ show themisScanResult
  let licenseSourceUnit =
        LicenseSourceUnit
          { licenseSourceUnitName = vendoredPath
          , licenseSourceUnitType = "cli-license-scanned"
          , licenseSourceUnitLicenseUnits = themisScanResult
          }

  depVersion <- case vendoredVersion of
    Nothing -> pure (toText (show (md5 $ encodeUtf8 (show themisScanResult))))
    Just version -> pure version

  logDebug "about to get signed URL"
  signedURL <- Fossa.getSignedLicenseScanURL apiOpts depVersion vendoredName

  logSticky $ "Uploading '" <> vendoredName <> "' to secure S3 bucket"
  _ <- Fossa.licenseScanResultUpload signedURL licenseSourceUnit

  pure $ Archive vendoredName depVersion

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
  archives <- scanAndUploadVendoredDeps apiOpts baseDir uniqDeps

  -- archiveBuildUpload takes archives without Organization information. This orgID is appended when creating the build on the backend.
  -- We don't care about the response here because if the build has already been queued, we get a 401 response.
  _ <- Fossa.licenseScanFinalize apiOpts (ArchiveComponents archives)

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
