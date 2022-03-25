{-# LANGUAGE RecordWildCards #-}

module App.Fossa.LicenseScanner (
  licenseScanSourceUnit,
  licenseNoScanSourceUnit,
) where

import App.Fossa.ArchiveUploader (
  VendoredDependency (..),
  arcToLocator,
  duplicateFailureBundle,
  duplicateNames,
  forceVendoredToArchive,
 )
import App.Fossa.EmbeddedBinary (ThemisBins, withThemisAndIndex)
import App.Fossa.FossaAPIV1 qualified as Fossa
import App.Fossa.RunThemis (
  execThemis,
 )
import Control.Effect.Diagnostics (Diagnostics, ToDiagnostic (renderDiagnostic), context, fatal, fatalText, fromMaybe, recover)
import Control.Effect.Lift (Has, Lift)
import Control.Effect.StickyLogger (StickyLogger, logSticky)
import Control.Monad (unless, void)
import Crypto.Hash (Digest, MD5, hash)
import Data.ByteString qualified as B
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)
import Data.String.Conversion (encodeUtf8, toString, toText)
import Data.Text.Extra (showT)
import Effect.Exec (Exec)
import Effect.Logger (Pretty (pretty))
import Fossa.API.Types (
  ApiOpts,
  Archive (Archive, archiveName),
  ArchiveComponents (ArchiveComponents),
  Organization (organizationId),
 )
import Path (Abs, Dir, Path, parseRelDir, (</>))
import Srclib.Types (
  LicenseScanType (CliLicenseScanned),
  LicenseSourceUnit (..),
  LicenseUnit (..),
  Locator (..),
 )

data NoSuccessfulScans = NoSuccessfulScans

instance ToDiagnostic NoSuccessfulScans where
  renderDiagnostic _ = "No native license scans were successful"

newtype NoLicenseResults = NoLicenseResults (Path Abs Dir)

instance ToDiagnostic NoLicenseResults where
  renderDiagnostic (NoLicenseResults path) = "No license results found after scanning directory: " <> pretty (toText path)

runLicenseScanOnDir ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Exec sig m
  ) =>
  Path Abs Dir ->
  m [LicenseUnit]
runLicenseScanOnDir scanDir = withThemisAndIndex $ themisRunner scanDir

themisRunner ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Exec sig m
  ) =>
  Path Abs Dir ->
  ThemisBins ->
  m [LicenseUnit]
themisRunner scanDir themisBins = runThemis themisBins scanDir

runThemis :: (Has Exec sig m, Has Diagnostics sig m) => ThemisBins -> Path Abs Dir -> m [LicenseUnit]
runThemis themisBins scanDir = do
  context "Running license scan binary" $ execThemis themisBins scanDir

md5 :: B.ByteString -> Digest MD5
md5 = hash

scanAndUploadVendoredDep ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Exec sig m
  ) =>
  ApiOpts ->
  Path Abs Dir ->
  VendoredDependency ->
  m (Maybe Archive)
scanAndUploadVendoredDep apiOpts baseDir vdep = context "Processing vendored dependency" $ do
  maybeLicenseUnits <- recover $ scanVendoredDep baseDir vdep
  case maybeLicenseUnits of
    Nothing -> pure Nothing
    Just licenseUnits -> uploadVendoredDep apiOpts vdep licenseUnits

scanVendoredDep ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Exec sig m
  ) =>
  -- ApiOpts ->
  Path Abs Dir ->
  VendoredDependency ->
  m (NonEmpty LicenseUnit)
scanVendoredDep baseDir VendoredDependency{..} = context "Scanning vendored deps for license data" $ do
  logSticky $ "License Scanning '" <> vendoredName <> "' at '" <> vendoredPath <> "'"
  vendoredDepDir <- case parseRelDir $ toString vendoredPath of
    Left err -> fatalText ("Error constructing scan dir for vendored scan: " <> showT err)
    Right val -> pure val
  let cliScanDir = baseDir </> vendoredDepDir
  themisScanResult <- runLicenseScanOnDir cliScanDir
  case NE.nonEmpty themisScanResult of
    Nothing -> fatal $ NoLicenseResults cliScanDir
    Just results -> pure results

uploadVendoredDep ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  ) =>
  ApiOpts ->
  VendoredDependency ->
  NE.NonEmpty LicenseUnit ->
  m (Maybe Archive)
uploadVendoredDep apiOpts VendoredDependency{..} themisScanResult = do
  let licenseSourceUnit =
        LicenseSourceUnit
          { licenseSourceUnitName = vendoredPath
          , licenseSourceUnitType = CliLicenseScanned
          , licenseSourceUnitLicenseUnits = themisScanResult
          }

  depVersion <- case vendoredVersion of
    Nothing -> pure . showT . md5 . encodeUtf8 . show . NE.sort $ themisScanResult
    Just version -> pure version

  signedURL <- Fossa.getSignedLicenseScanURL apiOpts depVersion vendoredName

  logSticky $ "Uploading '" <> vendoredName <> "' to secure S3 bucket"
  void $ Fossa.licenseScanResultUpload signedURL licenseSourceUnit

  pure $ Just $ Archive vendoredName depVersion

-- | licenseScanSourceUnit receives a list of vendored dependencies, a root path, and API settings.
-- Using this information, it license scans each vendored dependency, uploads the license scan results and then queues a build for the dependency.
licenseScanSourceUnit ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Exec sig m
  ) =>
  Path Abs Dir ->
  ApiOpts ->
  NonEmpty VendoredDependency ->
  m (NonEmpty Locator)
licenseScanSourceUnit baseDir apiOpts vendoredDeps = do
  -- Users with many instances of vendored dependencies may accidentally have complete duplicates. Remove them.
  let uniqDeps = NE.nub vendoredDeps

  -- However, users may also have vendored dependencies that have duplicate names but are not complete duplicates.
  -- These aren't valid and can't be automatically handled, so fail the scan with them.
  let duplicates = duplicateNames uniqDeps
  unless (null duplicates) $ fatalText $ duplicateFailureBundle duplicates

  -- At this point, we have a good list of deps, so go for it.
  maybeArchives <- traverse (scanAndUploadVendoredDep apiOpts baseDir) uniqDeps
  archives <- fromMaybe NoSuccessfulScans $ NE.nonEmpty $ catMaybes $ NE.toList maybeArchives

  -- archiveBuildUpload takes archives without Organization information. This orgID is appended when creating the build on the backend.
  -- We don't care about the response here because if the build has already been queued, we get a 401 response.
  void $ Fossa.licenseScanFinalize apiOpts $ ArchiveComponents $ NE.toList archives

  -- The organizationID is needed to prefix each locator name. The FOSSA API automatically prefixes the locator when queuing the build
  -- but not when reading from a source unit.
  orgId <- organizationId <$> Fossa.getOrganization apiOpts

  pure $ NE.map arcToLocator (archivesWithOrganization orgId archives)
  where
    archivesWithOrganization :: Int -> NonEmpty Archive -> NonEmpty Archive
    archivesWithOrganization orgId = NE.map $ includeOrgId orgId

    includeOrgId :: Int -> Archive -> Archive
    includeOrgId orgId arc = arc{archiveName = showT orgId <> "/" <> archiveName arc}

-- | licenseNoScanSourceUnit exists for when users run `fossa analyze -o` and do not upload their source units.
licenseNoScanSourceUnit :: [VendoredDependency] -> [Locator]
licenseNoScanSourceUnit = map (arcToLocator . forceVendoredToArchive)
