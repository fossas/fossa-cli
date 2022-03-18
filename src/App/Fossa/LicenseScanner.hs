{-# LANGUAGE RecordWildCards #-}

module App.Fossa.LicenseScanner (
  licenseScanSourceUnit,
  licenseNoScanSourceUnit,
) where

import App.Fossa.RunThemis (
  execThemis,
 )

import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.StickyLogger (StickyLogger, logSticky)
import Control.Effect.Lift

import App.Fossa.ArchiveUploader (
  VendoredDependency (..),
  arcToLocator,
  duplicateFailureBundle,
  duplicateNames,
  forceVendoredToArchive,
 )
import App.Fossa.FossaAPIV1 qualified as Fossa
import Control.Effect.Diagnostics (Diagnostics, context, fatalText)
import Control.Monad (unless, void)
import Crypto.Hash (Digest, MD5, hash)
import Data.ByteString qualified as B
import Data.List (nub)
import Data.String.Conversion (encodeUtf8, toString, toText)
import Effect.Exec (Exec, runExecIO)
import Fossa.API.Types
import Path
import Srclib.Types (
  LicenseScanType (CliLicenseScanned),
  LicenseSourceUnit (..),
  LicenseUnit (..),
  Locator (..),
 )

import App.Fossa.EmbeddedBinary (ThemisBins, withThemisAndIndex)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)

runLicenseScanOnDir ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  ) =>
  Path Abs Dir ->
  m [LicenseUnit]
runLicenseScanOnDir scanDir = withThemisAndIndex $ themisRunner scanDir

themisRunner ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  ThemisBins ->
  m [LicenseUnit]
themisRunner scanDir themisBins = do
  runExecIO $ runThemis themisBins scanDir

runThemis :: (Has Exec sig m, Has Diagnostics sig m) => ThemisBins -> Path Abs Dir -> m [LicenseUnit]
runThemis themisBins scanDir = do
  context "Running license scan binary" $ execThemis themisBins scanDir

md5 :: B.ByteString -> Digest MD5
md5 = hash

scanAndUploadVendoredDep ::
  ( Has Diag.Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  ) =>
  ApiOpts ->
  Path Abs Dir ->
  VendoredDependency ->
  m (Maybe Archive)
scanAndUploadVendoredDep apiOpts baseDir vendoredDeps@VendoredDependency{..} = context "compressing and uploading vendored deps" $ do
  logSticky $ "License Scanning '" <> vendoredName <> "' at '" <> vendoredPath <> "'"
  vendoredDepDir <- case parseRelDir $ toString vendoredPath of
    Left _ -> fatalText "Error constructing scan dir for vendored scan"
    Right val -> pure val
  let cliScanDir = baseDir </> vendoredDepDir
  themisScanResult <- runLicenseScanOnDir cliScanDir
  -- TODO: log something when we get [] back for themisScanResult
  case themisScanResult of
    [] -> pure Nothing
    _ -> uploadVendoredDep apiOpts vendoredDeps (NE.fromList themisScanResult)

uploadVendoredDep ::
  ( Has Diag.Diagnostics sig m
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
    Nothing -> pure $ toText . show . md5 . encodeUtf8 . show $ themisScanResult
    Just version -> pure version

  signedURL <- Fossa.getSignedLicenseScanURL apiOpts depVersion vendoredName

  logSticky $ "Uploading '" <> vendoredName <> "' to secure S3 bucket"
  void $ Fossa.licenseScanResultUpload signedURL licenseSourceUnit

  pure $ Just $ Archive vendoredName depVersion

-- | licenseScanSourceUnit receives a list of vendored dependencies, a root path, and API settings.
-- Using this information, it license scans each vendored dependency, uploads the license scan results and then queues a build for the dependency.
licenseScanSourceUnit ::
  ( Has Diag.Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  ) =>
  Path Abs Dir ->
  ApiOpts ->
  [VendoredDependency] ->
  m [Locator]
licenseScanSourceUnit baseDir apiOpts vendoredDeps = do
  -- Users with many instances of vendored dependencies may accidentally have complete duplicates. Remove them.
  let uniqDeps = nub vendoredDeps

  -- However, users may also have vendored dependencies that have duplicate names but are not complete duplicates.
  -- These aren't valid and can't be automatically handled, so fail the scan with them.
  let duplicates = duplicateNames uniqDeps
  unless (null duplicates) $ Diag.fatalText $ duplicateFailureBundle duplicates

  -- At this point, we have a good list of deps, so go for it.
  maybeArchives <- traverse (scanAndUploadVendoredDep apiOpts baseDir) uniqDeps
  let archives = catMaybes maybeArchives

  -- archiveBuildUpload takes archives without Organization information. This orgID is appended when creating the build on the backend.
  -- We don't care about the response here because if the build has already been queued, we get a 401 response.
  void $ Fossa.licenseScanFinalize apiOpts $ ArchiveComponents archives

  -- The organizationID is needed to prefix each locator name. The FOSSA API automatically prefixes the locator when queuing the build
  -- but not when reading from a source unit.
  Fossa.Organization orgId _ <- Fossa.getOrganization apiOpts

  pure $ map arcToLocator (archivesWithOrganization orgId archives)
  where
    archivesWithOrganization :: Int -> [Archive] -> [Archive]
    archivesWithOrganization orgId = map (includeOrgId orgId)

    includeOrgId :: Int -> Archive -> Archive
    includeOrgId orgId arc = arc{archiveName = toText (show orgId) <> "/" <> archiveName arc}

-- | licenseNoScanSourceUnit exists for when users run `fossa analyze -o` and do not upload their source units.
licenseNoScanSourceUnit :: [VendoredDependency] -> [Locator]
licenseNoScanSourceUnit = map (arcToLocator . forceVendoredToArchive)
