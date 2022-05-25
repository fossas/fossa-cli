{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.LicenseScanner (
  licenseScanSourceUnit,
  combineLicenseUnits,
  scanVendoredDep,
) where

import App.Fossa.EmbeddedBinary (ThemisBins, withThemisAndIndex)
import App.Fossa.RunThemis (
  execThemis,
 )
import App.Fossa.VendoredDependency (
  VendoredDependency (..),
  VendoredDependencyScanMode (..),
  arcToLocator,
  compressFile,
  dedupVendoredDeps,
  forceVendoredToArchive,
  hashFile,
 )
import Control.Carrier.Finally (Finally, runFinally)
import Control.Carrier.FossaApiClient.Internal.FossaAPIV1 (renderLocatorUrl)
import Control.Effect.Diagnostics (Diagnostics, ToDiagnostic (renderDiagnostic), context, fatal, fromMaybe, recover)
import Control.Effect.FossaApiClient (
  FossaApiClient,
  PackageRevision (..),
  finalizeLicenseScan,
  getAnalyzedRevisions,
  getOrganization,
  getSignedLicenseScanUrl,
  uploadLicenseScanResult,
 )
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Path (withSystemTempDir)
import Control.Effect.StickyLogger (StickyLogger, logSticky)
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)
import Data.Semigroup (Any (..))
import Data.String.Conversion (toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Extra (showT)
import Discovery.Archive (withArchive')
import Discovery.Walk (WalkStep (WalkContinue, WalkStop), walk')
import Effect.Exec (Exec)
import Effect.Logger (Logger, logDebug)
import Effect.ReadFS (
  Has,
  ReadFS,
  SomePath (SomeDir, SomeFile),
  resolvePath',
 )
import Fossa.API.Types (
  Archive (Archive, archiveName),
  ArchiveComponents (ArchiveComponents),
  OrgId,
  Organization (organizationId),
 )
import Path (Abs, Dir, File, Path, SomeBase (Abs, Rel), fileExtension, parent, (</>))
import Path.Extra (tryMakeRelative)
import Prettyprinter (Pretty (pretty), squotes)
import Srclib.Types (
  LicenseScanType (CliLicenseScanned),
  LicenseSourceUnit (..),
  LicenseUnit (..),
  Locator (..),
 )

data LicenseScanErr
  = NoSuccessfulScans
  | NoLicenseResults (Path Abs Dir)
  | EmptyDirectory (Path Abs Dir)
  | EmptyArchive (Path Abs File)
  | UnsupportedArchive (Path Abs File)

instance ToDiagnostic LicenseScanErr where
  renderDiagnostic NoSuccessfulScans = "No native license scans were successful"
  renderDiagnostic (NoLicenseResults path) = "No license results found after scanning directory: " <> pretty (toText path)
  renderDiagnostic (EmptyDirectory path) = "vendored-dependencies path has no files and cannot be scanned: " <> pretty (toString path)
  renderDiagnostic (EmptyArchive path) = "vendored-dependencies archive contains no files and cannot be scanned: " <> pretty (toString path)
  renderDiagnostic (UnsupportedArchive path) = case fileExtension path of
    Just ext -> "fossa-cli does not support archives of type " <> squotes (pretty ext) <> ": " <> pretty (toString path)
    Nothing -> "fossa-cli does not support archives without file extensions: " <> pretty (toString path)

newtype ScannableArchive = ScannableArchive {scanFile :: Path Abs File} deriving (Eq, Ord, Show)

newtype NeedScanningDeps = NeedScanningDeps {needScanningDeps :: [VendoredDependency]}
  deriving (Eq, Ord, Show)

newtype SkippableDeps = SkippableDeps {skippableDeps :: [VendoredDependency]}
  deriving (Eq, Ord, Show)

runLicenseScanOnDir ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Exec sig m
  , Has ReadFS sig m
  ) =>
  Text ->
  Path Abs Dir ->
  m [LicenseUnit]
runLicenseScanOnDir pathPrefix scanDir = do
  -- license scan the root directory
  rootDirUnits <- withThemisAndIndex $ themisRunner pathPrefix scanDir
  -- recursively unpack archives and license scan them too
  otherArchiveUnits <- runFinally $ recursivelyScanArchives pathPrefix scanDir
  -- when we scan multiple archives, we need to combine the results
  pure $ combineLicenseUnits (rootDirUnits <> otherArchiveUnits)

recursivelyScanArchives ::
  ( Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Finally sig m
  , Has Exec sig m
  ) =>
  Text ->
  Path Abs Dir ->
  m [LicenseUnit]
recursivelyScanArchives pathPrefix dir = flip walk' dir $
  \_ _ files -> do
    let process file unpackedDir = do
          let updatedPathPrefix = pathPrefix <> getPathPrefix dir (parent file)
          currentDirResults <- withThemisAndIndex $ themisRunner updatedPathPrefix unpackedDir
          recursiveResults <- recursivelyScanArchives updatedPathPrefix unpackedDir
          pure $ currentDirResults <> recursiveResults
    -- withArchive' emits Nothing when archive type is not supported.
    archives <- traverse (\file -> withArchive' file (process file)) files
    pure (concat (catMaybes archives), WalkContinue)

-- When we recursively scan archives, we end up with an array of LicenseUnits that may have multiple entries for a single license.
-- The license is contained in the licenseUnitName field of the LicenseUnit.
-- E.g. we might end up with an MIT LicenseUnit from scanning foo/bar.tar.gz and another MIT LicenseUnit from scanning foo/baz.tar.gz.
-- To combine these, we find LicenseUnits with the same licenseUnitName and merge them by concatenating their licenseUnitFile and licenseUnitData lists
-- This is safe because the licenseUnitType will always be "LicenseUnit", licenseUnitDir will always be ""
-- and licenseUnitInfo will just contain a LicenseUnitInfo entry with an empty description.
combineLicenseUnits :: [LicenseUnit] -> [LicenseUnit]
combineLicenseUnits units =
  HM.elems licenseUnitMap
  where
    mergeTwoUnits :: LicenseUnit -> LicenseUnit -> LicenseUnit
    mergeTwoUnits unitA unitB =
      unitA{licenseUnitFiles = combinedFiles, licenseUnitData = combinedData}
      where
        combinedFiles = NE.sort $ NE.nub $ licenseUnitFiles unitA <> licenseUnitFiles unitB
        combinedData = NE.sort $ NE.nub $ licenseUnitData unitA <> licenseUnitData unitB

    addUnit :: LicenseUnit -> HM.HashMap Text LicenseUnit -> HM.HashMap Text LicenseUnit
    addUnit unit = HM.insertWith mergeTwoUnits (licenseUnitName unit) unit

    licenseUnitMap = foldr addUnit HM.empty units

themisRunner ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Exec sig m
  ) =>
  Text ->
  Path Abs Dir ->
  ThemisBins ->
  m [LicenseUnit]
themisRunner pathPrefix scanDir themisBins = runThemis themisBins pathPrefix scanDir

runThemis :: (Has Exec sig m, Has Diagnostics sig m) => ThemisBins -> Text -> Path Abs Dir -> m [LicenseUnit]
runThemis themisBins pathPrefix scanDir = do
  context "Running license scan binary" $ execThemis themisBins pathPrefix scanDir

calculateVendoredHash :: Path Abs Dir -> Text -> Path Abs Dir -> IO Text
calculateVendoredHash baseDir vendoredPath tmpDir = do
  compressedFilePath <- sendIO $ compressFile tmpDir baseDir (toString vendoredPath)
  hashFile compressedFilePath

scanAndUploadVendoredDep ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has FossaApiClient sig m
  ) =>
  Path Abs Dir ->
  VendoredDependency ->
  m (Maybe Archive)
scanAndUploadVendoredDep baseDir vdep = context "Processing vendored dependency" $ do
  maybeLicenseUnits <- recover $ scanVendoredDep baseDir vdep
  case maybeLicenseUnits of
    Nothing -> pure Nothing
    Just licenseUnits -> uploadVendoredDep baseDir vdep licenseUnits

scanVendoredDep ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Exec sig m
  , Has ReadFS sig m
  ) =>
  Path Abs Dir ->
  VendoredDependency ->
  m LicenseSourceUnit
scanVendoredDep baseDir VendoredDependency{..} = context "Scanning vendored deps for license data" $ do
  logSticky $ "License Scanning '" <> vendoredName <> "' at '" <> vendoredPath <> "'"
  scanPath <- resolvePath' baseDir $ toString vendoredPath
  licenseUnits <- case scanPath of
    SomeFile (Abs path) -> scanArchive baseDir $ ScannableArchive path
    SomeFile (Rel path) -> scanArchive baseDir . ScannableArchive $ baseDir </> path
    SomeDir (Abs path) -> scanDirectory Nothing (getPathPrefix baseDir path) path
    SomeDir (Rel path) -> scanDirectory Nothing (toText path) (baseDir </> path)
  pure $ LicenseSourceUnit vendoredPath CliLicenseScanned licenseUnits

getPathPrefix :: Path Abs Dir -> Path Abs t -> Text
getPathPrefix baseDir scanPath = do
  case tryMakeRelative baseDir scanPath of
    Path.Abs _ -> Text.empty
    Path.Rel path -> toText path

scanArchive ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has StickyLogger sig m
  ) =>
  Path Abs Dir ->
  ScannableArchive ->
  m (NonEmpty LicenseUnit)
scanArchive baseDir file = runFinally $ do
  -- withArchive' emits Nothing when archive type is not supported.
  logSticky $ "scanning archive at " <> toText (scanFile file)
  result <- withArchive' (scanFile file) (scanDirectory (Just file) pathPrefix)
  case result of
    Nothing -> fatal . UnsupportedArchive $ scanFile file
    Just units -> pure units
  where
    pathPrefix :: Text
    pathPrefix = getPathPrefix baseDir (parent $ scanFile file)

scanDirectory ::
  ( Has Exec sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Lift IO) sig m
  ) =>
  Maybe ScannableArchive ->
  Text ->
  Path Abs Dir ->
  m (NonEmpty LicenseUnit)
scanDirectory origin pathPrefix path = do
  hasFiles <- hasAnyFiles path
  if hasFiles
    then scanNonEmptyDirectory pathPrefix path
    else maybe (fatal $ EmptyDirectory path) (fatal . EmptyArchive . scanFile) origin

hasAnyFiles ::
  ( Has Diagnostics sig m
  , Has ReadFS sig m
  ) =>
  Path Abs Dir ->
  m Bool
hasAnyFiles path = getAny <$> go path
  where
    go = walk' $ \_ _ files ->
      pure
        if null files
          then (Any False, WalkContinue)
          else (Any True, WalkStop)

-- | Runs a themis license scan on a directory.
-- If the directory has no files (including in subfolders), themis may not return the ideal result.
-- Callers of this function should verify that this directory contains at least one file BEFORE calling.
scanNonEmptyDirectory ::
  ( Has Exec sig m
  , Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  ) =>
  Text ->
  Path Abs Dir ->
  m (NonEmpty LicenseUnit)
scanNonEmptyDirectory pathPrefix cliScanDir = do
  themisScanResult <- runLicenseScanOnDir pathPrefix cliScanDir
  case NE.nonEmpty themisScanResult of
    Nothing -> fatal $ NoLicenseResults cliScanDir
    Just results -> pure results

uploadVendoredDep ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has FossaApiClient sig m
  ) =>
  Path Abs Dir ->
  VendoredDependency ->
  LicenseSourceUnit ->
  m (Maybe Archive)
uploadVendoredDep baseDir VendoredDependency{..} licenseSourceUnit = do
  depVersion <- case vendoredVersion of
    Nothing -> sendIO $ withSystemTempDir "fossa-temp" (calculateVendoredHash baseDir vendoredPath)
    Just version -> pure version

  signedURL <- getSignedLicenseScanUrl $ PackageRevision{packageVersion = depVersion, packageName = vendoredName}

  logSticky $ "Uploading '" <> vendoredName <> "' to secure S3 bucket"
  uploadLicenseScanResult signedURL licenseSourceUnit

  pure $ Just $ Archive vendoredName depVersion

-- | licenseScanSourceUnit receives a list of vendored dependencies, a root path, and API settings.
-- Using this information, it license scans each vendored dependency, uploads the license scan results and then queues a build for the dependency.
licenseScanSourceUnit ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Logger sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has FossaApiClient sig m
  ) =>
  VendoredDependencyScanMode ->
  Path Abs Dir ->
  NonEmpty VendoredDependency ->
  m (NonEmpty Locator)
licenseScanSourceUnit vendoredDependencyScanMode baseDir vendoredDeps = do
  uniqDeps <- dedupVendoredDeps vendoredDeps

  -- The organizationID is needed to prefix each locator name. The FOSSA API automatically prefixes the locator when queuing the build
  -- but not when reading from a source unit.
  orgId <- organizationId <$> getOrganization

  -- make sure all revisions have their versions populated
  uniqDepsWithVersions <- traverse (ensureVendoredDepVersion baseDir) uniqDeps
  -- If skipping is supported, ask Core if any of these deps have already been scanned. If they have, skip scanning them.
  (needScanning, skippable) <-
    if vendoredDependencyScanMode == SkipPreviouslyScanned
      then findDepsThatNeedScanning uniqDepsWithVersions orgId
      else pure (NeedScanningDeps $ NE.toList uniqDepsWithVersions, SkippableDeps [])
  logSkippedDeps needScanning skippable vendoredDependencyScanMode

  -- At this point, we have a good list of deps, so go for it.
  -- If none of the dependencies need scanning we still need to do `finalizeLicenseScan`, so keep going
  maybeScannedArchives <- traverse (scanAndUploadVendoredDep baseDir) (needScanningDeps needScanning)

  -- We need to include both scanned and skipped archives in this list so that they all get included in the build in FOSSA
  let skippedArchives = map forceVendoredToArchive $ skippableDeps skippable
  archives <- fromMaybe NoSuccessfulScans $ NE.nonEmpty $ (catMaybes maybeScannedArchives) <> skippedArchives

  -- finalizeLicenseScan takes archives without Organization information. This orgID is appended when creating the build on the backend.
  -- We don't care about the response here because if the build has already been queued, we get a 401 response.
  finalizeLicenseScan $ ArchiveComponents $ NE.toList archives

  let archivesWithOrganization :: OrgId -> NonEmpty Archive -> NonEmpty Archive
      archivesWithOrganization org = NE.map $ includeOrgId org

      includeOrgId :: OrgId -> Archive -> Archive
      includeOrgId org arc = arc{archiveName = showT org <> "/" <> archiveName arc}

  pure $ NE.map arcToLocator (archivesWithOrganization orgId archives)

ensureVendoredDepVersion ::
  Has (Lift IO) sig m =>
  Path Abs Dir ->
  VendoredDependency ->
  m VendoredDependency
ensureVendoredDepVersion baseDir vdep = do
  depVersion <- case vendoredVersion vdep of
    Nothing -> sendIO . withSystemTempDir "fossa-temp" . calculateVendoredHash baseDir $ vendoredPath vdep
    Just version -> pure version
  pure vdep{vendoredVersion = Just depVersion}

-- Debug logs giving info about which vendored deps were actually scanned
logSkippedDeps ::
  Has Logger sig m =>
  NeedScanningDeps ->
  SkippableDeps ->
  VendoredDependencyScanMode ->
  m ()
logSkippedDeps needScanningDeps skippedDeps scanMode =
  case (needScanningDeps, scanMode) of
    (_, SkippingNotSupported) -> do
      logDebug "This version of the FOSSA service does not support enumerating previously scanned vendored dependencies."
      logDebug "Performing a full scan of all vendored dependencies even if they have been scanned previously."
    (NeedScanningDeps [], SkipPreviouslyScanned) -> do
      logDebug "All of the current vendored dependencies have been previously scanned, reusing previous results."
    (_, SkipPreviouslyScanned) -> do
      case skippedDeps of
        SkippableDeps [] -> logDebug "None of the current vendored dependencies have been previously scanned. License scanning all vendored dependencies"
        _ -> do
          logDebug "Some of the current vendored dependencies have already been scanned by FOSSA."
          logDebug . pretty $ "Reusing previous results for the following vendored dependencies: " <> show skippedDeps

-- | Split the supplied vendored dependencies into those that need scanning and those that do not.
--   We can skip scanning a vendored dependency if an analyzed revision for that vendored dependency already exists on Core.

-- For vendored dependencies with versions supplied by the user in fossa-deps.yml, this means that
-- we will only rescan when the version supplied by the user changes,
-- regardless of whether or not the code in the vendored dependency has changed.
-- This is intentional. Even if we did the scan, an already analyzed revision with this locator already exists on Core
-- and the build to analyze it would not get queued.
findDepsThatNeedScanning ::
  Has FossaApiClient sig m =>
  NonEmpty VendoredDependency ->
  OrgId ->
  m (NeedScanningDeps, SkippableDeps)
findDepsThatNeedScanning vdeps orgId = do
  analyzedLocators <- getAnalyzedRevisions vdeps
  let (need, already) = NE.partition (shouldScanRevision analyzedLocators orgId) vdeps
  pure (NeedScanningDeps need, SkippableDeps already)

shouldScanRevision :: [Text] -> OrgId -> VendoredDependency -> Bool
shouldScanRevision analyzedLocators orgId VendoredDependency{..} = locatorUrl orgId `notElem` analyzedLocators
  where
    locator :: Locator
    locator = Locator "archive" vendoredName vendoredVersion

    locatorUrl :: OrgId -> Text
    locatorUrl org = renderLocatorUrl org locator
