{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.LicenseScanner (
  licenseScanSourceUnit,
  combineLicenseUnits,
  scanVendoredDep,
  calculateVendoredHash,
  scanDirectory,
) where

import App.Fossa.EmbeddedBinary (ThemisBins, withThemisAndIndex)
import App.Fossa.RunThemis (
  execThemis,
  themisFlags,
 )
import App.Fossa.VendoredDependency (
  NeedScanningDeps (..),
  SkippableDeps (..),
  VendoredDependency (..),
  VendoredDependencyScanMode (..),
  arcToLocator,
  compressFile,
  dedupVendoredDeps,
  forceVendoredToArchive,
  hashFile,
  skippedDepsDebugLog,
  vendoredDependencyScanModeToDependencyRebuild,
 )
import App.Types (FileUpload)
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
import Data.Either.Combinators (rightToMaybe)
import Data.Error (SourceLocation, createEmptyBlock, getSourceLocation)
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
  resolvePath',
 )
import Errata (Errata, errataSimple)
import Fossa.API.Types (
  Archive (Archive, archiveName),
  ArchiveComponents (..),
  OrgId,
  Organization (organizationId),
 )
import Path (Abs, Dir, File, Path, SomeBase (Abs, Rel), fileExtension, parent, (</>))
import Path.Extra (SomePath (..), tryMakeRelative)
import Prettyprinter (Pretty (pretty))
import Srclib.Types (
  LicenseScanType (CliLicenseScanned),
  LicenseSourceUnit (..),
  LicenseUnit (..),
  Locator (..),
 )
import Types (LicenseScanPathFilters (licenseScanPathFilterFileExclude))

data LicenseScanErr
  = NoSuccessfulScans SourceLocation
  | NoLicenseResults SourceLocation (Path Abs Dir)
  | EmptyDirectory SourceLocation (Path Abs Dir)
  | EmptyOrCorruptedArchive SourceLocation (Path Abs File)
  | UnsupportedArchive SourceLocation (Path Abs File)

instance ToDiagnostic LicenseScanErr where
  renderDiagnostic :: LicenseScanErr -> Errata
  renderDiagnostic (NoSuccessfulScans srcLoc) =
    errataSimple (Just "No native license scans were successful") (createEmptyBlock srcLoc) Nothing
  renderDiagnostic (NoLicenseResults srcLoc path) =
    errataSimple (Just $ "No license results found after scanning directory: " <> toText path) (createEmptyBlock srcLoc) Nothing
  renderDiagnostic (EmptyDirectory srcLoc path) =
    errataSimple (Just $ "vendored-dependencies path has no files and cannot be scanned: " <> toText path) (createEmptyBlock srcLoc) Nothing
  renderDiagnostic (EmptyOrCorruptedArchive srcLoc path) =
    errataSimple (Just $ "vendored-dependencies archive is malformed or contains no files: " <> toText path) (createEmptyBlock srcLoc) Nothing
  renderDiagnostic (UnsupportedArchive srcLoc path) = case fileExtension path of
    Just ext -> errataSimple (Just $ "FOSSA CLI does not support archives of type " <> "`" <> toText ext <> "`" <> ": " <> toText path) (createEmptyBlock srcLoc) Nothing
    Nothing -> errataSimple (Just $ "FOSSA CLI does not support archives without file extensions: " <> toText path) (createEmptyBlock srcLoc) Nothing

newtype ScannableArchive = ScannableArchive {scanFile :: Path Abs File} deriving (Eq, Ord, Show)

runLicenseScanOnDir ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Exec sig m
  , Has ReadFS sig m
  ) =>
  Text ->
  Maybe LicenseScanPathFilters ->
  FileUpload ->
  Path Abs Dir ->
  m [LicenseUnit]
runLicenseScanOnDir pathPrefix licenseScanPathFilters uploadKind scanDir = do
  -- license scan the root directory
  rootDirUnits <- withThemisAndIndex $ themisRunner pathPrefix licenseScanPathFilters uploadKind scanDir
  -- recursively unpack archives and license scan them too
  otherArchiveUnits <- runFinally $ recursivelyScanArchives pathPrefix licenseScanPathFilters uploadKind scanDir
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
  Maybe LicenseScanPathFilters ->
  FileUpload ->
  Path Abs Dir ->
  m [LicenseUnit]
recursivelyScanArchives pathPrefix licenseScanPathFilters uploadKind dir = flip walk' dir $
  \_ _ files -> do
    let process file unpackedDir = do
          let updatedPathPrefix = pathPrefix <> getPathPrefix dir (parent file)
          currentDirResults <- withThemisAndIndex $ themisRunner updatedPathPrefix licenseScanPathFilters uploadKind unpackedDir
          recursiveResults <- recursivelyScanArchives updatedPathPrefix licenseScanPathFilters uploadKind unpackedDir
          pure $ currentDirResults <> recursiveResults
    -- filter out files that match licenseScanPathFilterFileExclude. Currently, these are only created by firstPartyScanMain
    -- but it would be easy to allow customers to filter out single files too.
    let archivesToSkip = maybe [] licenseScanPathFilterFileExclude licenseScanPathFilters
    let filesToProcess = filter (`notElem` archivesToSkip) files

    archives <- catMaybes <$> traverse (\file -> rightToMaybe <$> withArchive' file (process file)) filesToProcess
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
  Maybe LicenseScanPathFilters ->
  FileUpload ->
  Path Abs Dir ->
  ThemisBins ->
  m [LicenseUnit]
themisRunner pathPrefix licenseScanPathFilters uploadKind scanDir themisBins = runThemis themisBins pathPrefix licenseScanPathFilters uploadKind scanDir

runThemis :: (Has Exec sig m, Has Diagnostics sig m) => ThemisBins -> Text -> Maybe LicenseScanPathFilters -> FileUpload -> Path Abs Dir -> m [LicenseUnit]
runThemis themisBins pathPrefix licenseScanPathFilters uploadKind scanDir = do
  context "Running license scan binary" $ execThemis themisBins pathPrefix scanDir $ themisFlags licenseScanPathFilters uploadKind

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
  Maybe LicenseScanPathFilters ->
  FileUpload ->
  VendoredDependency ->
  m (Maybe Archive)
scanAndUploadVendoredDep baseDir licenseScanPathFilters uploadKind vdep = context "Processing vendored dependency" $ do
  maybeLicenseUnits <- recover $ scanVendoredDep baseDir licenseScanPathFilters uploadKind vdep
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
  Maybe LicenseScanPathFilters ->
  FileUpload ->
  VendoredDependency ->
  m LicenseSourceUnit
scanVendoredDep baseDir licenseScanPathFilters uploadKind VendoredDependency{..} = context "Scanning vendored deps for license data" $ do
  logSticky $ "License Scanning '" <> vendoredName <> "' at '" <> vendoredPath <> "'"
  scanPath <- resolvePath' baseDir $ toString vendoredPath
  licenseUnits <- case scanPath of
    SomeFile (Abs path) -> scanArchive baseDir licenseScanPathFilters uploadKind $ ScannableArchive path
    SomeFile (Rel path) -> scanArchive baseDir licenseScanPathFilters uploadKind . ScannableArchive $ baseDir </> path
    SomeDir (Abs path) -> scanDirectory Nothing (getPathPrefix baseDir path) licenseScanPathFilters uploadKind path
    SomeDir (Rel path) -> scanDirectory Nothing (toText path) licenseScanPathFilters uploadKind (baseDir </> path)
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
  Maybe LicenseScanPathFilters ->
  FileUpload ->
  ScannableArchive ->
  m (NonEmpty LicenseUnit)
scanArchive baseDir licenseScanPathFilters uploadKind file = runFinally $ do
  logSticky $ "scanning archive at " <> toText (scanFile file)
  result <- withArchive' (scanFile file) (scanDirectory (Just file) pathPrefix licenseScanPathFilters uploadKind)
  case result of
    Left _ -> fatal . UnsupportedArchive getSourceLocation $ scanFile file
    Right r -> case r of
      Nothing -> fatal . EmptyOrCorruptedArchive getSourceLocation $ scanFile file
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
  Maybe LicenseScanPathFilters ->
  FileUpload ->
  Path Abs Dir ->
  m (NonEmpty LicenseUnit)
scanDirectory origin pathPrefix licenseScanPathFilters uploadKind path = do
  hasFiles <- hasAnyFiles path
  if hasFiles
    then scanNonEmptyDirectory pathPrefix licenseScanPathFilters uploadKind path
    else maybe (fatal $ EmptyDirectory getSourceLocation path) (fatal . EmptyOrCorruptedArchive getSourceLocation . scanFile) origin

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
  Maybe LicenseScanPathFilters ->
  FileUpload ->
  Path Abs Dir ->
  m (NonEmpty LicenseUnit)
scanNonEmptyDirectory pathPrefix licenseScanPathFilters uploadKind cliScanDir = do
  themisScanResult <- runLicenseScanOnDir pathPrefix licenseScanPathFilters uploadKind cliScanDir
  case NE.nonEmpty themisScanResult of
    Nothing -> fatal $ NoLicenseResults getSourceLocation cliScanDir
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

  logSticky $ "Uploading license results for '" <> vendoredName <> "' to secure S3 bucket"
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
  Maybe LicenseScanPathFilters ->
  FileUpload ->
  Path Abs Dir ->
  NonEmpty VendoredDependency ->
  m (NonEmpty Locator)
licenseScanSourceUnit skipMode licenseScanPathFilters uploadKind baseDir vendoredDeps = do
  uniqDeps <- dedupVendoredDeps vendoredDeps

  -- The organizationID is needed to prefix each locator name. The FOSSA API automatically prefixes the locator when queuing the build
  -- but not when reading from a source unit.
  orgId <- organizationId <$> getOrganization

  -- make sure all revisions have their versions populated
  uniqDepsWithVersions <- traverse (ensureVendoredDepVersion baseDir) uniqDeps
  -- If skipping is supported, ask Core if any of these deps have already been scanned. If they have, skip scanning them.
  (needScanning, skippable) <-
    if skipMode == SkipPreviouslyScanned
      then findDepsThatNeedScanning uniqDepsWithVersions orgId
      else pure (NeedScanningDeps $ NE.toList uniqDepsWithVersions, SkippableDeps [])

  logDebug . pretty $ skippedDepsDebugLog needScanning skippable skipMode

  -- At this point, we have a good list of deps, so go for it.
  -- If none of the dependencies need scanning we still need to do `finalizeLicenseScan`, so keep going
  maybeScannedArchives <- traverse (scanAndUploadVendoredDep baseDir licenseScanPathFilters uploadKind) (needScanningDeps needScanning)

  -- We need to include both scanned and skipped archives in this list so that they all get included in the build in FOSSA
  let skippedArchives = map forceVendoredToArchive $ skippableDeps skippable
  archives <- fromMaybe (NoSuccessfulScans getSourceLocation) $ NE.nonEmpty $ (catMaybes maybeScannedArchives) <> skippedArchives

  -- finalizeLicenseScan takes archives without Organization information. This orgID is appended when creating the build on the backend.
  -- We don't care about the response here because if the build has already been queued, we get a 401 response.
  finalizeLicenseScan $ ArchiveComponents (NE.toList archives) (vendoredDependencyScanModeToDependencyRebuild skipMode) uploadKind

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
