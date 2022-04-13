{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.LicenseScanner (
  licenseScanSourceUnit,
) where

import App.Fossa.ArchiveUploader (
  VendoredDependency (..),
  arcToLocator,
  compressFile,
  duplicateFailureBundle,
  duplicateNames,
  hashFile,
 )
import App.Fossa.EmbeddedBinary (ThemisBins, withThemisAndIndex)
import App.Fossa.FossaAPIV1 qualified as Fossa
import App.Fossa.RunThemis (
  execThemis,
 )
import Control.Carrier.Finally (Finally, runFinally)
import Control.Carrier.TaskPool (TaskPool, forkTask)
import Control.Effect.Diagnostics (Diagnostics, ToDiagnostic (renderDiagnostic), context, fatal, fatalText, fromMaybe, recover)
import Control.Effect.FossaApiClient (FossaApiClient, getApiOpts)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Path (withSystemTempDir)
import Control.Effect.StickyLogger (StickyLogger, logSticky)
import Control.Monad (unless, void)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)
import Data.Semigroup (Any (..))
import Data.String.Conversion (toString, toText)
import Data.Text (Text)
import Data.Text.Extra (showT)
import Discovery.Archive (withArchive')
import Discovery.Archive qualified as Archive
import Discovery.Walk (WalkStep (WalkContinue, WalkStop), walk, walk')
import Effect.Exec (Exec)
import Effect.ReadFS (
  Has,
  ReadFS,
  SomePath (SomeDir, SomeFile),
  resolvePath',
 )
import Fossa.API.Types (
  ApiOpts,
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
  -- run the scan on the root directory
  rootDirUnits <- withThemisAndIndex $ themisRunner pathPrefix scanDir
  otherArchiveUnits <- runFinally $ discover (scanDirectory Nothing pathPrefix) scanDir
  pure $ rootDirUnits <> NE.toList otherArchiveUnits

-- pure rootDirUnits

-- runLicenseScanOnDir pathPrefix scanDir = withThemisAndIndex $ themisRunner pathPrefix scanDir

-- | Given a function to run over unarchived contents, recursively unpack archives
-- discover :: (Has (Lift IO) sig m, Has ReadFS sig m, Has Diagnostics sig m, Has Finally sig m, Has TaskPool sig m) => (Path Abs Dir -> (Maybe (NonEmpty LicenseUnit))) -> Path Abs Dir -> m (Maybe [LicenseUnit])
-- discover go dir = context "Finding archives" $
--   flip walk' dir $ \_ _ files -> do
--     -- To process an unpacked archive, run the provided function on the archive
--     -- contents, and recursively call discover
--     let process file unpackedDir = context (toText (fileName file)) $ do
--           a <- go unpackedDir
--           discover go unpackedDir

--     archives <- traverse (\file -> withArchive' file (process file)) files
--     pure (archives, WalkContinue)

-- compiles:
-- discover :: (Has (Lift IO) sig m, Has ReadFS sig m, Has Diagnostics sig m) => (Path Abs Dir -> m [Path Abs File]) -> Path Abs Dir -> m [Path Abs File]
-- discover go dir = context "Finding archives" $
--   flip walk' dir $ \_ _ files -> do
--     -- let process :: Path Abs Dir -> m [Path Abs File]
--     let process unpackedDir = do
--           a <- go unpackedDir
--           b <- discover go unpackedDir
--           pure (a ++ b)
--     archives <- traverse (`withArchive'` process) files
--     pure (concat (catMaybes archives), WalkContinue)

discover :: (Has (Lift IO) sig m, Has ReadFS sig m, Has Diagnostics sig m, Has Finally sig m) => (Path Abs Dir -> m (NonEmpty LicenseUnit)) -> Path Abs Dir -> m (NonEmpty LicenseUnit)
discover go dir = context "Finding archives" $
  flip walk' dir $ \_ _ files -> do
    -- let process :: (Monad m) => Path Abs Dir -> m (NonEmpty LicenseUnit)
    let process unpackedDir = do
          a <- go unpackedDir
          b <- discover go unpackedDir
          pure $ a <> b
    -- pure $ NE.fromList $ (NE.toList a) <> (NE.toList b)
    archives <- traverse (`withArchive'` process) files
    let as = catMaybes archives
        fas = NE.fromList $ concatMap NE.toList as
    pure (fas, WalkContinue)

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
  , Has Finally sig m
  ) =>
  ApiOpts ->
  Path Abs Dir ->
  VendoredDependency ->
  m (Maybe Archive)
scanAndUploadVendoredDep apiOpts baseDir vdep = context "Processing vendored dependency" $ do
  maybeLicenseUnits <- recover $ scanVendoredDep baseDir vdep
  case maybeLicenseUnits of
    Nothing -> pure Nothing
    Just licenseUnits -> uploadVendoredDep baseDir apiOpts vdep licenseUnits

scanVendoredDep ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has Finally sig m
  ) =>
  Path Abs Dir ->
  VendoredDependency ->
  m (NonEmpty LicenseUnit)
scanVendoredDep baseDir VendoredDependency{..} = context "Scanning vendored deps for license data" $ do
  logSticky $ "License Scanning '" <> vendoredName <> "' at '" <> vendoredPath <> "'"
  scanPath <- resolvePath' baseDir $ toString vendoredPath
  case scanPath of
    SomeFile (Abs path) -> scanArchive baseDir $ ScannableArchive path
    SomeFile (Rel path) -> scanArchive baseDir . ScannableArchive $ baseDir </> path
    SomeDir (Abs path) -> scanDirectory Nothing (getPathPrefix baseDir path) path
    SomeDir (Rel path) -> scanDirectory Nothing (toText path) (baseDir </> path)

getPathPrefix :: Path Abs Dir -> Path Abs t -> Text
getPathPrefix baseDir scanPath = do
  case tryMakeRelative baseDir scanPath of
    Path.Abs _ -> ""
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
    pathPrefix = getPathPrefix baseDir (parent $ scanFile file)

scanDirectory ::
  ( Has Exec sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Finally sig m
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
  , Has Finally sig m
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
  ) =>
  Path Abs Dir ->
  ApiOpts ->
  VendoredDependency ->
  NE.NonEmpty LicenseUnit ->
  m (Maybe Archive)
uploadVendoredDep baseDir apiOpts VendoredDependency{..} themisScanResult = do
  let licenseSourceUnit =
        LicenseSourceUnit
          { licenseSourceUnitName = vendoredPath
          , licenseSourceUnitType = CliLicenseScanned
          , licenseSourceUnitLicenseUnits = themisScanResult
          }

  depVersion <- case vendoredVersion of
    Nothing -> sendIO $ withSystemTempDir "fossa-temp" (calculateVendoredHash baseDir vendoredPath)
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
  , Has ReadFS sig m
  , Has FossaApiClient sig m
  , Has Finally sig m
  ) =>
  Path Abs Dir ->
  NonEmpty VendoredDependency ->
  m (NonEmpty Locator)
licenseScanSourceUnit baseDir vendoredDeps = do
  apiOpts <- getApiOpts
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
    archivesWithOrganization :: OrgId -> NonEmpty Archive -> NonEmpty Archive
    archivesWithOrganization orgId = NE.map $ includeOrgId orgId

    includeOrgId :: OrgId -> Archive -> Archive
    includeOrgId orgId arc = arc{archiveName = showT orgId <> "/" <> archiveName arc}
