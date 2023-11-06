module App.Fossa.PathDependency (
  enrichPathDependencies,

  -- * for testing only,
  hashOf,
  absPathOf,
) where

import App.Fossa.Analyze.Project (ProjectResult (projectResultGraph, projectResultPath))
import App.Fossa.Config.Analyze (IncludeAll (..))
import App.Fossa.LicenseScanner (scanDirectory)
import App.Fossa.VendoredDependency (hashFile)
import App.Types (FullFileUploads (..), ProjectRevision)
import Control.Carrier.StickyLogger (logSticky)
import Control.Effect.Diagnostics (
  Diagnostics,
  context,
  fatalText,
  fromEither,
  recover,
 )
import Control.Effect.FossaApiClient (
  FossaApiClient,
  PackageRevision (..),
  finalizeLicenseScanForPathDependency,
  getOrganization,
  uploadLicenseScanResult,
  uploadPathDependencyScan,
 )
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Path ()
import Control.Effect.StickyLogger (StickyLogger)
import Control.Monad (unless)
import Data.Flag (Flag, fromFlag)
import Data.List (find)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.String.Conversion (toText)
import Data.Text (Text)
import DepTypes (DepType (..), Dependency (..), VerConstraint (CEq))
import Discovery.Archive (mkZip)
import Effect.Exec (Exec)
import Effect.ReadFS (
  Has,
  ReadFS,
  doesDirExist,
  doesFileExist,
  resolveDir',
  resolveFile',
 )
import Fossa.API.Types (Organization (..), PathDependencyUpload (..), UploadedPathDependencyLocator (..))
import Graphing (Graphing, gmap, vertexList)
import Path (Abs, Dir, Path, toFilePath)
import Path.Extra (SomeResolvedPath (ResolvedDir, ResolvedFile))
import Path.IO (withSystemTempDir)
import Path.IO qualified as PathIO
import Srclib.Converter (shouldPublishDep, toLocator)
import Srclib.Types (LicenseScanType (..), LicenseSourceUnit (..), Locator)
import Types (LicenseScanPathFilters)

-- | Updates 'ProjectResult' dependency graph, by updating
-- unresolved path dependency, as well as performing license
-- scan and license scan result upload
enrichPathDependencies ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has FossaApiClient sig m
  ) =>
  Flag IncludeAll ->
  ProjectRevision ->
  ProjectResult ->
  m ProjectResult
enrichPathDependencies includeAll projectRevision pr = do
  org <- getOrganization
  let baseDir = projectResultPath pr
  let graph = projectResultGraph pr
  let includeAll' = (fromFlag IncludeAll includeAll)
  if orgSupportsPathDependencyScans org
    then do
      graph' <- resolvePaths includeAll' projectRevision org baseDir graph
      pure $ pr{projectResultGraph = graph'}
    else pure pr

-- | Scan and Uploads path dependency from a graph
resolvePaths ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has FossaApiClient sig m
  ) =>
  Bool ->
  ProjectRevision ->
  Organization ->
  Path Abs Dir ->
  Graphing Dependency ->
  m (Graphing Dependency)
resolvePaths leaveUnfiltered projectRevision org baseDir graph = do
  let fullFile = FullFileUploads $ orgRequiresFullFileUploads org
  let maybePathDeps = NE.nonEmpty $ filter (isValidDep leaveUnfiltered) $ vertexList graph
  case maybePathDeps of
    -- If there are no resolvable path dependencies
    -- we need to do nothing!
    Nothing -> pure graph
    Just pathDeps -> do
      -- 1. Get only unique dependencies, since we may have
      -- duplicate dependencies in the graph!
      let uniqDeps = NE.nub pathDeps

      -- 2. Get hash from the dependencies, and check
      -- dependencies that can be skipped!
      depsWithMetadata <- catMaybes . NE.toList <$> traverse (metadataOf baseDir) uniqDeps
      let needToScanDepsWithMetadata = depsWithMetadata

      -- 3. We scan and upload dependencies, and retrieve transformed
      -- dependencies!
      scannedDeps <- traverse (scanAndUpload Nothing fullFile projectRevision) needToScanDepsWithMetadata

      -- 4. Kickoff job to finalize the build process for path dependencies
      let resolvedLoc = resolvedLocators scannedDeps
      unless (null resolvedLoc) $
        finalizeLicenseScanForPathDependency resolvedLoc False

      -- 5. Finally update the graph with resolved path dependencies!
      pure $ replaceResolved scannedDeps
  where
    replaceResolved :: [(Dependency, Maybe Dependency)] -> Graphing Dependency
    replaceResolved deps = gmap (replace deps) graph

    replace :: [(Dependency, Maybe Dependency)] -> Dependency -> Dependency
    replace deps dep = case find (\d -> fst d == dep) deps of
      Nothing -> dep
      Just (_, maybeDep) -> fromMaybe dep maybeDep

    resolvedLocators :: [(Dependency, Maybe Dependency)] -> [Locator]
    resolvedLocators deps = map toLocator (mapMaybe snd deps)

-- | Performs license scanning and upload for single path dependency.
scanAndUpload ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has StickyLogger sig m
  , Has FossaApiClient sig m
  ) =>
  Maybe LicenseScanPathFilters ->
  FullFileUploads ->
  ProjectRevision ->
  (Dependency, SomeResolvedPath, Text) ->
  m (Dependency, Maybe Dependency)
scanAndUpload pathFilters fullFileUpload projectRevision (rawDep, resolvedPath, version) = context "Path Dependency" $ do
  maybeLicenseUnits <- recover scan
  case maybeLicenseUnits of
    Nothing -> pure (rawDep, Nothing)
    Just licenseUnits -> (rawDep,) <$> upload licenseUnits
  where
    rawPath :: Text
    rawPath = dependencyName rawDep

    scan = do
      logSticky $ "License Scanning '" <> rawPath <> "' at '" <> toText resolvedPath <> "'"
      licenseUnits <- case resolvedPath of
        ResolvedDir dir -> scanDirectory Nothing mempty pathFilters fullFileUpload dir
        ResolvedFile _ -> fatalText $ "path dependency for single file is not supported: " <> rawPath
      pure $ LicenseSourceUnit rawPath CliLicenseScanned licenseUnits

    upload licSrcUnit = do
      logSticky $ "Uploading '" <> rawPath <> "' to secure S3 bucket"
      resp <- uploadPathDependencyScan (PackageRevision rawPath version) projectRevision fullFileUpload

      let signedURL = pdSignedURL resp
      let name' = updlName . pdLocator $ resp
      let version' = updlVersion . pdLocator $ resp
      uploadLicenseScanResult signedURL licSrcUnit

      pure $
        Just
          rawDep
            { dependencyType = PathType
            , dependencyName = name'
            , dependencyVersion = Just $ CEq version'
            }

-- * Helpers

canResolve :: Dependency -> Bool
canResolve d = dependencyType d == UnresolvedPathType

isValidDep :: Bool -> Dependency -> Bool
isValidDep leaveOtherEnvDeps d =
  if leaveOtherEnvDeps
    then canResolve d
    else (canResolve d && shouldPublishDep d)

hashOf :: Has (Lift IO) sig m => SomeResolvedPath -> m Text
hashOf (ResolvedDir dir) = sendIO $ withSystemTempDir "fossa-path-dep" (calculateHash dir)
hashOf (ResolvedFile file) = sendIO $ hashFile (toFilePath file)

calculateHash :: Has (Lift IO) sig m => Path Abs Dir -> Path Abs Dir -> m Text
calculateHash targetDir tempFileDir = do
  filePath <- sendIO $ PathIO.resolveFile tempFileDir "fosse-path-dep.zip"
  mkZip targetDir filePath
  sendIO $ hashFile (toFilePath filePath)

absPathOf :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> Text -> m (SomeResolvedPath)
absPathOf baseDir relativeOrAbsPath = do
  dir' <- fromEither =<< resolveDir' baseDir relativeOrAbsPath
  file' <- fromEither =<< resolveFile' baseDir relativeOrAbsPath

  dirExists <- doesDirExist dir'
  fileExists <- doesFileExist file'

  case (dirExists, fileExists) of
    (True, _) -> pure $ ResolvedDir dir'
    (_, True) -> pure $ ResolvedFile file'
    _ -> fatalText $ "Provided path: " <> relativeOrAbsPath <> " could not be resolved!"

metadataOf ::
  ( Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  Dependency ->
  m (Maybe (Dependency, SomeResolvedPath, Text))
metadataOf baseDir dep = case dependencyType dep of
  UnresolvedPathType -> do
    let depPath = dependencyName dep
    depPath' <- absPathOf baseDir depPath
    hash <- hashOf depPath'
    pure $ Just (dep, depPath', hash)
  _ -> pure Nothing
