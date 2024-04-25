module App.Fossa.PathDependency (
  enrichPathDependencies,
  enrichPathDependencies',
  withPathDependencyNudge,

  -- * for testing only,
  hashOf,
  absPathOf,
) where

import App.Docs (pathDependencyDocsUrl)
import App.Fossa.Analyze.Project (ProjectResult (projectResultGraph, projectResultPath, projectResultType))
import App.Fossa.Config.Analyze (IncludeAll (..), VendoredDependencyOptions, forceRescans)
import App.Fossa.LicenseScanner (scanDirectory)
import App.Fossa.VendoredDependency (hashBs, hashFile)
import App.Types (FullFileUploads (..), ProjectRevision)
import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry (GenEntry (entryOwnership, entryTime))
import Codec.Archive.Tar.Entry qualified as Tar
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
  getAnalyzedPathRevisions,
  getOrganization,
  uploadLicenseScanResult,
  uploadPathDependencyScan,
 )
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Path ()
import Control.Effect.StickyLogger (StickyLogger)
import Control.Monad (unless, when)
import Data.Flag (Flag, fromFlag)
import Data.List (find, partition)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.String.Conversion (toText)
import Data.Text (Text)
import DepTypes (DepType (..), Dependency (..), VerConstraint (CEq))
import Effect.Exec (Exec)
import Effect.Logger (Logger, logInfo, pretty, redText)
import Effect.ReadFS (
  Has,
  ReadFS,
  doesDirExist,
  doesFileExist,
  resolveDir',
  resolveFile',
 )
import Fossa.API.Types (AnalyzedPathDependency (adpId, adpVersion, apdPath), Organization (..), PathDependencyUpload (..), UploadedPathDependencyLocator (..))
import Graphing (Graphing, gmap, vertexList)
import Path (Abs, Dir, Path, dirname, parent, toFilePath)
import Path.Extra (SomeResolvedPath (ResolvedDir, ResolvedFile))
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
  VendoredDependencyOptions ->
  ProjectRevision ->
  ProjectResult ->
  m ProjectResult
enrichPathDependencies includeAll options projectRevision pr = do
  org <- getOrganization
  let baseDir = projectResultPath pr
  let graph = projectResultGraph pr
  let includeAll' = (fromFlag IncludeAll includeAll)
  if orgSupportsPathDependencyScans org
    then do
      graph' <- resolvePaths includeAll' options projectRevision org baseDir graph
      pure $ pr{projectResultGraph = graph'}
    else pure pr

-- | Updates 'ProjectResult' dependency graph, by updating
-- unresolved path dependency, without license scan, or endpoint upload.
enrichPathDependencies' :: ProjectResult -> ProjectResult
enrichPathDependencies' pr = do
  let graph = projectResultGraph pr
  let graph' = gmap (\d -> if dependencyType d == UnresolvedPathType then d{dependencyType = PathType} else d) graph
  pr{projectResultGraph = graph'}

withPathDependencyNudge :: Has Logger sig m => Flag IncludeAll -> ProjectResult -> m ProjectResult
withPathDependencyNudge includeAll pr = do
  let includeAll' = (fromFlag IncludeAll includeAll)
  let maybePathDeps = NE.nonEmpty $ filter (isValidPathDep includeAll') $ vertexList $ projectResultGraph pr

  -- Only nudge users, if they have any path dependencies!
  when (isJust maybePathDeps) $ do
    logInfo $
      redText "NOTE: "
        <> "path dependency detected in project: "
        <> pretty projectLabel
        <> "! To enable path dependency analysis, use: --experimental-analyze-path-dependencies flag."
        <> pretty (" See " <> pathDependencyDocsUrl <> " for more information.")
  pure pr
  where
    projectLabel :: Text
    projectLabel = toText (projectResultType pr) <> " at: " <> toText (projectResultPath pr)

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
  VendoredDependencyOptions ->
  ProjectRevision ->
  Organization ->
  Path Abs Dir ->
  Graphing Dependency ->
  m (Graphing Dependency)
resolvePaths leaveUnfiltered options projectRevision org baseDir graph = do
  let fullFile = FullFileUploads $ orgRequiresFullFileUploads org
  let maybePathDeps = NE.nonEmpty $ filter (isValidPathDep leaveUnfiltered) $ vertexList graph
  case maybePathDeps of
    -- If there are no resolvable path dependencies
    -- we need to do nothing!
    Nothing -> pure graph
    Just pathDeps -> do
      -- 1. Get only unique dependencies, since we may have
      -- duplicate dependencies in the graph!
      let uniqDeps = NE.nub pathDeps

      -- 2. Get hash from the dependencies
      depsWithMetadata <- catMaybes . NE.toList <$> traverse (metadataOf baseDir) uniqDeps

      -- 3. Identify dependencies that have been already resolved!
      alreadyAnalyzed <-
        if forceRescans options
          then pure []
          else getAnalyzedPathRevisions projectRevision
      let (alreadyAnalyzedMeta, notAnalyzedMeta) = partition (isAnalyzed alreadyAnalyzed) depsWithMetadata
      let alreadyScannedDeps = map (transformResolved alreadyAnalyzed) alreadyAnalyzedMeta

      -- 4. We scan and upload dependencies, and retrieve transformed dependencies!
      scannedDeps <- traverse (scanAndUpload Nothing fullFile projectRevision) notAnalyzedMeta

      -- 5. Kickoff job to finalize the build process for path dependencies
      let resolvedLoc = resolvedLocators scannedDeps
      unless (null resolvedLoc) $
        finalizeLicenseScanForPathDependency resolvedLoc False

      -- 6. Finally update the graph with resolved path dependencies!
      pure $ replaceResolved (scannedDeps ++ alreadyScannedDeps)
  where
    replaceResolved :: [(Dependency, Maybe Dependency)] -> Graphing Dependency
    replaceResolved deps = gmap (replace deps) graph

    replace :: [(Dependency, Maybe Dependency)] -> Dependency -> Dependency
    replace deps dep = case find (\d -> fst d == dep) deps of
      Nothing -> dep
      Just (_, maybeDep) -> fromMaybe dep maybeDep

    resolvedLocators :: [(Dependency, Maybe Dependency)] -> [Locator]
    resolvedLocators deps = map toLocator (mapMaybe snd deps)

    isAnalyzed :: [AnalyzedPathDependency] -> (Dependency, SomeResolvedPath, Text) -> Bool
    isAnalyzed alreadyAnalyzed (d, _, version) = isJust $ find (\aa -> apdPath aa == dependencyName d && adpVersion aa == version) alreadyAnalyzed

transformResolved :: [AnalyzedPathDependency] -> (Dependency, SomeResolvedPath, Text) -> (Dependency, Maybe Dependency)
transformResolved alreadyAnalyzed (rawDep, _, version) = case maybeAlreadyAnalyzed of
  Nothing -> (rawDep, Nothing)
  Just ap -> (rawDep, Just rawDep{dependencyType = PathType, dependencyName = adpId ap, dependencyVersion = Just . CEq $ adpVersion ap})
  where
    maybeAlreadyAnalyzed :: Maybe AnalyzedPathDependency
    maybeAlreadyAnalyzed = find (\aa -> apdPath aa == dependencyName rawDep && adpVersion aa == version) alreadyAnalyzed

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

canResolvePathDep :: Dependency -> Bool
canResolvePathDep d = dependencyType d == UnresolvedPathType

isValidPathDep :: Bool -> Dependency -> Bool
isValidPathDep leaveOtherEnvDeps d =
  if leaveOtherEnvDeps
    then canResolvePathDep d
    else (canResolvePathDep d && shouldPublishDep d)

hashOf :: Has (Lift IO) sig m => SomeResolvedPath -> m Text
hashOf (ResolvedDir dir) = sendIO $ hashDir dir
hashOf (ResolvedFile file) = sendIO $ hashFile (toFilePath file)

hashDir :: Has (Lift IO) sig m => Path Abs Dir -> m Text
hashDir targetDir = do
  let baseDir = parent targetDir
  let dirToPack = dirname targetDir

  -- >> equivalent to: tar -C baseDir -c dirToPack
  es <- sendIO $ Tar.pack (toFilePath baseDir) [toFilePath dirToPack]

  -- We want to ignore permissions, and time
  -- in our packing, since this may lead to different
  -- hash, even if the content remained the same.
  --
  -- For example, consider, user storing vendor
  -- dependency in 'git'. Git by default, does not
  -- track modification time, file permission, etc.
  -- This means that in CI, each 'git clone' can
  -- lead to FOSSA inferring new version from hash,
  -- even though content is the same!
  let es' = map (setUnknownOwner . setZeroTime) es
  sendIO $ hashBs . Tar.write $ es'
  where
    setUnknownOwner :: Tar.Entry -> Tar.Entry
    setUnknownOwner e = e{entryOwnership = Tar.Ownership "" "" 0 0}

    setZeroTime :: Tar.Entry -> Tar.Entry
    setZeroTime e = e{entryTime = 0}

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
