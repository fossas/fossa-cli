
module App.Fossa.PathDependency (
  enrichPathDependencies,

  -- * for testing only,
  hashOf,
  normalizePathOf,
  SomeAbsPath(..), -- TODO: Move to Path.Extra

  -- licenseScanSourceUnit,
  -- combineLicenseUnits,
  -- scanVendoredDep,
  -- scanAndUploadPathDependency,
) where

import Path (Dir, Path, File, toFilePath, Abs)
import App.Fossa.LicenseScanner (scanDirectory)
import Data.Text (Text)
import DepTypes (Dependency(..), VerConstraint (CEq), DepType (..))
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Path ()
import Types (LicenseScanPathFilters)
import App.Types (FullFileUploads(..))
import Srclib.Types (LicenseSourceUnit(..), LicenseScanType (..), Locator)
import Effect.ReadFS
    ( Has,
      ReadFS,
      resolveFile',
      resolveDir',
      doesFileExist,
      doesDirExist )
import Effect.Exec (Exec)
import Control.Effect.StickyLogger (StickyLogger)
import Control.Effect.Diagnostics (Diagnostics, recover, fromEither)
import Control.Effect.FossaApiClient
    ( FossaApiClient,
      PackageRevision(..),
      getOrganization,
      uploadPathDependencyScan,
      uploadLicenseScanResult, finalizeLicenseScanForPathDependency )
import Control.Carrier.StickyLogger (logSticky)
import qualified Data.List.NonEmpty as NE
import Fossa.API.Types (Organization(..), PathDependencyUpload (..), UploadedPathDependencyLocator (..))
import Graphing (Graphing, vertexList)
import App.Fossa.Analyze.Project (ProjectResult (projectResultPath, projectResultGraph))
import Srclib.Converter (shouldPublishDep, toLocator)
import Data.Flag (Flag, fromFlag)
import App.Fossa.Config.Analyze (IncludeAll(..))
import Control.Carrier.Diagnostics (fatalText)
import Path.IO (withSystemTempDir)
import Path.IO qualified as PathIO
import Discovery.Archive (mkZip)
import App.Fossa.VendoredDependency (hashFile)
import Data.Maybe (catMaybes, mapMaybe)
import System.FilePath (normalise)
import Control.Monad (unless)

data SomeAbsPath = SDir (Path Abs Dir) | SFile (Path Abs File)

-- | Updates 'ProjectResult' dependency graph, by updating path dependency,
-- as well as performing license scan and license scan result upload
enrichPathDependencies :: (
    Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has FossaApiClient sig m
  ) => Flag IncludeAll -> ProjectResult -> m ProjectResult
enrichPathDependencies includeAll pr = do
  org <- getOrganization
  let baseDir = projectResultPath pr
  let graph = projectResultGraph pr
  if orgSupportsPathDependencyScans org
    then do
      graph' <- resolvePaths (fromFlag IncludeAll includeAll) org baseDir graph
      pure $ pr {projectResultGraph = graph'}
    else
      pure pr

-- | Scan and Uploads path dependency from a graph
resolvePaths :: (
    Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has FossaApiClient sig m
  ) => Bool -> Organization -> Path Abs Dir -> Graphing Dependency -> m (Graphing Dependency)
resolvePaths leaveUnfiltered org baseDir graph = do
  let fullFile = FullFileUploads $ orgRequiresFullFileUploads org
  let maybePathDeps = NE.nonEmpty $ filter (isValidDep leaveUnfiltered) $ vertexList graph
  case maybePathDeps of
      -- If there are no valid path dependencies
      -- we need to do nothing!
      Nothing -> pure graph

      -- If there are valid unresolved path dependencies, we
      -- scan only dependencies that have yet to be uploaded!
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
        scannedDeps <- traverse (scanAndUpload Nothing fullFile) needToScanDepsWithMetadata

        -- 4. Kickoff job to finalize the build process for path dependencies
        let resolvedLoc = resolvedLocators scannedDeps
        unless (null resolvedLoc) $
          finalizeLicenseScanForPathDependency resolvedLoc False

        -- 5. Finally update the graph with dependencyName, with orgId, and
        -- dependencyHash. We ignore 
        pure $ replaceResolved scannedDeps
  where
    replaceResolved :: [(Dependency, Maybe Dependency)] -> Graphing Dependency
    replaceResolved _ = graph

    resolvedLocators :: [(Dependency, Maybe Dependency)] -> [Locator]
    resolvedLocators deps = map toLocator (mapMaybe snd deps)

-- | Performs license scanning and upload for single path dependency.
scanAndUpload :: ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has StickyLogger sig m
  , Has FossaApiClient sig m
  ) =>
  Maybe LicenseScanPathFilters ->
  FullFileUploads ->
  (Dependency, SomeAbsPath, Text) ->
  m (Dependency, Maybe Dependency)
scanAndUpload pathFilters fullFileUpload (rawDep, resolvedPath, version) = do
  maybeLicenseUnits <- recover scan
  case maybeLicenseUnits of
    Nothing -> pure (rawDep, Nothing)
    Just licenseUnits -> (rawDep, ) <$> upload licenseUnits

  where
    rawPath :: Text
    rawPath = dependencyName rawDep

    scan :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m, Has (Lift IO) sig m) => m LicenseSourceUnit
    scan = do
      let vendoredPath = dependencyName rawDep
      licenseUnits <- case resolvedPath of
        SDir dir -> scanDirectory Nothing mempty pathFilters fullFileUpload dir
        SFile _ -> fatalText $ "path dependency for single file is not supported: " <> rawPath

      let srcUnit = LicenseSourceUnit vendoredPath CliLicenseScanned licenseUnits
      pure srcUnit

    upload :: (Has FossaApiClient sig m, Has StickyLogger sig m) => LicenseSourceUnit -> m (Maybe Dependency)
    upload licSrcUnit = do
      resp <- uploadPathDependencyScan $ PackageRevision rawPath version
      let signedURL =  pdSignedURL resp
      let name' = updlName . pdLocator $ resp
      let version' = updlVersion . pdLocator $ resp

      logSticky $ "Uploading license scan findings of: '"
        <> rawPath
        <>  "' to secure S3 bucket"

      uploadLicenseScanResult signedURL licSrcUnit
      pure $ Just rawDep {
        dependencyType = PathType,
        dependencyName = name',
        dependencyVersion = Just $ CEq version'}

-- * Helpers

normalizePathOf :: FilePath -> FilePath
normalizePathOf = normalise

canResolve :: Dependency -> Bool
canResolve d = dependencyType d == UnresolvedPathType

isValidDep :: Bool -> Dependency -> Bool
isValidDep leaveOtherEnvDeps d =
  if leaveOtherEnvDeps then canResolve d
  else (canResolve d && shouldPublishDep d)

hashOf :: Has (Lift IO) sig m => SomeAbsPath -> m Text
hashOf (SDir dir) = sendIO $ withSystemTempDir "fossa-path-dep" (calculateHash dir)
hashOf (SFile file) = sendIO $ hashFile (toFilePath file)

calculateHash :: Has (Lift IO) sig m => Path Abs Dir -> Path Abs Dir -> m Text
calculateHash targetDir tempFileDir = do
  filePath <- sendIO $ PathIO.resolveFile tempFileDir "fosse-path-dep.zip"
  mkZip targetDir filePath
  sendIO $ hashFile (toFilePath filePath)

absPathOf :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> Text -> m (SomeAbsPath)
absPathOf baseDir relativeOrAbsPath = do
  dir' <- fromEither =<< resolveDir' baseDir relativeOrAbsPath
  file' <- fromEither =<< resolveFile' baseDir relativeOrAbsPath

  dirExists <- doesDirExist dir'
  fileExists <- doesFileExist file'

  case (dirExists, fileExists) of
    (True, _) -> pure $ SDir dir'
    (_, True) -> pure $ SFile file'
    _ -> fatalText $ "Provided path: " <> relativeOrAbsPath <> " does not exist!"

metadataOf :: (Has (Lift IO) sig m, Has ReadFS sig m, Has Diagnostics sig m)
  => Path Abs Dir
  -> Dependency
  -> m (Maybe (Dependency, SomeAbsPath, Text))
metadataOf baseDir dep = case dependencyType dep of
  UnresolvedPathType -> do
    let depPath = dependencyName dep
    depPath' <- absPathOf baseDir depPath
    hash <- hashOf depPath'
    pure $ Just (dep, depPath', hash)
  _ -> pure Nothing