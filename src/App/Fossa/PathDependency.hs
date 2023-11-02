
module App.Fossa.PathDependency (
  enrichPathDependencies
  -- licenseScanSourceUnit,
  -- combineLicenseUnits,
  -- scanVendoredDep,
  -- scanAndUploadPathDependency,
) where

import Path (Abs, Dir, Path, SomeBase (Abs, Rel), (</>), File, toFilePath)
import App.Fossa.LicenseScanner (scanDirectory, getPathPrefix)
import Data.Text (Text)
import DepTypes (Dependency(..), VerConstraint (CEq), DepType (..))
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Path ()
import Types (LicenseScanPathFilters)
import App.Types (FullFileUploads(..))
import Srclib.Types (LicenseSourceUnit(..), LicenseScanType (..))
import Effect.ReadFS
    ( Has,
      ReadFS,
      resolveFile',
      resolveDir',
      resolvePath',
      doesFileExist,
      doesDirExist,
      getCurrentDir',
      runReadFSIO )
import Effect.Exec (Exec, runExecIO)
import Control.Effect.StickyLogger (StickyLogger)
import Control.Effect.Diagnostics (Diagnostics, recover, fromEither)
import Path.Extra (SomePath(..))
import Data.String.Conversion (toString, toText)
import Control.Effect.FossaApiClient
    ( FossaApiClient,
      PackageRevision(..),
      getOrganization,
      uploadLicenseScanResult, getSignedLicenseScanUrlForPathDependency, finalizeLicenseScanForPathDependency )
import Control.Carrier.StickyLogger (logSticky)
import qualified Data.List.NonEmpty as NE
import Fossa.API.Types (Organization(..), ArchiveComponents (ArchiveComponents))
import Graphing (Graphing, gtraverse, vertexList)
import App.Fossa.Analyze.Project (ProjectResult (projectResultPath, projectResultGraph))
import Srclib.Converter (shouldPublishDep)
import Data.Flag (Flag, fromFlag)
import App.Fossa.Config.Analyze (IncludeAll(..))
import Control.Carrier.Diagnostics (fatalText, runDiagnostics)
import Effect.Logger (withDefaultLogger, Severity (..))
import Text.Pretty.Simple (pPrint)
import Control.Carrier.Stack (runStack)
import Path.IO (withSystemTempDir)
import Path.IO qualified as PathIO
import Discovery.Archive (mkZip)
import App.Fossa.VendoredDependency (hashFile, VendoredDependencyScanMode (..))
import Data.Maybe (catMaybes)

data SomeAbsPath = SDir (Path Abs Dir) | SFile (Path Abs File)


-- | True, if dependency is path dependency, otherwise False.
isPathDep :: Dependency -> Bool
isPathDep d = dependencyType d == PathType

-- | True, if dependency is path dependency that ought to be license scanned.
isValidPathDep :: Bool -> Dependency -> Bool
isValidPathDep leaveOtherEnvDeps d = 
  if leaveOtherEnvDeps then isPathDep d 
  else (isPathDep d && shouldPublishDep d)

-- | Updates revision of dependency
updateRevision :: Dependency -> Text -> Dependency
updateRevision dep ver = dep { dependencyVersion = Just $ CEq ver }

-- | Retrieves the hash of the directory
hashOf :: Has (Lift IO) sig m => Path Abs Dir -> m Text
hashOf pathDepPath = do sendIO $ withSystemTempDir "fossa-path-dep" (calculateHash pathDepPath)

-- | Retrieves the hash of the directory
hashOfFile :: Has (Lift IO) sig m => Path Abs File -> m Text
hashOfFile f = sendIO $ hashFile (toFilePath f)

-- | Calculate hash of the directory
calculateHash :: Has (Lift IO) sig m => Path Abs Dir -> Path Abs Dir -> m Text
calculateHash targetDir tempFileDir = do
  filePath <- sendIO $ PathIO.resolveFile tempFileDir "fosse-path-dep.zip"
  mkZip targetDir filePath
  sendIO $ hashFile (toFilePath filePath)

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
  let baseDir = projectResultPath pr
  let graph = projectResultGraph pr
  
  org <- getOrganization
  graph' <- scanAndUploadPathDependencyGraph (fromFlag IncludeAll includeAll) org baseDir graph
  pure $ pr {projectResultGraph = graph'}

-- | Scan and Uploads path dependency from a graph
scanAndUploadPathDependencyGraph :: (
    Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has FossaApiClient sig m
  ) => Bool -> Organization -> Path Abs Dir -> Graphing Dependency -> m (Graphing Dependency)
scanAndUploadPathDependencyGraph leaveUnfiltered org baseDir graph = do
  let fullFileUploads = FullFileUploads $ orgRequiresFullFileUploads org
  let maybePathDeps = NE.nonEmpty $ filter (isValidPathDep leaveUnfiltered) $ vertexList graph

  case maybePathDeps of
      Nothing -> pure graph
      Just pathDeps -> do
        -- 1. Get metadata about each path dependency, mainly absolute path, and hash
        let uniqDeps = dedupDeps pathDeps
        depsWithMetadata <- catMaybes . NE.toList <$> traverse (getPathDepsMetadata baseDir) uniqDeps
        let needToScanDepsWithMetadata = depsWithMetadata

        -- 2. Scan and upload path dependencies license scan findings
        scannedDeps <- traverse (scanAndUpload baseDir Nothing fullFileUploads) needToScanDepsWithMetadata

        -- 3. Finalize scan, by asking endpoint to build the provided components
        -- We do not need to provide orgId here, as endpoint makes locator
        finalizeLicenseScanForPathDependency $ ArchiveComponents (NE.toList archives) (False) fullFileUploads

        -- 4. Finally update the graph with dependencyName, with orgId, and
        -- dependencyHash. We ignore 
        pure graph


  where
    dedupDeps :: NE.NonEmpty Dependency -> NE.NonEmpty Dependency
    dedupDeps = NE.nub


-- | Performs license scanning and upload for single path dependency.
scanAndUpload :: ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has StickyLogger sig m
  , Has FossaApiClient sig m
  ) =>
  Path Abs Dir ->
  Maybe LicenseScanPathFilters ->
  FullFileUploads ->
  (Dependency, SomeAbsPath, Text) ->
  m (Dependency, Maybe Dependency)
scanAndUpload baseDir pathFilters fullFileUpload depMetadata = do
  maybeLicenseUnits <- recover $ scanDep baseDir pathFilters fullFileUpload depMetadata
  case maybeLicenseUnits of
    Nothing -> pure (dep, Nothing)
    Just licenseUnits -> (dep, ) <$> uploadDepScan dep' licenseUnits
  where
    dep :: Dependency
    dep = depOnly depMetadata

    dep' :: Dependency
    dep' = updateRevision dep (versionOnly depMetadata)

    depOnly :: (Dependency, SomeAbsPath, Text) -> Dependency
    depOnly (d, _, _) = d

    versionOnly :: (Dependency, SomeAbsPath, Text) -> Text
    versionOnly (_, _, v) = v


-- | Performs license scanning for single path dependency.
scanDep :: ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Exec sig m
  , Has ReadFS sig m
  ) =>
  Path Abs Dir ->
  Maybe LicenseScanPathFilters ->
  FullFileUploads ->
  (Dependency, SomeAbsPath, Text) ->
  m LicenseSourceUnit
scanDep baseDir licenseScanPathFilters fullFileUploads (dep, depPath, _) = do
  let vendoredPath = dependencyName dep
  scanPath <- resolvePath' baseDir $ toString vendoredPath

  licenseUnits <- case depPath of
    SDir dir -> scanDirectory Nothing mempty licenseScanPathFilters fullFileUploads dir
    SFile file -> fatalText $ "path dependency for single file is not supported: " <> (dependencyName dep)

  let srcUnit = LicenseSourceUnit vendoredPath CliLicenseScanned licenseUnits
  pure srcUnit

-- | Performs license scanning result upload for single path dependency.
uploadDepScan :: ( Has Diagnostics sig m
  , Has StickyLogger sig m
  , Has FossaApiClient sig m
  ) =>
  Dependency ->
  LicenseSourceUnit ->
  m (Maybe Dependency)
uploadDepScan dep licenseSourceUnit = do
  let dependencyPath = dependencyName dep
  depVer <- case getRawVersion dep of
    Just ver -> pure ver
    Nothing -> fatalText $ "path dependency does not have hashed version: " <> (dependencyName dep)
  
  -- Get signed url to upload result to s3
  signedURL <- getSignedLicenseScanUrlForPathDependency $ PackageRevision{packageVersion = depVer, packageName = dependencyPath}
  
  -- upload result to s3
  logSticky $ "Uploading license scan findings of: '" <> dependencyPath <> "' to secure S3 bucket"
  uploadLicenseScanResult signedURL licenseSourceUnit
  pure $ Just dep

  where
    getRawVersion :: Dependency -> Maybe Text
    getRawVersion (Dependency _ _ (Just (CEq ver)) _ _ _ ) = Just ver
    getRawVersion _ = Nothing

getPathDepsMetadata :: (Has (Lift IO) sig m, Has ReadFS sig m, Has Diagnostics sig m) 
  => Path Abs Dir 
  -> Dependency 
  -> m (Maybe (Dependency, SomeAbsPath, Text))
getPathDepsMetadata baseDir dep = case dependencyType dep of
  PathType -> do
    let depPath = dependencyName dep
    depPath' <- getNormalizedPathFromBase baseDir depPath

    hash <- case depPath' of
      (SDir dir) -> hashOf dir
      (SFile file) -> hashOfFile file
    pure $ Just (dep, depPath', hash)
  _ -> pure Nothing


-- | Transforms graph, by updating path dependencies revision. Updated revision
-- is hash of the content.
normalizePathDeps :: (Has (Lift IO) sig m, Has ReadFS sig m, Has Diagnostics sig m) 
  => Path Abs Dir 
  -> Graphing Dependency 
  -> m (Graphing (Dependency, Maybe SomeAbsPath))
normalizePathDeps baseDir = gtraverse go
  where
    go :: (Has (Lift IO) sig m, Has ReadFS sig m, Has Diagnostics sig m) => Dependency -> m (Dependency, Maybe SomeAbsPath)
    go dep = case dependencyType dep of
      PathType -> do
        let depPath = dependencyName dep
        depPath' <- getNormalizedPathFromBase baseDir depPath

        hash <- case depPath' of
          (SDir dir) -> hashOf dir
          (SFile file) -> hashOfFile file

        pure (updateRevision dep hash, Just depPath')
      _ -> pure (dep, Nothing)

    

-- Get normalized path, from root of the scan directory, so
-- -
-- - vendor/
-- -  squirrel/
-- -      ....
-- - project/
--    go.mod 
-- 
-- For, `fossa analyze project`: dependencyName: ./vendor/squirrel
-- *
-- 
getNormalizedPathFromBase :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> Text -> m (SomeAbsPath)
getNormalizedPathFromBase baseDir relativePathOrAbsPath = do
  dir' <- fromEither =<< resolveDir' baseDir relativePathOrAbsPath
  file' <- fromEither =<< resolveFile' baseDir relativePathOrAbsPath

  dirExists <- doesDirExist dir'
  fileExists <- doesFileExist file'

  case (dirExists, fileExists) of
    (True, _) -> pure $ SDir dir'
    (_, True) -> pure $ SFile file'
    _ -> fatalText $ "Provided path: " <> relativePathOrAbsPath <> " does not exist!"


debugMain :: Text -> IO ()
debugMain candidate = do 
  myDir <- runStack . withDefaultLogger SevInfo . runDiagnostics . runReadFSIO . runExecIO $ do
    baseDir <- getCurrentDir'
    case baseDir of
      Right baseDir' -> do 
        dir' <- fromEither =<< resolveDir' baseDir' candidate
        Just <$> hashOf dir'
        -- logInfo $ pretty $ show dir'
        -- Just <$> doesDirExist dir'
      Left _ -> pure Nothing

  pPrint myDir

-- getHash :: Has (Lift IO) sig m => Path Abs Dir => 
-- restrict 

-- Print => 