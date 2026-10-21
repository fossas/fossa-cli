{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Strategy.Cargo (
  -- * Discovery and project model
  discover,
  findProjects,
  mkProject,
  CargoProject (..),
  getDeps,

  -- * Analysis
  analyze,

  -- * Cargo metadata types (re-exported for backward compatibility)
  CargoMetadata (..),
  Package (..),
  NodeDependency (..),
  NodeDepKind (..),
  PackageId (..),
  Resolve (..),
  ResolveNode (..),
  parsePkgId,

  -- * Lockfile analysis (re-exported for backward compatibility)
  CargoLock (..),
  CargoDependency (..),
  CargoLockError (..),
  parseCargoLock,
  parseDependencyString,
  WorkspaceMember (..),
  DepKind (..),
  ManifestAnalysis (..),
  enumerateWorkspaceMembers,
  analyzeManifests,
  LockfileSource (..),
  analysisLockfileSource,
  decideLockfileSource,
  StaticLockfileOutcome (..),
  staticLockfileOutcome,
  readLockfile,
  lockfileToDeps,
  lockfileToMetadata,
  MissingCargoLockFile (..),
  UnparseableCargoLockFile (..),
  FailedToReadCargoLockFile (..),
  FailedToParseCargoLockFile (..),

  -- * cargo metadata path (re-exported for backward compatibility)
  buildGraph,
  parseGitRepoUrl,
  extractGitCommitHash,
) where

import App.Fossa.Analyze.LicenseAnalyze (LicenseAnalyzeProject (licenseAnalyzeProject))
import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProjectStaticOnly), analyzeProject)
import App.Fossa.Config.Analyze (StrategyConfig (useGitBackedCargoLocators), UseGitBackedCargoLocators (..))
import Control.Effect.Diagnostics (Diagnostics, Has, context, errCtx, fatal, warn)
import Control.Effect.Reader (Reader, ask)
import Control.Monad (unless)
import Data.Aeson.Types (ToJSON)
import Data.Functor (void)
import Data.Maybe (catMaybes, isJust)
import Data.String.Conversion (toText)
import Data.Text qualified as Text
import Diag.Diagnostic (renderDiagnostic)
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (WalkStep (WalkContinue, WalkSkipAll), findFileNamed, walkWithFilters')
import Effect.Exec (Exec, execJson, execThrow)
import Effect.ReadFS (ReadFS, doesFileExist, readContentsToml)
import GHC.Generics (Generic)
import Graphing (Graphing)
import Path (Abs, Dir, File, Path, mkRelFile, parent, parseRelFile, toFilePath, (</>))
import Strategy.Cargo.Lock (
  CargoDependency (..),
  CargoLock (..),
  CargoLockError (..),
  DepKind (..),
  FailedToParseCargoLockFile (..),
  FailedToReadCargoLockFile (..),
  LockfileSource (..),
  ManifestAnalysis (..),
  MissingCargoLockFile (..),
  StaticLockfileOutcome (..),
  UnparseableCargoLockFile (..),
  WorkspaceMember (..),
  analysisLockfileSource,
  analyzeManifests,
  decideLockfileSource,
  enumerateWorkspaceMembers,
  lockfileToDeps,
  lockfileToMetadata,
  parseCargoLock,
  parseDependencyString,
  readLockfile,
  staticLockfileOutcome,
 )
import Strategy.Cargo.MetadataCmd (
  FailedToGenLockFile (..),
  FailedToRetrieveCargoMetadata (..),
  buildGraph,
  cargoGenLockfileCmd,
  cargoMetadataCmd,
  extractGitCommitHash,
  parseGitRepoUrl,
 )
import Strategy.Cargo.Types (CargoMetadata (..), NodeDepKind (..), NodeDependency (..), Package (..), PackageId (..), Resolve (..), ResolveNode (..), parsePkgId)
import Toml.Schema qualified
import Types (Dependency (..), DependencyResults (..), DiscoveredProject (..), DiscoveredProjectType (CargoProjectType), GraphBreadth (Complete), License (License), LicenseResult (LicenseResult, licenseFile, licensesFound), LicenseType (LicenseFile, LicenseSPDX, UnknownType))

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [DiscoveredProject CargoProject]
discover = simpleDiscover findProjects mkProject CargoProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [CargoProject]
findProjects = walkWithFilters' $ \dir _ files -> do
  case findFileNamed "Cargo.toml" files of
    Nothing -> pure ([], WalkContinue)
    Just toml -> do
      let project =
            CargoProject
              { cargoToml = toml
              , cargoDir = dir
              }

      pure ([project], WalkSkipAll)

data CargoProject = CargoProject
  { cargoDir :: Path Abs Dir
  , cargoToml :: Path Abs File
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON CargoProject

instance AnalyzeProject CargoProject where
  analyzeProject _ = getDeps
  analyzeProjectStaticOnly _ project = do
    readRes <- readLockfile (cargoDir project)
    outcome <- either fatal pure readRes
    case staticLockfileOutcome outcome of
      StaticUseLockfile lock -> do
        (graph, graphBreadth) <- lockfileToDeps False (cargoDir project) lock
        pure $
          DependencyResults
            { dependencyGraph = graph
            , dependencyGraphBreadth = graphBreadth
            , dependencyManifestFiles = [cargoToml project]
            }
      StaticMissingLockfile -> fatal (MissingCargoLockFile (cargoDir project </> $(mkRelFile "Cargo.lock")))
      StaticUnparseableLockfile err -> fatal (UnparseableCargoLockFile (cargoDir project </> $(mkRelFile "Cargo.lock"), renderDiagnostic err))

data CargoPackage = CargoPackage
  { license :: Maybe Text.Text
  , cargoLicenseFile :: Maybe FilePath
  -- ^ Path relative to Cargo.toml containing the license
  }
  deriving (Eq, Show)

instance Toml.Schema.FromValue CargoPackage where
  fromValue =
    Toml.Schema.parseTableFromValue $
      CargoPackage
        <$> Toml.Schema.optKey "license"
        <*> Toml.Schema.optKey "license-file"

-- | Representation of a Cargo.toml file. See
--  [here](https://doc.rust-lang.org/cargo/reference/manifest.html)
--  for a description of this format.
newtype CargoToml = CargoToml
  {cargoPackage :: CargoPackage}
  deriving (Eq, Show)

instance Toml.Schema.FromValue CargoToml where
  fromValue =
    Toml.Schema.parseTableFromValue $
      CargoToml
        <$> Toml.Schema.reqKey "package"

instance LicenseAnalyzeProject CargoProject where
  licenseAnalyzeProject = analyzeLicenses . cargoToml

-- | Analyze a Cargo.toml for license information. The format is documented
--  (here)[https://doc.rust-lang.org/cargo/reference/manifest.html#the-license-and-license-file-fields]
analyzeLicenses :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m [LicenseResult]
analyzeLicenses tomlPath = do
  pkg <- cargoPackage <$> readContentsToml tomlPath
  licensePathText <- maybe (pure Nothing) mkLicensePath (cargoLicenseFile pkg)

  -- The license-file field in Cargo.toml is relative to the dir of the
  -- Cargo.toml file. Generate an absolute path to license-file.
  let maybeLicense = license pkg
  let licenseCon = selectLicenseCon <$> maybeLicense
  pure
    [ LicenseResult
        { licenseFile = toFilePath tomlPath
        , licensesFound =
            catMaybes
              [ License <$> licenseCon <*> maybeLicense
              , License LicenseFile <$> licensePathText
              ]
        }
    ]
  where
    mkLicensePath path = case parseRelFile path of
      Just p -> pure . Just . toText $ parent tomlPath </> p
      Nothing ->
        warn ("Cannot parse 'license-file' value: " <> path)
          >> pure Nothing

    textElem c = isJust . Text.findIndex (== c)
    -- Old versions of Cargo allow '/' as a separator between SPDX values in
    -- 'license'. In that case 'license' can't be treated as a LicenseSPDX.
    selectLicenseCon licenseText =
      if textElem '/' licenseText
        then UnknownType
        else LicenseSPDX

mkProject :: CargoProject -> DiscoveredProject CargoProject
mkProject project =
  DiscoveredProject
    { projectType = CargoProjectType
    , projectBuildTargets = mempty
    , projectPath = cargoDir project
    , projectData = project
    }

getDeps :: (Has Exec sig m, Has Diagnostics sig m, Has ReadFS sig m, Has (Reader StrategyConfig) sig m) => CargoProject -> m DependencyResults
getDeps project = do
  strategyCfg <- ask @StrategyConfig
  (graph, graphBreadth) <- context "Cargo" . context "Dynamic analysis" $ analyze (unUseGitBackedCargoLocators $ useGitBackedCargoLocators strategyCfg) project
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = graphBreadth
      , dependencyManifestFiles = [cargoToml project]
      }

-- | Full (dynamic) analysis of a Cargo project.
--
-- When a 'Cargo.lock' exists in the manifest directory, it is read and
-- parsed and the dependency graph is built from it directly, without
-- running @cargo@. A lockfile that fails to parse emits an errata warning
-- and falls back to the pre-existing @cargo metadata@ path. When no
-- lockfile exists, the pre-existing @cargo metadata@ path (which generates
-- a lockfile if missing) is used unchanged.
analyze ::
  ( Has Exec sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  ) =>
  Bool ->
  CargoProject ->
  m (Graphing Dependency, GraphBreadth)
analyze emitGitBackedLocators project = do
  lockParsed <- readLockfile (cargoDir project)
  case lockParsed of
    Left readErr ->
      warn (FailedToReadCargoLockFile (cargoDir project </> $(mkRelFile "Cargo.lock"), renderDiagnostic readErr))
        >> analyzeViaMetadata emitGitBackedLocators project
    Right Nothing -> analyzeViaMetadata emitGitBackedLocators project
    Right (Just (Right lock)) -> lockfileToDeps emitGitBackedLocators (cargoDir project) lock
    Right (Just (Left err)) ->
      warn (FailedToParseCargoLockFile (cargoDir project </> $(mkRelFile "Cargo.lock"), renderDiagnostic err))
        >> analyzeViaMetadata emitGitBackedLocators project

-- | The pre-existing @cargo metadata@-based analysis path: generate a
-- lockfile if one is missing, then run @cargo metadata@.
analyzeViaMetadata ::
  ( Has Exec sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  ) =>
  Bool ->
  CargoProject ->
  m (Graphing Dependency, GraphBreadth)
analyzeViaMetadata emitGitBackedLocators (CargoProject manifestDir manifestFile) = do
  exists <- doesFileExist $ manifestDir </> $(mkRelFile "Cargo.lock")
  unless exists $
    void $
      context "Generating lockfile" $
        errCtx (FailedToGenLockFile manifestFile) $
          execThrow manifestDir cargoGenLockfileCmd
  meta <- errCtx (FailedToRetrieveCargoMetadata manifestFile) $ execJson @CargoMetadata manifestDir cargoMetadataCmd
  graph <- context "Building dependency graph" $ pure (buildGraph emitGitBackedLocators meta)
  pure (graph, Complete)
