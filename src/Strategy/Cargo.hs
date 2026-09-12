{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Strategy.Cargo (
  discover,
  CargoMetadata (..),
  CargoProject (..),
  NodeDependency (..),
  NodeDepKind (..),
  PackageId (..),
  Resolve (..),
  ResolveNode (..),
  buildGraph,
  getDeps,
  mkProject,
  findProjects,

  -- * for testing
  Package (..),
  analyze,
  extractGitCommitHash,
  lockfileToMetadata,
  parseGitRepoUrl,
  parsePkgId,
  readLockfileSoft,
) where

import App.Fossa.Analyze.LicenseAnalyze (
  LicenseAnalyzeProject (licenseAnalyzeProject),
 )
import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProjectStaticOnly), analyzeProject)
import App.Fossa.Config.Analyze (StrategyConfig (useGitBackedCargoLocators), UseGitBackedCargoLocators (..))
import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  ToDiagnostic,
  context,
  errCtx,
  fatal,
  run,
  warn,
 )
import Control.Effect.Reader (Reader, ask)
import Control.Monad (guard, unless)
import Data.Aeson.Types (
  FromJSON (parseJSON),
  ToJSON,
  withObject,
  (.:),
  (.:?),
 )
import Data.Bifunctor (first)
import Data.Foldable (for_, traverse_)
import Data.Functor (void)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Conversion (toString, toText)
import Data.Text (Text, breakOn)
import Data.Text qualified as Text
import Data.Void (Void)
import Diag.Diagnostic (renderDiagnostic)
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue, WalkSkipAll),
  findFileNamed,
  walkWithFilters',
 )
import Effect.Exec (
  AllowErr (Never),
  Command (..),
  Exec,
  execJson,
  execThrow,
 )
import Effect.Grapher (
  direct,
  edge,
  label,
  withLabeling,
 )
import Effect.ReadFS (
  ReadFS,
  ReadFSErr (..),
  doesFileExist,
  readContentsText,
  readContentsText',
  readContentsToml,
 )
import Errata (Errata (..))
import GHC.Generics (Generic)
import Graphing (Graphing, shrinkRoots)
import Network.URI (parseURI, uriAuthority, uriPath, uriRegName)
import Path (Abs, Dir, File, Path, mkRelFile, parent, parseRelFile, toFilePath, (</>))
import Strategy.CargoLock qualified as Lock
import Text.Megaparsec (
  Parsec,
  choice,
  errorBundlePretty,
  lookAhead,
  optional,
  parse,
  takeRest,
  takeWhile1P,
  try,
 )
import Text.Megaparsec.Char (char, digitChar, space, string)
import Toml.Schema qualified
import Types (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (CargoType, UnresolvedPathType),
  Dependency (..),
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (CargoProjectType),
  GraphBreadth (Complete),
  License (License),
  LicenseResult (LicenseResult, licenseFile, licensesFound),
  LicenseType (LicenseFile, LicenseSPDX, UnknownType),
  VerConstraint (CEq),
  insertEnvironment,
 )

newtype CargoLabel
  = CargoDepKind DepEnvironment
  deriving (Eq, Ord, Show)

data PackageId = PackageId
  { pkgIdName :: Text.Text
  , pkgIdVersion :: Text.Text
  , pkgIdSource :: Text.Text
  }
  deriving (Eq, Ord, Show)

data PackageDependency = PackageDependency
  { pkgDepName :: Text.Text
  , pkgDepReq :: Text.Text
  , pkgDepKind :: Maybe Text.Text
  }
  deriving (Eq, Ord, Show)

data Package = Package
  { pkgName :: Text.Text
  , pkgVersion :: Text.Text
  , pkgId :: PackageId
  , pkgLicense :: Maybe Text.Text
  , pkgLicenseFile :: Maybe Text.Text
  , pkgDependencies :: [PackageDependency]
  , pkgSourceUrl :: Maybe Text.Text
  }
  deriving (Eq, Ord, Show)

data NodeDepKind = NodeDepKind
  { nodeDepKind :: Maybe Text.Text
  , nodeDepTarget :: Maybe Text.Text
  }
  deriving (Eq, Ord, Show)

data NodeDependency = NodeDependency
  { nodePkg :: PackageId
  , nodeDepKinds :: [NodeDepKind]
  }
  deriving (Eq, Ord, Show)

data ResolveNode = ResolveNode
  { resolveNodeId :: PackageId
  , resolveNodeDeps :: [NodeDependency]
  }
  deriving (Eq, Ord, Show)

newtype Resolve = Resolve
  { resolvedNodes :: [ResolveNode]
  }
  deriving (Eq, Ord, Show)

data CargoMetadata = CargoMetadata
  { metadataPackages :: [Package]
  , metadataWorkspaceMembers :: [PackageId]
  , metadataResolve :: Resolve
  }
  deriving (Eq, Ord, Show)

instance FromJSON PackageDependency where
  parseJSON = withObject "PackageDependency" $ \obj ->
    PackageDependency
      <$> obj .: "name"
      <*> obj .: "req"
      <*> obj .:? "kind"

instance FromJSON Package where
  parseJSON = withObject "Package" $ \obj ->
    Package
      <$> obj .: "name"
      <*> obj .: "version"
      <*> (obj .: "id" >>= parsePkgId)
      <*> obj .:? "license"
      <*> obj .:? "license_file"
      <*> obj .: "dependencies"
      <*> obj .:? "source"

instance FromJSON NodeDepKind where
  parseJSON = withObject "NodeDepKind" $ \obj ->
    NodeDepKind
      <$> obj .:? "kind"
      <*> obj .:? "target"

instance FromJSON NodeDependency where
  parseJSON = withObject "NodeDependency" $ \obj ->
    NodeDependency
      <$> (obj .: "pkg" >>= parsePkgId)
      <*> obj .: "dep_kinds"

instance FromJSON ResolveNode where
  parseJSON = withObject "ResolveNode" $ \obj ->
    ResolveNode
      <$> (obj .: "id" >>= parsePkgId)
      <*> obj .: "deps"

instance FromJSON Resolve where
  parseJSON = withObject "Resolve" $ \obj ->
    Resolve <$> obj .: "nodes"

instance FromJSON CargoMetadata where
  parseJSON = withObject "CargoMetadata" $ \obj ->
    CargoMetadata
      <$> obj .: "packages"
      <*> (obj .: "workspace_members" >>= traverse parsePkgId)
      <*> obj .: "resolve"

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
    outcome <- Lock.staticLockfileOutcome <$> readLockfile (cargoDir project)
    case outcome of
      Lock.StaticUseLockfile lock -> do
        (graph, graphBreadth) <- lockfileToDeps False (cargoDir project) lock
        pure $
          DependencyResults
            { dependencyGraph = graph
            , dependencyGraphBreadth = graphBreadth
            , dependencyManifestFiles = [cargoToml project]
            }
      Lock.StaticMissingLockfile -> fatal (MissingCargoLockFile (cargoDir project </> $(mkRelFile "Cargo.lock")))
      Lock.StaticUnparseableLockfile err -> fatal (UnparseableCargoLockFile (cargoDir project </> $(mkRelFile "Cargo.lock"), renderCargoLockError err))

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

cargoGenLockfileCmd :: Command
cargoGenLockfileCmd =
  Command
    { cmdName = "cargo"
    , cmdArgs = ["generate-lockfile"]
    , cmdAllowErr = Never
    , cmdEnvVars = Map.empty
    }

cargoMetadataCmd :: Command
cargoMetadataCmd =
  Command
    { cmdName = "cargo"
    , cmdArgs = ["metadata"]
    , cmdAllowErr = Never
    , cmdEnvVars = Map.empty
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
  lockParsed <- readLockfileSoft (cargoDir project)
  case lockParsed of
    Left readErr ->
      warn (FailedToReadCargoLockFile (cargoDir project </> $(mkRelFile "Cargo.lock"), renderReadFSErr readErr))
        >> analyzeViaMetadata emitGitBackedLocators project
    Right Nothing -> analyzeViaMetadata emitGitBackedLocators project
    Right (Just (Right lock)) -> lockfileToDeps emitGitBackedLocators (cargoDir project) lock
    Right (Just (Left err)) ->
      warn (FailedToParseCargoLockFile (cargoDir project </> $(mkRelFile "Cargo.lock"), renderCargoLockError err))
        >> analyzeViaMetadata emitGitBackedLocators project

-- | Read and parse the @Cargo.lock@ in @manifestDir@, if one exists.
-- 'Nothing' means no lockfile is present; @Just (Left err)@ means a lockfile
-- is present but did not parse; @Just (Right lock)@ is the parsed lockfile.
-- A lockfile that cannot be read (an IO error) aborts the analysis via the
-- 'ReadFS' fatal. Used by the static path, which has no fallback to
-- @cargo metadata@.
readLockfile :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m (Maybe (Either Lock.CargoLockError Lock.CargoLock))
readLockfile manifestDir = do
  let lockPath = manifestDir </> $(mkRelFile "Cargo.lock")
  exists <- doesFileExist lockPath
  if exists
    then fmap (Just . Lock.parseCargoLock) (readContentsText lockPath)
    else pure Nothing

-- | Read and parse the @Cargo.lock@ in @manifestDir@, if one exists.
-- 'Right Nothing' means no lockfile is present; @Right (Just (Left err))@
-- means a lockfile is present but did not parse; @Right (Just (Right lock))@
-- is the parsed lockfile. Unlike 'readLockfile', a lockfile that cannot be
-- read (an IO error) surfaces as @Left err@ instead of aborting, so the
-- caller can warn and fall back to @cargo metadata@. Used by the dynamic
-- ('analyze') path.
readLockfileSoft :: Has ReadFS sig m => Path Abs Dir -> m (Either ReadFSErr (Maybe (Either Lock.CargoLockError Lock.CargoLock)))
readLockfileSoft manifestDir = do
  let lockPath = manifestDir </> $(mkRelFile "Cargo.lock")
  exists <- doesFileExist lockPath
  if exists
    then do
      contents <- readContentsText' lockPath
      pure (contents >>= \text -> Right (Just (Lock.parseCargoLock text)))
    else pure (Right Nothing)

-- | Render a 'ReadFSErr' as a short human-readable reason string.
renderReadFSErr :: ReadFSErr -> Text
renderReadFSErr (FileReadError _ reason) = reason
renderReadFSErr (FileParseError _ reason) = reason
renderReadFSErr (ResolveError _ _ reason) = reason
renderReadFSErr (ListDirError _ reason) = reason
renderReadFSErr (NotDirOrFile path) = "path is neither a file nor a directory: " <> toText path
renderReadFSErr (UndeterminableFileType path) = "path is both a file and a directory: " <> toText path
renderReadFSErr (CurrentDirError reason) = reason

-- | Render a 'Lock.CargoLockError' as a human-readable diagnostic message.
renderCargoLockError :: Lock.CargoLockError -> Text
renderCargoLockError (Lock.TomlParseError reason) = reason
renderCargoLockError (Lock.UnsupportedVersion version) =
  "unsupported Cargo.lock version " <> toText version

-- | Build the dependency graph from a parsed lockfile and workspace manifest
-- analysis, without running @cargo@. Shared by the lockfile paths of
-- 'analyze' and 'AnalyzeProject.analyzeProjectStaticOnly'.
lockfileToDeps ::
  ( Has Diagnostics sig m
  , Has ReadFS sig m
  ) =>
  Bool ->
  Path Abs Dir ->
  Lock.CargoLock ->
  m (Graphing Dependency, GraphBreadth)
lockfileToDeps emitGitBackedLocators manifestDir lock =
  context "Building dependency graph" $ do
    members <- Lock.enumerateWorkspaceMembers manifestDir
    analysis <- Lock.analyzeManifests manifestDir members
    for_ (Lock.lockWarnings lock) warn
    for_ (Lock.manifestWarnings analysis) warn
    let graph = buildGraph emitGitBackedLocators (lockfileToMetadata lock members analysis)
    pure (graph, Complete)

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

newtype FailedToReadCargoLockFile = FailedToReadCargoLockFile (Path Abs File, Text)
instance ToDiagnostic FailedToReadCargoLockFile where
  renderDiagnostic (FailedToReadCargoLockFile (path, reason)) = do
    let header = "Could not read Cargo.lock for cargo manifest: " <> toText path <> ": " <> reason <> ". Falling back to `cargo metadata`."
    Errata (Just header) [] Nothing

newtype FailedToParseCargoLockFile = FailedToParseCargoLockFile (Path Abs File, Text)
instance ToDiagnostic FailedToParseCargoLockFile where
  renderDiagnostic (FailedToParseCargoLockFile (path, reason)) = do
    let header = "Could not parse Cargo.lock for cargo manifest: " <> toText path <> ": " <> reason <> ". Falling back to `cargo metadata`."
    Errata (Just header) [] Nothing

newtype MissingCargoLockFile = MissingCargoLockFile (Path Abs File)
instance ToDiagnostic MissingCargoLockFile where
  renderDiagnostic (MissingCargoLockFile path) = do
    let header =
          "Cannot analyze Cargo project statically: no Cargo.lock was found for cargo manifest: "
            <> toText path
            <> ". Static analysis reads Cargo.lock directly and cannot run `cargo metadata` to generate one."
    Errata (Just header) [] Nothing

newtype UnparseableCargoLockFile = UnparseableCargoLockFile (Path Abs File, Text)
instance ToDiagnostic UnparseableCargoLockFile where
  renderDiagnostic (UnparseableCargoLockFile (path, reason)) = do
    let header =
          "Cannot analyze Cargo project statically: could not parse Cargo.lock for cargo manifest: "
            <> toText path
            <> ": "
            <> reason
            <> "."
    Errata (Just header) [] Nothing

newtype FailedToGenLockFile = FailedToGenLockFile (Path Abs File)
instance ToDiagnostic FailedToGenLockFile where
  renderDiagnostic (FailedToGenLockFile path) = do
    let header = "Could not generate lock file for cargo manifest: " <> toText path
    Errata (Just header) [] Nothing

newtype FailedToRetrieveCargoMetadata = FailedToRetrieveCargoMetadata (Path Abs File)
instance ToDiagnostic FailedToRetrieveCargoMetadata where
  renderDiagnostic (FailedToRetrieveCargoMetadata path) = do
    let header = "Could not retrieve machine readable cargo metadata for: " <> toText path
    Errata (Just header) [] Nothing

type PackageIdSourceKind = Text.Text
type PackageIdSourceProtocol = Text.Text

-- | Extract the git repository host+path from a cargo source URL.
-- Input:  "git+https://github.com/fossas/locator-rs?tag=v3.0.3#54c..."
-- Output: Just "github.com/fossas/locator-rs"
parseGitRepoUrl :: Text -> Maybe Text
parseGitRepoUrl src = do
  stripped <- Text.stripPrefix "git+" src
  uri <- parseURI (toString stripped)
  auth <- uriAuthority uri
  let host = toText (uriRegName auth)
      rawPath = Text.dropWhile (== '/') (toText (uriPath uri))
      cleanPath = fromMaybe rawPath (Text.stripSuffix ".git" rawPath)
  guard (not (Text.null host) && not (Text.null cleanPath))
  pure (host <> "/" <> cleanPath)

-- | Extract the commit hash from a cargo package source URL.
-- Input:  "git+https://github.com/fossas/foundation-libs#4bc3762e73f371717566fb075d02e1d25b21146e"
-- Output: Just "4bc3762e73f371717566fb075d02e1d25b21146e"
extractGitCommitHash :: Text -> Maybe Text
extractGitCommitHash src = do
  guard (Text.isPrefixOf "git+" src)
  let (_, fragment) = breakOn "#" src
  commit <- Text.stripPrefix "#" fragment
  guard (not (Text.null commit))
  pure commit

-- | A map from PackageId to the package's source URL (which contains the commit hash for git deps).
type PackageSourceMap = Map.Map PackageId Text

-- | Build a lookup from package ID to source URL from the packages list.
buildPackageSourceMap :: [Package] -> PackageSourceMap
buildPackageSourceMap = Map.fromList . concatMap toEntry
  where
    toEntry pkg = case pkgSourceUrl pkg of
      Just src -> [(pkgId pkg, src)]
      Nothing -> []

toDependency :: Bool -> PackageSourceMap -> PackageId -> Set CargoLabel -> Dependency
toDependency emitGitBackedLocators sourceMap pkg =
  foldr
    applyLabel
    Dependency
      { dependencyType = depType
      , dependencyName = depName
      , dependencyVersion = Just $ CEq depVersion
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }
  where
    applyLabel :: CargoLabel -> Dependency -> Dependency
    applyLabel (CargoDepKind env) = insertEnvironment env

    -- For example:
    -- path+file:///some/file/path -> ("path", "file")
    -- file:///some/file/path      -> ("", "file")
    parseDepKindAndProtocol :: Text.Text -> (PackageIdSourceKind, PackageIdSourceProtocol)
    parseDepKindAndProtocol src = case breakOn "+" . Text.takeWhile (/= ':') $ src of
      (protocol, "") -> ("", protocol)
      (kind, protocol) -> (kind, protocol)

    depType = case parseDepKindAndProtocol $ pkgIdSource pkg of
      ("path", _) -> UnresolvedPathType
      (_, "file") -> UnresolvedPathType
      -- Using  a git type is probably more correct, but the git type doesn't have the ability
      -- to track the package name within a repo, and this has poor support for monorepos.
      -- ("git", _) -> GitType
      -- (_, "ssh") -> GitType
      _ -> CargoType

    -- For a path dependency, use the path as the package name. For example:
    -- path+file:///some/file/path -> /some/file/path
    -- For a git dependency when the server supports it, use repo-url#crate-name. For example:
    -- git+https://github.com/fossas/locator-rs?tag=v3.0.3#sha -> github.com/fossas/locator-rs#locator
    -- When the server does not support git-backed locators, fall back to the plain crate name.
    depName =
      let sourceUrl = Text.drop 2 $ snd $ breakOn "//" $ pkgIdSource pkg
       in case depType of
            UnresolvedPathType -> sourceUrl
            _
              | emitGitBackedLocators
              , Just repoUrl <- parseGitRepoUrl (pkgIdSource pkg) ->
                  repoUrl <> "#" <> pkgIdName pkg
            _ -> pkgIdName pkg

    -- For git dependencies without a tag, use the commit hash from the package source URL.
    -- For all other dependencies (including tagged git deps), use the crate version.
    depVersion = case (emitGitBackedLocators, untaggedGitCommitHash) of
      (True, Just commitHash) -> commitHash
      _ -> pkgIdVersion pkg

    -- Look up the commit hash for an untagged git dependency.
    -- Note: the `git+` here is from a URL like `git+https://github.com...`, not from a git+ locator.
    untaggedGitCommitHash :: Maybe Text
    untaggedGitCommitHash = case ("git+" `Text.isPrefixOf` pkgIdSource pkg, "?tag=" `Text.isInfixOf` pkgIdSource pkg) of
      (True, False) -> do
        sourceUrl <- Map.lookup pkg sourceMap
        extractGitCommitHash sourceUrl
      _ -> Nothing

-- A Cargo edge's kind ("build", "dev", or null) reflects the parent's manifest
-- declaration, not the path taken to reach the parent. We classify each package
-- by which workspace-rooted paths can reach it:
--
--   * Production: reachable from a workspace member via a path of null-kind
--     edges only. These packages are linked into the release artifact.
--
--   * Development: any package reachable (via any edge) from the target of a
--     non-null-kind edge. A "build" or "dev" edge marks the start of a subtree
--     that never ships in the release binary, and every descendant of that
--     subtree inherits Development.
--
-- A package can carry both labels when it's reachable by both kinds of paths.
--
-- We do not need a separate "dev-deps of prod-deps" case: Cargo only resolves
-- dev-dependencies for workspace members, so non-workspace edges with kind
-- "dev" do not appear in 'cargo metadata' output. The only non-null kind we
-- see on a non-workspace edge is "build".
--
-- Cargo is the only strategy with per-edge kinds; others (pnpm, yarn, poetry)
-- label nodes and propagate with 'hydrateDepEnvs'. That helper walks from a
-- labeled node to every dependency it declares, regardless of edge kind, so
-- a Production label on a workspace member would flow through a "dev" or
-- "build" edge and mislabel the dev/build subtree as Production. We roll our
-- own edge-filtered reachability here rather than generalize the shared helper.
buildGraph :: Bool -> CargoMetadata -> Graphing Dependency
buildGraph emitGitBackedLocators meta = shrinkRoots $
  run . withLabeling (toDependency emitGitBackedLocators sourceMap) $ do
    traverse_ direct (metadataWorkspaceMembers meta)
    for_ nodes $ \node ->
      for_ (resolveNodeDeps node) $ \dep ->
        edge (resolveNodeId node) (nodePkg dep)
    for_ (Set.toList prodReachable) $ \pkg ->
      label pkg (CargoDepKind EnvProduction)
    for_ (Set.toList devReachable) $ \pkg ->
      label pkg (CargoDepKind EnvDevelopment)
  where
    sourceMap = buildPackageSourceMap $ metadataPackages meta
    nodes = resolvedNodes (metadataResolve meta)
    workspaceMembers = Set.fromList (metadataWorkspaceMembers meta)

    -- These predicates are not mutually exclusive: a dep declared in both
    -- [dependencies] and [dev-dependencies] on the same parent carries both
    -- a null and a non-null kind, so the edge feeds prodAdj *and* devSeeds.
    isProdEdge dep = any (isNothing . nodeDepKind) (nodeDepKinds dep)
    isDevEdge dep = any (isJust . nodeDepKind) (nodeDepKinds dep)

    -- Adjacency containing only edges whose parent declares the child as a
    -- normal dependency (at least one kind is null). Production reachability
    -- must only traverse these — a build or dev edge breaks the release chain.
    prodAdj =
      Map.fromList $
        map
          (\node -> (resolveNodeId node, map nodePkg . filter isProdEdge . resolveNodeDeps $ node))
          nodes

    -- Every edge in the metadata graph, for Development reachability.
    allAdj =
      Map.fromList $
        map (\node -> (resolveNodeId node, map nodePkg (resolveNodeDeps node))) nodes

    -- Targets of any non-null-kind edge. Each seeds a Development subtree:
    -- the target and all its transitive descendants are never linked into
    -- a release build.
    devSeeds =
      Set.fromList $
        map nodePkg $
          concatMap (filter isDevEdge . resolveNodeDeps) nodes

    prodReachable = reachable prodAdj workspaceMembers
    devReachable = reachable allAdj devSeeds

reachable :: Map.Map PackageId [PackageId] -> Set PackageId -> Set PackageId
reachable adj = go Set.empty . Set.toList
  where
    go visited [] = visited
    go visited (x : xs) =
      if Set.member x visited
        then go visited xs
        else
          let children = fromMaybe [] (Map.lookup x adj)
           in go (Set.insert x visited) (children ++ xs)

-- | Rebuild a 'CargoMetadata' from a parsed 'CargoLock' plus workspace manifest
-- analysis, so 'buildGraph' can be driven without running @cargo metadata@.
--
-- The 'PackageId's are constructed so that 'toDependency' classifies each
-- dependency exactly as it would for the equivalent @cargo metadata@ output:
--
--   * registry / sparse packages use their source string verbatim, yielding
--     'CargoType' with the crate name and version;
--   * git packages keep their source verbatim in the package list (so the
--     commit hash is available to 'toDependency' via the source map) while the
--     'PackageId' itself drops the @#commit@ fragment, so untagged git deps
--     surface the commit hash as their version and tagged deps keep the crate
--     version, matching the metadata path;
--   * workspace members and other path packages use an absolute @file://@
--     source (with a @path+@ prefix for members), yielding 'UnresolvedPathType'
--     with the absolute path as the dependency name.
--
-- Dependency edge kinds come from the manifest analysis: a dependency declared
-- by a workspace member carries one 'NodeDepKind' per declared kind (a dep in
-- both @\[dependencies\]@ and @\[build-dependencies\]@ carries both a null and a
-- @"build"@ kind). Edges whose parent is not a member default to a single
-- null kind -- the documented, accepted gap (see the module header of
-- "Strategy.CargoLock").
lockfileToMetadata :: Lock.CargoLock -> [Lock.WorkspaceMember] -> Lock.ManifestAnalysis -> CargoMetadata
lockfileToMetadata lock members analysis =
  CargoMetadata
    { metadataPackages = packages
    , metadataWorkspaceMembers = memberIds
    , metadataResolve = Resolve resolveNodes
    }
  where
    memberDirs = Map.fromList [(Lock.memberName m, Lock.memberDir m) | m <- members]

    isMemberPath p = isNothing (Lock.packageSource p) && isJust (Map.lookup (Lock.packageName p) memberDirs)

    pkgIdentity p =
      let name = Lock.packageName p
          version = fromMaybe "*" (Lock.packageVersion p)
       in case Lock.packageSource p of
            Nothing ->
              case Map.lookup name memberDirs of
                Just dir -> (PackageId name version ("path+file://" <> toText (toFilePath dir)), Nothing)
                Nothing ->
                  case Map.lookup name (Lock.manifestPathDeps analysis) of
                    Just dir -> (PackageId name version ("file://" <> toText (toFilePath dir)), Nothing)
                    -- No member declared this path; fall back to a bare file
                    -- source so the dependency still resolves to
                    -- 'UnresolvedPathType'.
                    Nothing -> (PackageId name version ("file://" <> name), Nothing)
            Just src
              | "git+" `Text.isPrefixOf` src ->
                  -- Keep the query (?tag=/?branch=) but drop the trailing
                  -- #commit fragment from the id; the full source (with the
                  -- hash) is kept in the source map so 'toDependency' can
                  -- recover it.
                  let base = fst (breakOn "#" src)
                   in (PackageId name version base, Just src)
              | otherwise -> (PackageId name version src, Just src)

    labeled = [(p, pkgIdentity p) | p <- Lock.lockPackages lock]

    -- (name, version) -> PackageId, for resolving "name version" dep strings.
    pidByNameVer = Map.fromList [((Lock.packageName p, fromMaybe "*" (Lock.packageVersion p)), pid) | (p, (pid, _)) <- labeled]

    -- name -> [PackageId], a fallback for references that don't pin a version.
    pidByName = Map.fromListWith (++) [(Lock.packageName p, [pid]) | (p, (pid, _)) <- labeled]

    packages =
      [ Package (pkgIdName pid) (pkgIdVersion pid) pid Nothing Nothing [] srcUrl
      | (_, (pid, srcUrl)) <- labeled
      ]

    memberIds = [pid | (p, (pid, _)) <- labeled, isMemberPath p]

    resolveNodes =
      [ ResolveNode pid [NodeDependency (resolveTarget dep) (nodeDepKinds p dep) | dep <- Lock.packageDependencies p]
      | (p, (pid, _)) <- labeled
      ]

    resolveTarget dep =
      case Map.lookup (Lock.depName dep, fromMaybe "*" (Lock.depVersion dep)) pidByNameVer of
        Just pid -> pid
        Nothing ->
          case Map.lookup (Lock.depName dep) pidByName of
            Just (pid : _) -> pid
            _ -> PackageId (Lock.depName dep) (fromMaybe "*" (Lock.depVersion dep)) ""

    nodeDepKinds p dep =
      let parentName = Lock.packageName p
       in if isMemberPath p
            then case Map.lookup (parentName, Lock.depName dep) (Lock.depKindMap analysis) of
              Just kinds -> map depKindToNdk (Set.toList kinds)
              Nothing -> [prodNdk]
            else [prodNdk]

    depKindToNdk Lock.DepProd = prodNdk
    depKindToNdk Lock.DepDev = NodeDepKind (Just "dev") Nothing
    depKindToNdk Lock.DepBuild = NodeDepKind (Just "build") Nothing

    prodNdk = NodeDepKind Nothing Nothing

-- | Custom Parsec type alias
type PkgSpecParser a = Parsec Void Text a

-- | Parser for pre cargo v1.77.0 package ids.
oldPkgIdParser :: PkgSpecParser PackageId
oldPkgIdParser = do
  name <- takeWhile1P (Just "Package name") (/= ' ') <* char ' '
  version <- takeWhile1P (Just "Package version") (/= ' ') <* string " ("
  source <- takeWhile1P (Just "Package source") (/= ')')
  pure $
    PackageId
      { pkgIdName = name
      , pkgIdVersion = version
      , pkgIdSource = source
      }

type PkgName = Text
type PkgVersion = Text

-- | Parser for post cargo v1.77.0 package ids
newPkgIdParser :: PkgSpecParser PackageId
newPkgIdParser = eatSpaces (try longSpec <|> simplePkgSpec')
  where
    eatSpaces m = space *> m <* space

    -- Given the fragment: adler@1.0.2
    pkgName :: PkgSpecParser (PkgName, PkgVersion)
    pkgName = do
      -- Parse: adler
      name <- takeWhile1P (Just "Package name") (`notElem` ['@', ':'])
      -- Parse: @1.0.2
      version <- optional (choice [char '@', char ':'] *> semver)
      -- It's possible to specify a name with no version, use "*" in this case.
      pure (name, fromMaybe "*" version)

    simplePkgSpec' =
      pkgName >>= \(name, version) ->
        pure
          PackageId
            { pkgIdName = name
            , pkgIdVersion = version
            , pkgIdSource = ""
            }

    -- Given the spec: registry+https://github.com/rust-lang/crates.io-index#adler@1.0.2
    longSpec :: PkgSpecParser PackageId
    longSpec = do
      -- Parse: registry+https
      sourceInit <- takeWhile1P (Just "Initial URL") (/= ':')
      -- Parse: ://github.com/rust-lang/crates.io-index
      sourceRemaining <- takeWhile1P (Just "Remaining URL") (/= '#')
      let pkgSource = sourceInit <> sourceRemaining

      -- In cases where we can't find a real name, use text after the last slash as a name.
      -- e.g. file:///path/to/my/project/bar#2.0.0 has the name 'bar'
      -- Cases of this are generally path dependencies.
      -- Strip query parameters (e.g. ?tag=v0.3.6) before splitting, so that
      -- git+https://github.com/fossas/broker?tag=v0.3.6#0.3.6 yields "broker", not "broker?tag=v0.3.6".
      let fallbackName =
            maybe pkgSource NonEmpty.last
              . NonEmpty.nonEmpty
              . filter (/= "")
              . Text.split (== '/')
              $ Text.takeWhile (/= '?') sourceRemaining

      -- Parse (Optional): #adler@1.0.2
      nameVersion <- optional $ do
        void $ char '#'
        -- If there's only a version after '#', use the fallback as the name.
        ((fallbackName,) <$> semver)
          <|> pkgName

      let (name, version) = fromMaybe (fallbackName, "*") nameVersion
      pure $
        PackageId
          { pkgIdName = name
          , pkgIdVersion = version
          , pkgIdSource = pkgSource
          }

    -- In the grammar, a semver always appears at the end of a string and is the only
    -- non-terminal that starts with a digit, so don't bother parsing internally.
    semver = try (lookAhead digitChar) *> takeRest

-- Prior to Cargo 1.77.0, package IDs looked like this:
-- package version (source URL)
-- adler 1.0.2 (registry+https://github.com/rust-lang/crates.io-index)
--
-- For 1.77.0 and later, they look like this:
-- registry source URL with a fragment of package@version
-- registry+https://github.com/rust-lang/crates.io-index#adler@1.0.2
-- or
-- path source URL with a fragment of package@version
-- path+file:///Users/scott/projects/health-data/health_data#package_name@0.1.0
-- or
-- path source URL with a fragment of version
-- In this case we grab the last entry in the path to use for the package name
-- path+file:///Users/scott/projects/health-data/health_data#0.1.0
--
-- Package Spec: https://doc.rust-lang.org/cargo/reference/pkgid-spec.html
parsePkgId :: MonadFail m => Text.Text -> m PackageId
parsePkgId t = either fail pure $ first errorBundlePretty parseResult
  where
    parseResult = parse (try oldPkgIdParser <|> newPkgIdParser) "Cargo package spec" t
