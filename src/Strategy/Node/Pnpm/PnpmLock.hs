{-# LANGUAGE OverloadedRecordDot #-}

module Strategy.Node.Pnpm.PnpmLock (
  analyze,

  -- * for testing
  buildGraph,
)
where

import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, Has, context)
import Data.Aeson (FromJSON (..), withObject)
import Data.Aeson.Extra (TextLike (..))
import Data.Aeson.KeyMap (toHashMapText)
import Data.Foldable (for_)
import Data.HashMap.Strict qualified as HashMap
import Data.Map (Map, toList)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.String.Conversion (toString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Yaml (Object, Parser, (.!=), (.:), (.:?))
import Data.Yaml qualified as Yaml
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (GitType, NodeJSType, URLType, UserType),
  Dependency (Dependency, dependencyType),
  VerConstraint (CEq),
  hydrateDepEnvs,
  insertEnvironment,
 )
import Effect.Grapher (deep, direct, edge, label, run, withLabeling)
import Effect.Logger (
  Logger,
  logWarn,
  pretty,
 )
import Effect.ReadFS (ReadFS, readContentsYaml)
import Graphing (Graphing)
import Graphing qualified
import Path (Abs, File, Path)

-- | Label attached to direct dependencies so that hydrateDepEnvs can
-- propagate environments to transitive successors. Used only for v9
-- lockfiles where the @dev@ field on packages is unreliable.
newtype PnpmLabel = PnpmEnv DepEnvironment
  deriving (Eq, Ord)

-- | Pnpm Lockfile
--
-- Pnpm lockfile (v5) (in yaml) has the following shape (irrelevant fields omitted):
--
-- @
-- > lockFileVersion: 5.4
-- > importers:
-- >   .:
-- >     specifiers:
-- >       aws-sdk: ^2.0.0
-- >     dependencies:
-- >       aws-sdk: 2.1148.0
-- >     devDependencies:
-- >       react: 18.1.0
-- >
-- >   packages/a:
-- >     specifiers:
-- >       commander: 9.2.0
-- >     dependencies:
-- >       commander: 9.2.0
-- >
-- > packages:
-- >  /aws-sdk/2.1148.0:
-- >    dev: false
-- >    peerDependencies:
-- >      ...
-- >    dependencies:
-- >      buffer: 4.9.2
-- >
-- >  /react/18.1.0:
-- >    dev: true
-- >
-- >  /buffer/4.9.2:
-- >    dev: false
-- @
--
--  In this file,
--    * `importers`: refers to configurations imported/specified via package.json.
--      * Key of `importers` (e.g. ".") refers to package.json filepath.
--        * `specifier`: refers to version constraint specified in package.json.
--        * `dependencies`: refer to direct and resolved production dependencies.
--        * `devDependencies`: refer to direct and resolved development dependencies,
--
--    * `packages`: lists all resolved dependencies (it does not include root level workspace package, or root package itself)
--      * Key of `packages` refer (e.g. "/buffer/4.9.2:") denotes name of dependency and resolved version.
--          - For dependency resolved via registry resolver, format is: "/${dependencyName}/${resolvedVersion}".
--          - For dependency resolved via tarball resolver, format is: "${Url}".
--          - For dependency resolved via git resolver, format is: "${Url}".
--          - For dependency resolved via directory resolver, format is: "file:${relativePath}".
--
--
--  Pnpm lockfile (v6) differs (v5), in following manner:
--  -----------------------------------------------------
--
--    * `importers` shape merges specifiers and version, in singular object:
--      @
--      > importers:
--      >    dependencies:
--      >      aws-sdk:
--      >        specifier: 2.1148.0
--      >        version: 2.1148.0
--      @
--
--    * Key of `packages` refer (e.g. "/buffer@4.9.2") denotes name of dependency and resolved version using '@' separator
--        - For dependency resolved via registry resolver, format is: "/${dependencyName}@${resolvedVersion}${peerDepsInParenthesis}".
--      @
--      >   /ieee754@1.1.13:
--      >      resolution: {integrity: sha512...}
--      >      dev: false
--      >
--      >   /@clerk/nextjs@4.22.1(next@13.4.10)(react-dom@18.2.0)(react@18.2.0):
--      >       resolution: {integrity: sha512...}
--      @
--
--    * If project has set peerDependencies to be not auto installed, pnpm
--      by default, does not include them in the lockfile. So, no additional
--      work is required for newly introduced `settings.autoInstallPeers` field.
--      This means, that if user has chosen, not to install peerDependencies, they
--      won't be included in the lock-file, so no additional work is required by fossa-cli.
--      Note that, fossa-cli by default includes peer dependencies.
--
--  References:
--    - [pnpm](https://pnpm.io/)
--    - [pnpm-lockfile](https://github.com/pnpm/pnpm/blob/5cfd6d01946edcce86f62580bddc788d02f93ed6/packages/lockfile-types/src/index.ts)
--    - [pnpm-lockfile-v6](https://github.com/pnpm/pnpm/pull/5810/files)
data PnpmLockfile = PnpmLockfile
  { importers :: Map Text ProjectMap
  , packages :: Map Text PackageData
  , lockFileVersion :: PnpmLockFileVersion
  , lockFileSnapshots :: PnpmLockFileSnapshots
  -- ^ Dependency graph in lockfile version > 9
  , lockFileCatalogs :: PnpmCatalogs
  -- ^ Catalog definitions in lockfile version >= 9
  }
  deriving (Show, Eq, Ord)

-- | Catalogs parsed from lockfile. Maps catalog name to (package name -> resolved version).
-- See: https://pnpm.io/catalogs
newtype PnpmCatalogs = PnpmCatalogs
  { catalogEntries :: Map Text (Map Text Text)
  }
  deriving (Show, Eq, Ord, Semigroup, Monoid)

instance FromJSON PnpmCatalogs where
  parseJSON = withObject "PnpmCatalogs" $ \o -> do
    parsed <- traverse parseCatalog (toHashMapText o)
    pure $ PnpmCatalogs (Map.fromList $ HashMap.toList parsed)
    where
      parseCatalog :: Yaml.Value -> Parser (Map Text Text)
      parseCatalog = withObject "Catalog" $ \entries ->
        (Map.fromList . HashMap.toList)
          <$> traverse (withObject "CatalogEntry" (.: "version")) (toHashMapText entries)

-- | Resolve a @catalog:name@ version reference using the parsed catalogs section.
-- @catalog:@ (empty name) maps to the @default@ catalog.
-- @catalog:react19@ maps to the @react19@ catalog.
-- If the catalog or package is not found, the original version string is returned.
resolveCatalogVersion :: PnpmCatalogs -> Text -> Text -> Text
resolveCatalogVersion (PnpmCatalogs cats) depName ver
  | Just catalogName <- Text.stripPrefix "catalog:" ver =
      let name = if Text.null catalogName then "default" else catalogName
       in fromMaybe ver $ Map.lookup name cats >>= Map.lookup depName
  | otherwise = ver

type SnapshotDepName = Text

type SnapShotDepRev = Text

-- | Lockfile versions > 9 use snapshots to represent the dependency graph.
-- Example:
--   snapshots:
--
--     colorjs@0.1.9: {}
--
--     punycode@2.3.1: {}
--
--     uri-js@4.4.1:
--       dependencies:
--       punycode: 2.3.1
newtype PnpmLockFileSnapshots = PnpmLockFileSnapshots
  {snapshots :: HashMap.HashMap SnapshotDepName [(SnapshotDepName, SnapShotDepRev)]}
  deriving (Show, Eq, Ord, Semigroup, Monoid)

instance FromJSON PnpmLockFileSnapshots where
  parseJSON = withObject "Read PnpmLockFileSnapshots" $ \o ->
    do
      let readTransitiveDepPairs = withObject "Parse dependencies" $
            \ds -> do
              deps <- ds .:? "dependencies" .!= mempty
              pure . HashMap.toList $ deps
      snapshots <- traverse readTransitiveDepPairs o

      -- Remove the peer dependency suffix. It's present in the snapshot entry, but it's not present in packages
      -- section which is where we look the dependency up.
      let snapshots' = (HashMap.mapKeys withoutPeerDepSuffix) . toHashMapText $ snapshots
      pure $
        PnpmLockFileSnapshots{snapshots = snapshots'}

data PnpmLockFileVersion
  = PnpmLockLt4 Text
  | PnpmLock4Or5
  | PnpmLockV678 Text
  | PnpmLockV9
  deriving (Show, Eq, Ord)

instance FromJSON PnpmLockfile where
  parseJSON = Yaml.withObject "pnpm-lock content" $ \obj -> do
    rawLockFileVersion <- getVersion =<< obj .:? "lockfileVersion" .!= (TextLike mempty)
    importers <- obj .:? "importers" .!= mempty
    packages <- obj .:? "packages" .!= mempty
    -- PNPM 9 snapshots
    snapshots <- obj .:? "snapshots" .!= mempty
    -- PNPM 9 catalogs
    catalogs <- obj .:? "catalogs" .!= mempty

    -- Map pnpm non-workspace lockfile format to pnpm workspace lockfile format.
    --
    -- For lockfile without workspaces, the 'importers' field is not included in
    -- the lockfile. And 'dependencies' and 'devDependencies' are instead shown
    -- at the root level.
    --
    -- A project without a workspace is the same as having a single workspace at
    -- the path of ".".

    dependencies <- obj .:? "dependencies" .!= mempty
    devDependencies <- obj .:? "devDependencies" .!= mempty
    let virtualRootWs = ProjectMap dependencies devDependencies
    let refinedImporters =
          if Map.null importers
            then Map.insert "." virtualRootWs importers
            else importers

    pure $ PnpmLockfile{importers = refinedImporters, packages = packages, lockFileVersion = rawLockFileVersion, lockFileSnapshots = snapshots, lockFileCatalogs = catalogs}
    where
      getVersion (TextLike ver) = case (listToMaybe . toString $ ver) of
        (Just '1') -> pure $ PnpmLockLt4 ver
        (Just '2') -> pure $ PnpmLockLt4 ver
        (Just '3') -> pure $ PnpmLockLt4 ver
        (Just '4') -> pure PnpmLock4Or5
        (Just '5') -> pure PnpmLock4Or5
        (Just x) | x `elem` ['6', '7', '8'] -> pure $ PnpmLockV678 ver
        (Just '9') -> pure PnpmLockV9
        _ -> fail ("expected numeric lockfileVersion, got: " <> show ver)

data ProjectMap = ProjectMap
  { directDependencies :: Map Text ProjectMapDepMetadata
  , directDevDependencies :: Map Text ProjectMapDepMetadata
  }
  deriving (Show, Eq, Ord)

instance FromJSON ProjectMap where
  parseJSON = Yaml.withObject "ProjectMap" $ \obj ->
    ProjectMap
      <$> obj .:? "dependencies" .!= mempty
      <*> obj .:? "devDependencies" .!= mempty

newtype ProjectMapDepMetadata = ProjectMapDepMetadata
  { version :: Text
  }
  deriving (Show, Eq, Ord)

instance FromJSON ProjectMapDepMetadata where
  -- This is v5 lock format
  parseJSON (Yaml.String r) = pure $ ProjectMapDepMetadata r
  -- This is v6 lock format
  parseJSON (Yaml.Object obj) = ProjectMapDepMetadata <$> obj .: "version"
  parseJSON other = fail ("Invalid format; expected pure string or an object with a `version` field, got: " <> show other)

data PackageData = PackageData
  { isDev :: Bool
  , name :: Maybe Text -- only provided when non-registry resolver is used
  , resolution :: Resolution
  , dependencies :: Map Text Text
  , peerDependencies :: Map Text Text
  }
  deriving (Show, Eq, Ord)

instance FromJSON PackageData where
  parseJSON = Yaml.withObject "PackageData" $ \obj ->
    PackageData
      <$> (obj .:? "dev" .!= False)
      <*> obj .:? "name"
      <*> obj .: "resolution"
      <*> (obj .:? "dependencies" .!= mempty)
      <*> (obj .:? "peerDependencies" .!= mempty)

data Resolution
  = GitResolve GitResolution
  | RegistryResolve RegistryResolution
  | TarballResolve TarballResolution
  | DirectoryResolve DirectoryResolution
  deriving (Show, Eq, Ord)

data GitResolution = GitResolution
  { gitUrl :: Text
  , revision :: Text
  }
  deriving (Show, Eq, Ord)

newtype TarballResolution = TarballResolution {tarballUrl :: Text} deriving (Show, Eq, Ord)

newtype RegistryResolution = RegistryResolution {integrity :: Text} deriving (Show, Eq, Ord)

newtype DirectoryResolution = DirectoryResolution {directory :: Text} deriving (Show, Eq, Ord)

instance FromJSON Resolution where
  parseJSON = Yaml.withObject "Resolution" $ \obj ->
    gitRes obj <|> tarballRes obj <|> directoryRes obj <|> registryRes obj
    where
      directoryRes :: Object -> Parser Resolution
      directoryRes obj = DirectoryResolve . DirectoryResolution <$> obj .: "directory"

      registryRes :: Object -> Parser Resolution
      registryRes obj = RegistryResolve . RegistryResolution <$> obj .: "integrity"

      tarballRes :: Object -> Parser Resolution
      tarballRes obj = TarballResolve . TarballResolution <$> obj .: "tarball"

      gitRes :: Object -> Parser Resolution
      gitRes obj = GitResolve <$> (GitResolution <$> obj .: "repo" <*> obj .: "commit")

--
-- Shared helpers (version-independent)
--

-- | Convert a resolved package into a 'Dependency' node.
toDependency :: (Bool -> Set.Set DepEnvironment)
             -> Text -> Maybe Text -> PackageData -> Dependency
toDependency toEnv name maybeVersion (PackageData isDev _ (RegistryResolve _) _ _) =
  toDep toEnv NodeJSType name (withoutPeerDepSuffix . withoutSymConstraint <$> maybeVersion) isDev
toDependency toEnv _ _ (PackageData isDev _ (GitResolve (GitResolution url rev)) _ _) =
  toDep toEnv GitType url (Just rev) isDev
toDependency toEnv _ _ (PackageData isDev _ (TarballResolve (TarballResolution url)) _ _) =
  toDep toEnv URLType url Nothing isDev
toDependency toEnv _ _ (PackageData isDev (Just name) (DirectoryResolve _) _ _) =
  toDep toEnv UserType name Nothing isDev
toDependency toEnv name _ (PackageData isDev Nothing (DirectoryResolve _) _ _) =
  toDep toEnv UserType name Nothing isDev

-- | Construct a 'Dependency' from its components.
toDep :: (Bool -> Set.Set DepEnvironment)
      -> DepType -> Text -> Maybe Text -> Bool -> Dependency
toDep toEnv depType name version isDev =
  Dependency depType name (CEq <$> version) mempty (toEnv isDev) mempty

-- | Sometimes package versions include symlinked paths
-- of sibling dependencies used for resolution.
--
-- >> withoutSymConstraint "1.2.0" = "1.2.0"
-- >> withoutSymConstraint "1.2.0_vue@3.0" = "1.2.0"
withoutSymConstraint :: Text -> Text
withoutSymConstraint version = fst $ Text.breakOn "_" version

-- | Apply accumulated labels to transform a graph node.
applyLabels :: Dependency -> Set.Set PnpmLabel -> Dependency
applyLabels = foldr applyLabel
  where
    applyLabel (PnpmEnv env) = insertEnvironment env

-- | Strip local (file:) packages from the final graph.
withoutLocalPackages :: Graphing Dependency -> Graphing Dependency
withoutLocalPackages = Graphing.shrink (\dep -> dependencyType dep /= UserType)

--
-- Version-specific key parsers
--

-- | Parse a v5-style key: @\/name\/version@ (slash required).
--
-- >> getPkgNameVersionV5 "" = Nothing
-- >> getPkgNameVersionV5 "/pkg-a/1.0.0" = Just ("pkg-a", "1.0.0")
getPkgNameVersionV5 :: Text -> Maybe (Text, Text)
getPkgNameVersionV5 pkgKey = case Text.stripPrefix "/" pkgKey of
  Nothing -> Nothing
  Just txt -> do
    let (nameWithSlash, version) = Text.breakOnEnd "/" txt
    case (Text.stripSuffix "/" nameWithSlash, version) of
      (Just name, v) -> Just (name, v)
      _ -> Nothing

-- | Parse an @-style key (v6+). When @slashRequired@ is 'True', the key
-- must start with @/@ (v6/v7/v8). When 'False', the slash is optional (v9).
parseAtKey :: Bool -> Text -> Maybe (Text, Text)
parseAtKey slashRequired pkgKey =
  case Text.stripPrefix "/" pkgKey of
    Nothing | slashRequired -> Nothing
    Nothing -> Just pkgKey
    Just txt -> Just txt
  >>= \txt -> do
    let (nameAndVersion, peerDepInfo) = Text.breakOn "(" txt
    let (nameWithSlash, version) = Text.breakOnEnd "@" nameAndVersion
    case (Text.stripSuffix "@" nameWithSlash, version) of
      (Just name, v) -> Just (name, v <> peerDepInfo)
      _ -> Nothing

-- | Parse a v6/v7/v8-style key: @\/name@version@ (slash required).
--
-- >> getPkgNameVersionV6 "" = Nothing
-- >> getPkgNameVersionV6 "/pkg-a@1.0.0" = Just ("pkg-a", "1.0.0")
-- >> getPkgNameVersionV6 "/@angular/core@1.0.0(babel@1.0.0)" = Just ("@angular/core", "1.0.0(babel@1.0.0)")
getPkgNameVersionV6 :: Text -> Maybe (Text, Text)
getPkgNameVersionV6 = parseAtKey True

-- | Parse a v9-style key: @name@version@ (slash optional).
--
-- >> getPkgNameVersionV9 "@angular/core@1.0.0(babel@1.0.0)" = Just ("@angular/core", "1.0.0(babel@1.0.0)")
getPkgNameVersionV9 :: Text -> Maybe (Text, Text)
getPkgNameVersionV9 = parseAtKey False

-- | Build a registry package key for v5 format: @\/name\/version@
mkPkgKeyV5 :: Text -> Text -> Text
mkPkgKeyV5 name version = "/" <> name <> "/" <> version

-- | Build a registry package key for v6/v7/v8 format: @\/name@version@
mkPkgKeyV6 :: Text -> Text -> Text
mkPkgKeyV6 name version = "/" <> name <> "@" <> version

-- | Build a registry package key for v9 format: @name@version@ (no leading slash)
mkPkgKeyV9 :: Text -> Text -> Text
mkPkgKeyV9 name version = name <> "@" <> version

--
-- Version-specific environment resolvers
--

-- | For v4/v5/v6/v7/v8: derive environment directly from @dev@ field.
toEnvInline :: Bool -> Set.Set DepEnvironment
toEnvInline isDev = Set.singleton (if isDev then EnvDevelopment else EnvProduction)

-- | For v9: start with empty environments; labels set them later.
toEnvEmpty :: Bool -> Set.Set DepEnvironment
toEnvEmpty _ = mempty

--
-- Resolved dependency lookup
--

-- | Resolve a dependency name and version to a 'Dependency' by looking it up
-- in the packages map.
--
-- Non-registry resolvers (tarball, git, directory) use the version value
-- directly as the @packages@ key. Registry resolvers use a constructed key.
toResolvedDependency
  :: (Bool -> Set.Set DepEnvironment) -- ^ toEnv for this version
  -> Map Text PackageData
  -> (Text -> Text -> Text) -- ^ mkPkgKey for this version
  -> Text -- ^ dependency name
  -> Text -- ^ dependency version
  -> Maybe Dependency
toResolvedDependency toEnv pkgs mkPkg depName depVersion = do
  -- Some versions of the lockfile remove the peer dep suffix.
  -- Others do not which is why it tries both.
  let strippedVersion = withoutPeerDepSuffix depVersion
  let maybeNonRegistrySrcPackage =
        Map.lookup strippedVersion pkgs
          <|> Map.lookup depVersion pkgs
  let maybeRegistrySrcPackage =
        fmap (strippedVersion,) (Map.lookup (mkPkg depName strippedVersion) pkgs)
          <|> fmap (depVersion,) (Map.lookup (mkPkg depName depVersion) pkgs)
  case (maybeNonRegistrySrcPackage, maybeRegistrySrcPackage) of
    (Nothing, Nothing) -> Nothing
    (Just nonRegistryPkg, _) ->
      Just $ toDependency toEnv depName Nothing nonRegistryPkg
    (Nothing, Just (version, registryPkg)) ->
      Just $ toDependency toEnv depName (Just version) registryPkg

--
-- Shared graph-building loop
--

-- | Whether to label direct dependencies so that 'hydrateDepEnvs'
-- propagates environments through the graph.
--
-- V9 lockfiles start with empty environments and rely on label
-- propagation. Older versions set environments inline from the @dev@ field.
data LabelingMode = LabelingOff | LabelingOn
  deriving (Show, Eq, Ord)

-- | Configuration for 'buildGraphCore'.
--
-- Each field controls version-specific behavior:
--
--   - 'bgcGetPkgNameVersion': parse (name, version) from a package key
--   - 'bgcMkPkgKey': build a registry package key from (name, version)
--   - 'bgcToEnv': derive environment from @dev@ field (or ignore it for v9)
--   - 'bgcLabelingMode': whether to label direct deps for hydrateDepEnvs propagation
--   - 'bgcSnapshotEdges': additional edges from v9 snapshots (empty for older versions)
data BuildGraphConfig = BuildGraphConfig
  { bgcGetPkgNameVersion :: Text -> Maybe (Text, Text)
  , bgcMkPkgKey :: Text -> Text -> Text
  , bgcToEnv :: Bool -> Set.Set DepEnvironment
  , bgcLabelingMode :: LabelingMode
  , bgcSnapshotEdges :: [(SnapshotDepName, [(SnapshotDepName, SnapShotDepRev)])]
  }

-- | Core graph-building logic shared across all lockfile versions.
buildGraphCore :: BuildGraphConfig -> PnpmLockfile -> Graphing Dependency
buildGraphCore cfg lockFile =
  withoutLocalPackages . hydrateDepEnvs $
    run . withLabeling applyLabels $ do
      -- Direct dependencies from each importer (workspace package).
      for_ (toList lockFile.importers) $ \(_, projectImporters) -> do
        for_ (Map.toList $ directDependencies projectImporters) $ \(depName, ProjectMapDepMetadata depVersion) ->
          let resolvedVersion = resolveCatalogVersion lockFile.lockFileCatalogs depName depVersion
           in for_ (toResolvedDependency (bgcToEnv cfg) pkgs (bgcMkPkgKey cfg) depName resolvedVersion) $ \dep -> do
                direct dep
                case bgcLabelingMode cfg of
                  LabelingOn -> label dep (PnpmEnv EnvProduction)
                  LabelingOff -> pure ()

        for_ (Map.toList $ directDevDependencies projectImporters) $ \(depName, ProjectMapDepMetadata depVersion) ->
          let resolvedVersion = resolveCatalogVersion lockFile.lockFileCatalogs depName depVersion
           in for_ (toResolvedDependency (bgcToEnv cfg) pkgs (bgcMkPkgKey cfg) depName resolvedVersion) $ \dep -> do
                direct dep
                case bgcLabelingMode cfg of
                  LabelingOn -> label dep (PnpmEnv EnvDevelopment)
                  LabelingOff -> pure ()

      -- Deep dependencies and edges from the packages section.
      for_ (toList pkgs) $ \(pkgKey, pkgMeta) -> do
        let deepDependencies =
              Map.toList (dependencies pkgMeta)
                <> Map.toList (peerDependencies pkgMeta)
                <> fromMaybe mempty (HashMap.lookup pkgKey snapshotEdgesHM)

        let (depName, depVersion) = case bgcGetPkgNameVersion cfg pkgKey of
              Nothing -> (pkgKey, Nothing)
              Just (name, version) -> (name, Just version)
        let parentDep = toDependency (bgcToEnv cfg) depName depVersion pkgMeta

        -- It is ok if this dependency was already graphed as direct
        -- @direct 1 <> deep 1 = direct 1@
        deep parentDep

        for_ deepDependencies $ \(deepName, deepVersion) -> do
          maybe (pure ()) (edge parentDep) (toResolvedDependency (bgcToEnv cfg) pkgs (bgcMkPkgKey cfg) deepName deepVersion)
  where
    pkgs = packages lockFile
    snapshotEdgesHM = HashMap.fromList (bgcSnapshotEdges cfg)

--
-- Per-version graph builders
--

-- | Shared builder for v4 through v8: environment from @dev@ field,
-- no label propagation, no snapshot edges.
buildGraphPreV9
  :: (Text -> Maybe (Text, Text)) -- ^ getPkgNameVersion
  -> (Text -> Text -> Text) -- ^ mkPkgKey
  -> PnpmLockfile
  -> Graphing Dependency
buildGraphPreV9 getPkgNameVersion mkPkgKey =
  buildGraphCore (BuildGraphConfig
    { bgcGetPkgNameVersion = getPkgNameVersion
    , bgcMkPkgKey = mkPkgKey
    , bgcToEnv = toEnvInline
    , bgcLabelingMode = LabelingOff
    , bgcSnapshotEdges = mempty
    })

-- | Build graph for lockfile v4/v5.
--
-- v5 uses @\/name\/version@ keys and derives environment from @dev@ field.
buildGraphV4or5 :: PnpmLockfile -> Graphing Dependency
buildGraphV4or5 = buildGraphPreV9 getPkgNameVersionV5 mkPkgKeyV5

-- | Build graph for lockfile v6/v7/v8.
--
-- v6+ uses @\/name@version@ keys and derives environment from @dev@ field.
buildGraphV678 :: PnpmLockfile -> Graphing Dependency
buildGraphV678 = buildGraphPreV9 getPkgNameVersionV6 mkPkgKeyV6

-- | Build graph for lockfile v9.
--
-- v9 uses @name@version@ keys (no leading slash), derives environment via
-- label propagation, and uses snapshots for transitive dependency edges.
buildGraphV9 :: PnpmLockfile -> Graphing Dependency
buildGraphV9 lockFile =
  buildGraphCore (BuildGraphConfig
    { bgcGetPkgNameVersion = getPkgNameVersionV9
    , bgcMkPkgKey = mkPkgKeyV9
    , bgcToEnv = toEnvEmpty
    , bgcLabelingMode = LabelingOn
    , bgcSnapshotEdges = HashMap.toList lockFile.lockFileSnapshots.snapshots
    }) lockFile

--
-- Top-level dispatch
--

-- | Build the dependency graph, labeling direct deps with their environment
-- (prod\/dev). hydrateDepEnvs then propagates those environments to all
-- transitive successors.
buildGraph :: PnpmLockfile -> Graphing Dependency
buildGraph lockFile = case lockFileVersion lockFile of
  PnpmLockLt4 _ -> buildGraphV4or5 lockFile
  PnpmLock4Or5 -> buildGraphV4or5 lockFile
  PnpmLockV678 _ -> buildGraphV678 lockFile
  PnpmLockV9 -> buildGraphV9 lockFile

analyze :: (Has ReadFS sig m, Has Logger sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze file = context "Analyzing Pnpm Lockfile" $ do
  pnpmLockFile <- context "Parsing pnpm-lock file" $ readContentsYaml file

  case lockFileVersion pnpmLockFile of
    PnpmLockLt4 raw -> logWarn . pretty $ "pnpm-lock file is using older lockFileVersion: " <> raw <> " of, which is not officially supported!"
    _ -> pure ()

  context "Building dependency graph" $ pure $ buildGraph pnpmLockFile

-- Sometimes package versions include resolved peer dependency version
-- in parentheses. This is used by pnpm for dependency resolution, we do
-- not care about them, as they do not represent package version.
--
-- >> withoutPeerDepSuffix "1.2.0" = "1.2.0"
-- >> withoutPeerDepSuffix "1.2.0(babel@1.0.0)" = "1.2.0"
withoutPeerDepSuffix :: Text -> Text
withoutPeerDepSuffix = fst . Text.breakOn "("
