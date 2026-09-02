module Strategy.Node.Pnpm.PnpmLock (
  analyze,

  -- * for testing
  buildGraph,
  parsePnpmLockfile,
  resolveImporterKey,
) where

import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, Has, context, errSupport, fatal)
import Control.Monad (when)
import Data.Aeson.Types (Value, parseEither, parseJSON)
import Data.ByteString (ByteString)
import Data.Either (partitionEithers)
import Data.Foldable (for_)
import Data.HashMap.Strict qualified as HashMap
import Data.List (foldl')
import Data.Map (Map, toList)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.String.Conversion (toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Yaml (decodeAllEither', prettyPrintParseException)
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
import Effect.ReadFS (ReadFS, ReadFSErr (FileParseError), fileParseErrorSupportMsg, readContentsBS)
import Graphing (Graphing)
import Graphing qualified
import Path (Abs, File, Path)
import Strategy.Node.Pnpm.Types (
  BuildGraphConfig (..),
  GitResolution (..),
  LabelingMode (..),
  PackageData (..),
  PnpmCatalogs (..),
  PnpmLockfile (..),
  PnpmLockfileBase (..),
  PnpmLockfileV4Or5 (..),
  PnpmLockfileV678 (..),
  PnpmLockfileV9 (..),
  ProjectMap (..),
  ProjectMapDepMetadata (..),
  Resolution (..),
  TarballResolution (..),
  withoutPeerDepSuffix,
 )
import Strategy.Node.Pnpm.V4_8 (
  buildGraphConfigV4or5,
  buildGraphConfigV678,
 )
import Strategy.Node.Pnpm.V9 (buildGraphConfigV9)

-- | Label attached to direct dependencies so that hydrateDepEnvs can
-- propagate environments to transitive successors. Used only for v9
-- lockfiles where the @dev@ field on packages is unreliable.
newtype PnpmLabel = PnpmEnv DepEnvironment
  deriving (Eq, Ord)

--
-- Shared helpers (version-independent)
--

-- | Convert a resolved package into a 'Dependency' node.
toDependency ::
  (Bool -> Set.Set DepEnvironment) ->
  Text ->
  Maybe Text ->
  PackageData ->
  Dependency
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
toDep ::
  (Bool -> Set.Set DepEnvironment) ->
  DepType ->
  Text ->
  Maybe Text ->
  Bool ->
  Dependency
toDep toEnv depType name version isDev =
  Dependency depType name (CEq <$> version) mempty (toEnv isDev) mempty

-- | Sometimes package versions include symlinked paths
-- of sibling dependencies used for resolution.
--
-- >> withoutSymConstraint "1.2.0" = "1.2.0"
-- >> withoutSymConstraint "1.2.0_vue@3.0" = "1.2.0"
withoutSymConstraint :: Text -> Text
withoutSymConstraint version = fst $ Text.breakOn "_" version

-- | Resolve a catalog reference to its actual version.
--
-- If the version is a @catalog:name@ reference, looks up the package
-- in the catalog map. Otherwise returns the version unchanged.
resolveCatalogVersion :: PnpmCatalogs -> Text -> Text -> Text
resolveCatalogVersion (PnpmCatalogs cats) depName ver
  | Just catalogName <- Text.stripPrefix "catalog:" ver =
      let name = if Text.null catalogName then "default" else catalogName
       in fromMaybe ver $ Map.lookup name cats >>= Map.lookup depName
  | otherwise = ver

-- | Apply accumulated labels to transform a graph node.
applyLabels :: Dependency -> Set.Set PnpmLabel -> Dependency
applyLabels = foldr applyLabel
  where
    applyLabel (PnpmEnv env) = insertEnvironment env

-- | Strip local (file:) packages from the final graph.
withoutLocalPackages :: Graphing Dependency -> Graphing Dependency
withoutLocalPackages = Graphing.shrink (\dep -> dependencyType dep /= UserType)

--
-- Resolved dependency lookup
--

-- | Resolve a dependency name and version to a 'Dependency' by looking it up
-- in the packages map.
--
-- Non-registry resolvers (tarball, git, directory) use the version value
-- directly as the @packages@ key. Registry resolvers use a constructed key.
toResolvedDependency ::
  -- | toEnv for this version
  (Bool -> Set.Set DepEnvironment) ->
  Map Text PackageData ->
  -- | mkPkgKey for this version
  (Text -> Text -> Text) ->
  -- | dependency name
  Text ->
  -- | dependency version
  Text ->
  Maybe Dependency
toResolvedDependency toEnv pkgs mkPkg depName depVersion = do
  -- Some versions of the lockfile remove the peer dep suffix.
  -- Others do not which is why it tries both.
  let strippedVersion = withoutPeerDepSuffix depVersion
  let maybeNonRegistrySrcPackage =
        Map.lookup depVersion pkgs
          <|> Map.lookup strippedVersion pkgs
  let maybeRegistrySrcPackage =
        fmap (depVersion,) (Map.lookup (mkPkg depName depVersion) pkgs)
          <|> fmap (strippedVersion,) (Map.lookup (mkPkg depName strippedVersion) pkgs)
  case (maybeNonRegistrySrcPackage, maybeRegistrySrcPackage) of
    (Nothing, Nothing) -> Nothing
    (Just nonRegistryPkg, _) ->
      Just $ toDependency toEnv depName Nothing nonRegistryPkg
    (Nothing, Just (version, registryPkg)) ->
      Just $ toDependency toEnv depName (Just version) registryPkg

--
-- Shared graph-building loop
--

-- | Core graph-building logic shared across all lockfile versions.
--
-- The first argument is the set of importer keys to treat as direct-dependency
-- sources, or 'Nothing' to use every importer in the lockfile. See
-- 'scopedImporters'.
buildGraphCore :: Maybe (Set.Set Text) -> BuildGraphConfig -> PnpmLockfileBase -> Graphing Dependency
buildGraphCore selection BuildGraphConfig{bgcGetPkgNameVersion, bgcMkPkgKey, bgcToEnv, bgcLabelingMode, bgcSnapshotEdges, bgcCatalogs} base =
  let getPkgNameVersion = bgcGetPkgNameVersion
      mkPkgKey = bgcMkPkgKey
      toEnv = bgcToEnv
      labelingMode = bgcLabelingMode
      snapshotEdges = bgcSnapshotEdges
      catalogs = bgcCatalogs
      pkgs = lockfilePackages base
      snapshotEdgesHM = HashMap.fromList snapshotEdges
      importers = maybe (lockfileImporters base) (Map.restrictKeys (lockfileImporters base)) selection
      -- Every entry in `packages` is added as a deep node below, so a scoped
      -- graph would otherwise still carry the whole workspace's dependencies,
      -- just with a smaller direct set. Prune to what the selected importers
      -- can actually reach. Unscoped analysis skips this so its output is
      -- unchanged.
      pruneIfScoped = maybe id (const Graphing.pruneUnreachable) selection
   in pruneIfScoped . withoutLocalPackages . hydrateDepEnvs $
        run . withLabeling applyLabels $ do
          -- Direct dependencies from each importer (workspace package).
          for_ (toList importers) $ \(_, projectImporters) -> do
            for_ (Map.toList $ directDependencies projectImporters) $ \(depName, ProjectMapDepMetadata depVersion) ->
              let resolvedVersion = resolveCatalogVersion catalogs depName depVersion
               in for_ (toResolvedDependency toEnv pkgs mkPkgKey depName resolvedVersion) $ \dep -> do
                    direct dep
                    case labelingMode of
                      LabelingOn -> label dep (PnpmEnv EnvProduction)
                      LabelingOff -> pure ()

            for_ (Map.toList $ directDevDependencies projectImporters) $ \(depName, ProjectMapDepMetadata depVersion) ->
              let resolvedVersion = resolveCatalogVersion catalogs depName depVersion
               in for_ (toResolvedDependency toEnv pkgs mkPkgKey depName resolvedVersion) $ \dep -> do
                    direct dep
                    case labelingMode of
                      LabelingOn -> label dep (PnpmEnv EnvDevelopment)
                      LabelingOff -> pure ()

          -- Deep dependencies and edges from the packages section.
          for_ (toList pkgs) $ \(pkgKey, pkgMeta) -> do
            let deepDependencies =
                  Map.toList (dependencies pkgMeta)
                    <> Map.toList (peerDependencies pkgMeta)
                    <> fromMaybe mempty (HashMap.lookup pkgKey snapshotEdgesHM)

            let (depName, depVersion) = case getPkgNameVersion pkgKey of
                  Nothing -> (pkgKey, Nothing)
                  Just (name, version) -> (name, Just version)
            let parentDep = toDependency toEnv depName depVersion pkgMeta

            -- It is ok if this dependency was already graphed as direct
            -- @direct 1 <> deep 1 = direct 1@
            deep parentDep

            for_ deepDependencies $ \(deepName, deepVersion) -> do
              maybe (pure ()) (edge parentDep) (toResolvedDependency toEnv pkgs mkPkgKey deepName deepVersion)

--
-- Workspace scoping
--

-- | The base fields of a lockfile, whatever its version.
lockfileBaseOf :: PnpmLockfile -> PnpmLockfileBase
lockfileBaseOf (LockfileV4Or5 (PnpmLockfileV4Or5 base)) = base
lockfileBaseOf (LockfileV678 (PnpmLockfileV678 base)) = base
lockfileBaseOf (LockfileV9 v) = lockfileBase v

-- | The importers whose direct dependencies should be graphed, given the
-- importer keys resolved from the selected build targets by
-- 'Strategy.Node.resolvePnpmImporterKeys'.
--
-- 'Nothing' means the analysis is unscoped, which must reproduce pre-scoping
-- output exactly. That happens both when no target filter is applied and when
-- the selection turns out to cover every importer in the lockfile, which is
-- the default case where all targets are selected.
--
-- A selection that matches no importer yields @Just Set.empty@: nothing is
-- direct, so pruning leaves an empty graph. 'analyze' warns when that happens
-- rather than quietly falling back to the whole workspace.
scopedImporters :: Maybe (Set.Set Text) -> PnpmLockfileBase -> Maybe (Set.Set Text)
scopedImporters Nothing _ = Nothing
scopedImporters (Just keys) base =
  if selected == allImporters then Nothing else Just selected
  where
    allImporters = Map.keysSet (lockfileImporters base)
    selected = expandWorkspaceLinks base (keys `Set.intersection` allImporters)

-- | Grow a selection of importer keys to include the workspace importers that
-- those importers link to, transitively.
--
-- pnpm records a dependency on a sibling workspace package as
-- @version: link:\<relative path\>@ rather than as an entry in @packages@, so
-- the sibling's own dependencies live under its importer key and nowhere else.
-- Unscoped analysis merges every importer, so those dependencies land in the
-- graph regardless of who declared them; once a selection is applied they would
-- disappear. Following the links keeps a scoped result complete.
expandWorkspaceLinks :: PnpmLockfileBase -> Set.Set Text -> Set.Set Text
expandWorkspaceLinks base = go Set.empty . Set.toList
  where
    importers = lockfileImporters base

    go :: Set.Set Text -> [Text] -> Set.Set Text
    go seen [] = seen
    go seen (key : rest)
      | key `Set.member` seen = go seen rest
      | otherwise = go (Set.insert key seen) (linkedFrom key <> rest)

    linkedFrom :: Text -> [Text]
    linkedFrom key = case Map.lookup key importers of
      Nothing -> []
      Just projectMap ->
        mapMaybe (linkTarget key . version) $
          Map.elems (directDependencies projectMap) <> Map.elems (directDevDependencies projectMap)

    -- A link is only followed when it names an importer the lockfile actually
    -- has; a @link:@ pointing outside the workspace resolves to nothing.
    linkTarget :: Text -> Text -> Maybe Text
    linkTarget fromKey ver = do
      relPath <- Text.stripPrefix "link:" ver
      let key = resolveImporterKey fromKey relPath
      if key `Map.member` importers then Just key else Nothing

-- | Resolve a path relative to an importer back into importer-key form:
-- forward slashes, @.@ and @..@ segments collapsed, and @"."@ for the
-- workspace root.
--
-- >> resolveImporterKey "browser" "../server" = "server"
-- >> resolveImporterKey "apps/web" "../../libs/ui" = "libs/ui"
-- >> resolveImporterKey "browser" "../" = "."
resolveImporterKey :: Text -> Text -> Text
resolveImporterKey fromKey relPath = toKey $ foldl' step [] segments
  where
    segments :: [Text]
    segments =
      concatMap (filter (not . Text.null) . Text.splitOn "/" . Text.replace "\\" "/") [fromKey, relPath]

    -- The accumulator is in reverse order, so ".." drops its head.
    step :: [Text] -> Text -> [Text]
    step acc ".." = drop 1 acc
    step acc "." = acc
    step acc segment = segment : acc

    toKey :: [Text] -> Text
    toKey [] = "."
    toKey acc = Text.intercalate "/" (reverse acc)

--
-- Top-level dispatch
--

-- | Build the dependency graph, labeling direct deps with their environment
-- (prod\/dev). hydrateDepEnvs then propagates those environments to all
-- transitive successors.
--
-- The first argument scopes the graph to a set of workspace importer keys; see
-- 'scopedImporters'.
buildGraph :: Maybe (Set.Set Text) -> PnpmLockfile -> Graphing Dependency
buildGraph selection lockfile = case lockfile of
  LockfileV4Or5 (PnpmLockfileV4Or5 base) -> withSelection buildGraphConfigV4or5 base
  LockfileV678 (PnpmLockfileV678 base) -> withSelection buildGraphConfigV678 base
  LockfileV9 v -> withSelection (buildGraphConfigV9 v) (lockfileBase v)
  where
    withSelection config base = buildGraphCore (scopedImporters selection base) config base

-- | Parse the contents of a pnpm-lock.yaml file.
--
-- pnpm v11 can write the lockfile as a multi-document YAML stream. In practice
-- the stream is exactly two documents: a metadata front-document (pnpmfile
-- checksum, config dependency integrity, etc. — no dependency data) followed by
-- the lockfile document, which still carries all of the importers\/packages\/
-- snapshots data. Only the lockfile document parses as a 'PnpmLockfile' (the
-- metadata document has no @lockfileVersion@), so selecting the first document
-- that parses as a lockfile analyzes the full dependency data rather than
-- rejecting the stream with "Multiple YAML documents encountered".
parsePnpmLockfile :: ByteString -> Either Text PnpmLockfile
parsePnpmLockfile contents = case decodeAllEither' contents of
  Left err -> Left . toText $ prettyPrintParseException err
  Right (docs :: [Value]) -> case partitionEithers $ map (parseEither parseJSON) docs of
    (_, lockfile : _) -> Right lockfile
    ([], []) -> Left "no YAML documents found"
    (errs, []) -> Left . Text.intercalate "\n" $ map toText errs

-- | Analyze a pnpm lockfile, optionally scoped to the given workspace importer
-- keys (@"."@ for the root, @"packages/a"@ for a member), as resolved from the
-- selected build targets by 'Strategy.Node.resolvePnpmImporterKeys'. 'Nothing'
-- means no target filter is applied, so the whole workspace is graphed.
analyze :: (Has ReadFS sig m, Has Logger sig m, Has Diagnostics sig m) => Maybe (Set.Set Text) -> Path Abs File -> m (Graphing Dependency)
analyze selectedImporters file = context "Analyzing Pnpm Lockfile" $ do
  pnpmLockFile <- context "Parsing pnpm-lock file" $
    context ("Parsing YAML file '" <> toText (toString file) <> "'") $ do
      contents <- readContentsBS file
      case parsePnpmLockfile contents of
        Left err -> errSupport (fileParseErrorSupportMsg file) . fatal $ FileParseError (toString file) err
        Right lockfile -> pure lockfile

  -- Warn about unsupported versions (v1-v3).
  case pnpmLockFile of
    LockfileV4Or5 (PnpmLockfileV4Or5 base) ->
      case Text.uncons (lockfileRawVersion base) of
        Just (c, _)
          | c `elem` ['1', '2', '3'] ->
              logWarn . pretty $ "pnpm-lock file is using older lockFileVersion: " <> lockfileRawVersion base <> ", which is not officially supported!"
        _ -> pure ()
    LockfileV678 _ -> pure ()
    LockfileV9 _ -> pure ()

  let scoped = scopedImporters selectedImporters (lockfileBaseOf pnpmLockFile)
  case (selectedImporters, scoped) of
    (Just keys, Just selected) ->
      when (Set.null selected) . logWarn . pretty $
        "Target filter (resolved importer keys: "
          <> Text.intercalate ", " (Set.toList keys)
          <> ") did not match any importer in the pnpm lockfile; reporting an empty dependency graph."
    _ -> pure ()

  context "Building dependency graph" $ pure $ buildGraph selectedImporters pnpmLockFile
