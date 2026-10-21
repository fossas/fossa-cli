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
import Data.Foldable (for_)
import Data.HashMap.Strict qualified as HashMap
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..), last)
import Data.Map (Map, toList)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Semigroup (sconcat)
import Data.Set qualified as Set
import Data.String.Conversion (toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Yaml (decodeAllEither', prettyPrintParseException)
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (GitType, NodeJSType, URLType, UserType),
  Dependency (Dependency, dependencyEnvironments, dependencyType),
  VerConstraint (CEq),
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
import Graphing.Hydrate (hydrate)
import Path (Abs, File, Path)
import Strategy.Node.Pnpm.Types (
  BuildGraphConfig (..),
  GitResolution (..),
  LabelingMode (..),
  PackageData (..),
  PnpmLockfile (..),
  PnpmLockfileBase (..),
  PnpmLockfileV4Or5 (..),
  PnpmLockfileV678 (..),
  PnpmLockfileV9 (..),
  ProjectMap (..),
  ProjectMapDepMetadata (..),
  Resolution (..),
  TarballResolution (..),
  resolveCatalogVersion,
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

-- | Keep the lockfile key alongside the reportable dependency so peer
-- contexts remain distinct during reachability and environment propagation.
--
-- The key is the lockfile's own @packages@ or @snapshots@ key, so two
-- resolutions of the same package stay separate nodes even though they
-- report the same dependency:
--
-- >> ("widget@1.0.0(peer@1.0.0)", widget)
-- >> ("widget@1.0.0(peer@2.0.0)", widget)
--
-- Older lockfile formats spell the key differently: @/widget\@1.0.0(peer\@2.0.0)@
-- in v6 through v8, and @/widget/1.0.0_peer\@2.0.0@ in v5.
type PnpmNode = (Text, Dependency)

applyLabels :: PnpmNode -> Set.Set PnpmLabel -> PnpmNode
applyLabels (key, dep) labels = (key, foldr (\(PnpmEnv env) -> insertEnvironment env) dep labels)

-- Merge environments only after traversing the distinct resolution contexts.
-- Hydrating after this collapse would leak one context's environments into
-- another context's transitive dependencies.
collapseNodes :: Graphing PnpmNode -> Graphing Dependency
collapseNodes graph = Graphing.gmap report graph
  where
    identity dep = dep{dependencyEnvironments = mempty}
    environments = Map.fromListWith (<>) [(identity dep, dependencyEnvironments dep) | (_, dep) <- Graphing.vertexList graph]
    report (_, dep) = dep{dependencyEnvironments = Map.findWithDefault mempty (identity dep) environments}

--
-- Resolved dependency lookup
--

-- | Resolve a dependency to its lockfile key and reportable metadata.
--
-- Non-registry resolvers (tarball, git, directory) use the version value
-- directly as the @packages@ key. Registry resolvers use a constructed key.
toResolvedDependency ::
  (Bool -> Set.Set DepEnvironment) ->
  Map Text PackageData ->
  (Text -> Text -> Text) ->
  Text ->
  Text ->
  Maybe PnpmNode
toResolvedDependency toEnv pkgs mkPkg depName depVersion =
  nonRegistry depVersion
    <|> nonRegistry (withoutPeerDepSuffix depVersion)
    <|> registry depVersion
    <|> registry (withoutPeerDepSuffix depVersion)
  where
    nonRegistry key = (key,) . toDependency toEnv depName Nothing <$> Map.lookup key pkgs
    registry version =
      let key = mkPkg depName version
       in (key,) . toDependency toEnv depName (Just version) <$> Map.lookup key pkgs

--
-- Shared graph-building loop
--

-- | Core graph-building logic shared across all lockfile versions.
--
-- The first argument maps reachable importers to their environments, or is
-- 'Nothing' to use every importer in the lockfile. See
-- 'scopedImporters'.
buildGraphCore :: Maybe ImporterEnvironments -> BuildGraphConfig -> PnpmLockfileBase -> Graphing Dependency
buildGraphCore selection BuildGraphConfig{bgcGetPkgNameVersion, bgcMkPkgKey, bgcToEnv, bgcLabelingMode, bgcSnapshotEdges, bgcCatalogs} base =
  let getPkgNameVersion = bgcGetPkgNameVersion
      mkPkgKey = bgcMkPkgKey
      -- Inline dev flags in older lockfiles describe the whole workspace.
      -- Scoped scans derive environments from the selected importers instead.
      toEnv = maybe bgcToEnv (const $ const mempty) selection
      labelingMode = maybe bgcLabelingMode (const LabelingOn) selection
      snapshotEdges = bgcSnapshotEdges
      catalogs = bgcCatalogs
      -- v9 metadata is shared across peer contexts, but each snapshot has its
      -- own edges. Add contextual keys backed by that shared package metadata.
      pkgs = Map.union (Map.fromList $ mapMaybe snapshotPackage snapshotEdges) (lockfilePackages base)
      snapshotPackage (key, _) = (key,) <$> Map.lookup (withoutPeerDepSuffix key) (lockfilePackages base)
      snapshotEdgesHM = HashMap.fromList snapshotEdges
      importers = maybe (lockfileImporters base) (Map.restrictKeys (lockfileImporters base) . Map.keysSet) selection
      -- Every entry in `packages` is added as a deep node below, so a scoped
      -- graph would otherwise still carry the whole workspace's dependencies,
      -- just with a smaller direct set. Prune to what the selected importers
      -- can actually reach. Unscoped analysis retains every package.
      pruneIfScoped = maybe id (const Graphing.pruneUnreachable) selection
   in collapseNodes
        . Graphing.shrink ((/= UserType) . dependencyType . snd)
        . hydrate (dependencyEnvironments . snd) (\envs (key, dep) -> (key, dep{dependencyEnvironments = envs}))
        . pruneIfScoped
        $ run . withLabeling applyLabels
        $ do
          -- Direct dependencies from each importer (workspace package).
          for_ (toList importers) $ \(importerKey, projectImporters) -> do
            -- Optional dependencies are production dependencies an install may
            -- skip on a platform that cannot use them; see 'ProjectMap'.
            let prodDependencies = Map.toList (directDependencies projectImporters) <> Map.toList (directOptionalDependencies projectImporters)
            for_ prodDependencies $ \(depName, ProjectMapDepMetadata depVersion) ->
              let resolvedVersion = resolveCatalogVersion catalogs depName depVersion
               in for_ (toResolvedDependency toEnv pkgs mkPkgKey depName resolvedVersion) $ \dep -> do
                    direct dep
                    case labelingMode of
                      LabelingOn ->
                        for_ (maybe (Set.singleton EnvProduction) (Map.findWithDefault mempty importerKey) selection) $ \env ->
                          label dep (PnpmEnv env)
                      LabelingOff -> pure ()

            for_ (Map.toList $ directDevDependencies projectImporters) $ \(depName, ProjectMapDepMetadata depVersion) ->
              let resolvedVersion = resolveCatalogVersion catalogs depName depVersion
               in for_ (toResolvedDependency toEnv pkgs mkPkgKey depName resolvedVersion) $ \dep -> do
                    direct dep
                    case labelingMode of
                      LabelingOn -> label dep (PnpmEnv EnvDevelopment)
                      LabelingOff -> pure ()

            -- Config dependencies are real packages installed for the project.
            -- Marking them direct also keeps them in the graph: unreachable deep
            -- nodes are pruned before the source unit is built. (The env
            -- document's packageManagerDependencies — pnpm itself — is not
            -- parsed at all, so it is never reported.)
            for_ (Map.toList (configDependencies projectImporters)) $ \(depName, ProjectMapDepMetadata depVersion) ->
              let resolvedVersion = resolveCatalogVersion catalogs depName depVersion
               in for_ (toResolvedDependency toEnv pkgs mkPkgKey depName resolvedVersion) $ \dep -> do
                    direct dep
                    case labelingMode of
                      LabelingOn -> label dep (PnpmEnv EnvProduction)
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
            let parentDep = (pkgKey, toDependency toEnv depName depVersion pkgMeta)

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

-- | Environments in which each workspace importer is reached. Selected
-- importers start in production; a dev link makes everything beyond it dev.
type ImporterEnvironments = Map Text (Set.Set DepEnvironment)

-- | Nothing represents selection of the entire workspace. Only the original
-- selection can trigger that shortcut: reaching every importer via a dev link
-- must not erase the environments carried by that link.
scopedImporters :: Maybe (Set.Set Text) -> PnpmLockfileBase -> Maybe ImporterEnvironments
scopedImporters Nothing _ = Nothing
scopedImporters (Just keys) base =
  if selected == allImporters then Nothing else Just $ expandWorkspaceLinks base selected
  where
    allImporters = Map.keysSet (lockfileImporters base)
    selected = keys `Set.intersection` allImporters

-- | Follow workspace links with their environments. Track (importer, environment)
-- pairs so cycles terminate while a second production/development path still
-- propagates to the importer's descendants.
expandWorkspaceLinks :: PnpmLockfileBase -> Set.Set Text -> ImporterEnvironments
expandWorkspaceLinks base keys =
  Map.fromListWith (<>) [(key, Set.singleton env) | (key, env) <- Set.toList reached]
  where
    importers = lockfileImporters base
    reached = go Set.empty [(key, EnvProduction) | key <- Set.toList keys]

    go seen [] = seen
    go seen (item@(key, env) : rest)
      | item `Set.member` seen = go seen rest
      | otherwise = go (Set.insert item seen) (linkedFrom key env <> rest)

    linkedFrom key env = case Map.lookup key importers of
      Nothing -> []
      Just projectMap ->
        mapMaybe (linkTarget key env . version) (Map.elems (directDependencies projectMap) <> Map.elems (directOptionalDependencies projectMap))
          <> mapMaybe (linkTarget key EnvDevelopment . version) (Map.elems (directDevDependencies projectMap))

    linkTarget fromKey env ver = do
      relPath <- Text.stripPrefix "link:" ver
      let key = resolveImporterKey fromKey relPath
      if key `Map.member` importers then Just (key, env) else Nothing

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

    -- The accumulator is in reverse order. Preserve excess parent segments
    -- so paths outside the workspace cannot alias an internal importer.
    step :: [Text] -> Text -> [Text]
    step [] ".." = [".."]
    step acc@(".." : _) ".." = ".." : acc
    step (_ : rest) ".." = rest
    step acc "." = acc
    step acc segment = segment : acc

    toKey :: [Text] -> Text
    toKey [] = "."
    toKey acc = Text.intercalate "/" (reverse acc)

--
-- Top-level dispatch
--

-- | Union of each document's graph ('Graphing' is a 'Semigroup'). The first
-- argument scopes each document's graph to a set of workspace importer keys;
-- see 'scopedImporters'.
buildGraph :: Maybe (Set.Set Text) -> NonEmpty PnpmLockfile -> Graphing Dependency
buildGraph selection = sconcat . fmap (buildGraphOne selection)

buildGraphOne :: Maybe (Set.Set Text) -> PnpmLockfile -> Graphing Dependency
buildGraphOne selection lockfile = case lockfile of
  LockfileV4Or5 (PnpmLockfileV4Or5 base) -> withSelection buildGraphConfigV4or5 base
  LockfileV678 (PnpmLockfileV678 base) -> withSelection buildGraphConfigV678 base
  LockfileV9 v -> withSelection (buildGraphConfigV9 v) (lockfileBase v)
  where
    withSelection config base = buildGraphCore (scopedImporters selection base) config base

-- | Parse every document of a pnpm-lock.yaml stream. pnpm writes one or two
-- documents — an env lockfile (configDependencies) followed by the project
-- lockfile — and each is a valid 'PnpmLockfile' on its own
-- (https://pnpm.io/lockfile), so they are returned for per-document processing
-- rather than merged into one object.
parsePnpmLockfile :: ByteString -> Either Text (NonEmpty PnpmLockfile)
parsePnpmLockfile contents = case decodeAllEither' contents of
  Left err -> Left . toText $ prettyPrintParseException err
  Right (docs :: [Value]) -> case traverse (parseEither parseJSON) docs of
    Left err -> Left . toText $ "malformed lockfile document: " <> err
    Right [] -> Left "no YAML documents found"
    Right [one] -> Right (one :| [])
    Right [one, two] ->
      -- Both documents of one stream declare the same lockfileVersion
      -- (https://pnpm.io/lockfile); disagreement is not pnpm output.
      if lockfileVersion one == lockfileVersion two
        then Right (one :| [two])
        else Left "multi-document lockfile with inconsistent lockfileVersions across documents"
    Right _ ->
      -- pnpm writes one or two documents — an env lockfile and a project
      -- lockfile (https://pnpm.io/lockfile) — so more is not pnpm output.
      Left "pnpm-lock.yaml contains more than two YAML documents; pnpm writes at most two, an env lockfile and a project lockfile (see https://pnpm.io/lockfile)"

-- Both documents of one stream declare the same lockfileVersion
-- (https://pnpm.io/lockfile); disagreement is not pnpm output.
lockfileVersion :: PnpmLockfile -> Text
lockfileVersion (LockfileV4Or5 (PnpmLockfileV4Or5 b)) = lockfileRawVersion b
lockfileVersion (LockfileV678 (PnpmLockfileV678 b)) = lockfileRawVersion b
lockfileVersion (LockfileV9 v) = lockfileRawVersion (lockfileBase v)

-- | Analyze a pnpm lockfile, optionally scoped to the given workspace importer
-- keys (@"."@ for the root, @"packages/a"@ for a member), as resolved from the
-- selected build targets by 'Strategy.Node.resolvePnpmImporterKeys'. 'Nothing'
-- means no target filter is applied, so the whole workspace is graphed.
analyze :: (Has ReadFS sig m, Has Logger sig m, Has Diagnostics sig m) => Maybe (Set.Set Text) -> Path Abs File -> m (Graphing Dependency)
analyze selectedImporters file = context "Analyzing Pnpm Lockfile" $ do
  lockfiles <- context "Parsing pnpm-lock file" $
    context ("Parsing YAML file '" <> toText (toString file) <> "'") $ do
      contents <- readContentsBS file
      case parsePnpmLockfile contents of
        Left err -> errSupport (fileParseErrorSupportMsg file) . fatal $ FileParseError (toString file) err
        Right lockfiles -> pure lockfiles

  -- Warn about unsupported versions (v1-v3).
  -- parsePnpmLockfile rejects streams whose documents disagree on
  -- lockfileVersion, so checking the first document suffices.
  let (lockfile :| _) = lockfiles
  case lockfile of
    LockfileV4Or5 (PnpmLockfileV4Or5 base) ->
      case Text.uncons (lockfileRawVersion base) of
        Just (c, _)
          | c `elem` ['1', '2', '3'] ->
              logWarn . pretty $ "pnpm-lock file is using older lockFileVersion: " <> lockfileRawVersion base <> ", which is not officially supported!"
        _ -> pure ()
    LockfileV678 _ -> pure ()
    LockfileV9 _ -> pure ()

  -- The project document is last, and its importers are the workspace members
  -- the selection keys refer to.
  let scoped = scopedImporters selectedImporters (lockfileBaseOf (Data.List.NonEmpty.last lockfiles))
  case (selectedImporters, scoped) of
    (Just keys, Just selected) ->
      when (Map.null selected) . logWarn . pretty $
        "Target filter (resolved importer keys: "
          <> Text.intercalate ", " (Set.toList keys)
          <> ") did not match any importer in the pnpm lockfile; reporting an empty dependency graph."
    _ -> pure ()

  context "Building dependency graph" $ pure $ buildGraph selectedImporters lockfiles
