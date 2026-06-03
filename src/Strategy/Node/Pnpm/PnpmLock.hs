module Strategy.Node.Pnpm.PnpmLock (
  analyze,

  -- * for testing
  buildGraph,
) where

import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, Has, context)
import Data.Maybe (fromMaybe)
import Data.Foldable (for_)
import Data.HashMap.Strict qualified as HashMap
import Data.Map (Map, toList)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
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

-- | Core graph-building logic shared across all lockfile versions.
buildGraphCore :: BuildGraphConfig -> PnpmLockfileBase -> Graphing Dependency
buildGraphCore BuildGraphConfig{bgcGetPkgNameVersion, bgcMkPkgKey, bgcToEnv, bgcLabelingMode, bgcSnapshotEdges, bgcCatalogs} base =
  let getPkgNameVersion = bgcGetPkgNameVersion
      mkPkgKey = bgcMkPkgKey
      toEnv = bgcToEnv
      labelingMode = bgcLabelingMode
      snapshotEdges = bgcSnapshotEdges
      catalogs = bgcCatalogs
      pkgs = lockfilePackages base
      snapshotEdgesHM = HashMap.fromList snapshotEdges
  in withoutLocalPackages . hydrateDepEnvs $
    run . withLabeling applyLabels $ do
      -- Direct dependencies from each importer (workspace package).
      for_ (toList (lockfileImporters base)) $ \(_, projectImporters) -> do
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
-- Top-level dispatch
--

-- | Build the dependency graph, labeling direct deps with their environment
-- (prod\/dev). hydrateDepEnvs then propagates those environments to all
-- transitive successors.
buildGraph :: PnpmLockfile -> Graphing Dependency
buildGraph (LockfileV4Or5 (PnpmLockfileV4Or5 base)) = buildGraphCore buildGraphConfigV4or5 base
buildGraph (LockfileV678 (PnpmLockfileV678 base)) = buildGraphCore buildGraphConfigV678 base
buildGraph (LockfileV9 v) = buildGraphCore (buildGraphConfigV9 v) (lockfileBase v)

analyze :: (Has ReadFS sig m, Has Logger sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze file = context "Analyzing Pnpm Lockfile" $ do
  pnpmLockFile <- context "Parsing pnpm-lock file" $ readContentsYaml file

  -- Warn about unsupported versions (v1-v3).
  case pnpmLockFile of
    LockfileV4Or5 (PnpmLockfileV4Or5 base) ->
      case Text.uncons (lockfileRawVersion base) of
        Just (c, _) | c `elem` ['1', '2', '3'] ->
          logWarn . pretty $ "pnpm-lock file is using older lockFileVersion: " <> lockfileRawVersion base <> ", which is not officially supported!"
        _ -> pure ()
    LockfileV678 _ -> pure ()
    LockfileV9 _ -> pure ()

  context "Building dependency graph" $ pure $ buildGraph pnpmLockFile
