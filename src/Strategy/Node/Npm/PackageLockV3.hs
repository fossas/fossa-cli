module Strategy.Node.Npm.PackageLockV3 (
  PackageLockV3 (..),
  analyze,

  -- * for testing,
  PackagePath (..),
  PackageName (..),
  PackageLockV3Package (..),
  NpmLockV3Resolved (..),
  buildGraph,
  isV3Compatible,
  parsePathKey,
  vendorPrefixes,
) where

import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, context)
import Control.Monad (unless, when)
import Data.Aeson (
  FromJSON (parseJSON),
  FromJSONKey (fromJSONKey),
  FromJSONKeyFunction (FromJSONKeyTextParser),
  Value (Bool, String),
  withObject,
  withText,
  (.!=),
  (.:),
  (.:?),
 )
import Data.Foldable (find, for_, traverse_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, breakOnEnd)
import Data.Text qualified as Text
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (NodeJSType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.Grapher (direct, edge, evalGrapher, run)
import Effect.Logger (Logger, logWarn, pretty)
import Effect.ReadFS (ReadFS, readContentsJson)
import Graphing (Graphing)
import Graphing qualified
import Path (
  Abs,
  File,
  Path,
 )

data PackageLockV3 = PackageLockV3
  { rootPackage :: PackageLockV3Package
  , packages :: Map PackagePath PackageLockV3Package
  , lockFileVersion :: Maybe Int
  }
  deriving (Show, Eq, Ord)

instance FromJSON PackageLockV3 where
  parseJSON = withObject "PackageLockV3" $ \obj -> do
    packages <- obj .: "packages"
    lockFileVersion <- obj .:? "lockfileVersion"

    -- Ensure it is v3 compatible file
    -- v2 file by default are v3 compatible
    -- Refer to: https://docs.npmjs.com/cli/v8/configuring-npm/package-lock-json#lockfileversion
    unless (maybe False isV3Compatible lockFileVersion) $
      fail "Provided lockfile is not v3 compatible!"

    -- If root package is missing, it is likely
    -- corrupted package-lock file, fail early.
    case Map.lookup PackageLockV3Root packages of
      Nothing -> fail "Could not find root node in package-lock.json! Is this faulty package-lock.json file?"
      Just rootPackage -> pure $ PackageLockV3 rootPackage packages lockFileVersion

-- | Returns True when provided LockfileVersion is v3 compatible, otherwise returns False.
-- Although v2 lockfile is forward compatible
-- We intentionally mark them incompatible for now, give large blast zone.
-- TODO: make v2 lockfile also compatible with v3 after this analysis has been used in prod for some time.
isV3Compatible :: Int -> Bool
isV3Compatible = (3 ==)

-- | Primary identifier in package-lock.json v3 `packages`.
--
-- In package-lock v3 file,  keys in the `packages` object refer to paths in the filesystem that
-- contain a package.json file. These locations will be any of a workspace, a dependency, or
-- the root project we are analyzing.
--
-- * @PackageLockV3Root@ refers to parent package.json file.
--
--   project/
--    + package.json <- PackageLockV3Root
--
-- * @PackageLockV3WorkSpace Text@ refers to workspace path of package.json, for example:
--
--   project/
--    + package.json
--    + workspace-a/
--    ++ package.json <- PackageLockV3WorkSpace "workspace-a"
--
-- * @PackageLockV3PathKey Text PackageName@ refers to dependency's package.json:
--
--   project/
--    + package.json
--    + node_modules/
--    ++ a/
--    +++ package.json <- PackageLockV3PathKey "node_modules" "a"
--    + workspace-a/
--    ++ package.json
data PackagePath
  = PackageLockV3WorkSpace Text
  | PackageLockV3Root
  | PackageLockV3PathKey Text PackageName
  deriving (Show, Eq, Ord)

instance FromJSONKey PackagePath where
  fromJSONKey = FromJSONKeyTextParser $ pure . parsePathKey

instance FromJSON PackagePath where
  parseJSON = withText "PackageLockV3PackageKey" $ pure . parsePathKey

-- It is not possible to modify the default vendor dir in npm.
defaultVendorDir :: Text
defaultVendorDir = "node_modules/"

-- | Parse a path key to @PackagePath@.
parsePathKey :: Text -> PackagePath
parsePathKey t = case (breakOnEnd defaultVendorDir t) of
  ("", "") -> PackageLockV3Root
  ("", workspace) -> PackageLockV3WorkSpace workspace
  (prefix, moduleName) -> PackageLockV3PathKey prefix (PackageName moduleName)

newtype PackageName = PackageName {unPackageName :: Text} deriving (Show, Eq, Ord, FromJSONKey)
instance FromJSON PackageName where
  parseJSON = withText "PackageName" $ \pkgName -> pure $ PackageName pkgName

-- | Describes package and it's dependencies.
data PackageLockV3Package = PackageLockV3Package
  { plV3PkgVersion :: Maybe Text
  , plV3PkgResolved :: NpmLockV3Resolved
  , plV3PkgDependencies :: Map PackageName Text
  , plV3PkgDevDependencies :: Map PackageName Text
  , plV3PkgOptionalDependencies :: Map PackageName Text
  , plV3PkgPeerDependencies :: Map PackageName Text
  , plV3PkgDev :: Bool
  , plV3PkgLink :: Bool
  }
  deriving (Show, Eq, Ord)

instance FromJSON PackageLockV3Package where
  parseJSON = withObject "PackageLockV3Package" $ \obj ->
    PackageLockV3Package
      <$> obj .:? "version"
      <*> obj .:? "resolved" .!= (NpmLockV3Resolved Nothing)
      <*> obj .:? "dependencies" .!= mempty
      <*> obj .:? "devDependencies" .!= mempty
      <*> obj .:? "optionalDependencies" .!= mempty
      <*> obj .:? "peerDependencies" .!= mempty
      <*> obj .:? "dev" .!= False -- if 'dev' key is not included, it is presumed to be prod dependency
      <*> obj .:? "link" .!= False -- 'link' is only present (and true) for workspace link entries

newtype NpmLockV3Resolved = NpmLockV3Resolved {unNpmLockV3Resolved :: Maybe Text}
  deriving (Eq, Ord, Show)

instance FromJSON NpmLockV3Resolved where
  parseJSON (String t) = pure $ NpmLockV3Resolved (Just t)
  parseJSON (Bool _) = pure $ NpmLockV3Resolved Nothing
  parseJSON _ = fail "Expected to contain string or boolean value for 'resolved' field!"

-- | Analyzes a v3 lockfile, optionally scoped to the given root-relative
-- workspace paths (@""@ for the root project, @"packages/a"@ for a workspace
-- member), as resolved from the selected build targets by
-- 'Strategy.Node.resolveNpmV3WorkspacePaths'. @Nothing@ means no target
-- filter is applied, so the whole unscoped graph is produced.
analyze :: (Has ReadFS sig m, Has Diagnostics sig m, Has Logger sig m) => Maybe (Set Text) -> Path Abs File -> m (Graphing Dependency)
analyze selectedPaths file = context "Analyzing Npm Lockfile (v3)" $ do
  packageLockJson <- context "Parsing v3 compatible package-lock.json" $ readContentsJson @PackageLockV3 file
  case (selectedPaths, scopedSelection selectedPaths packageLockJson) of
    (Just paths, Just selected) ->
      when (Set.null selected) $
        logWarn . pretty $
          "Target filter (resolved workspace paths: "
            <> Text.intercalate ", " (Set.toList paths)
            <> ") did not match any root or workspace entry in the v3 lockfile; reporting an empty dependency graph."
    _ -> pure ()
  context "Building dependency graph" $ pure $ buildGraph selectedPaths packageLockJson

-- | Root/workspace entries selected for analysis, shared by 'analyze' and
-- 'buildGraph'. The given root-relative workspace paths are parsed into the
-- lockfile's top-level path keys ('PackageLockV3Root' / 'PackageLockV3WorkSpace')
-- and intersected with the entries actually present in the lockfile.
-- @Nothing@ means the analysis is unscoped (all entries are selected), which
-- must preserve pre-scoping output exactly: this happens when no target
-- filter is applied, or when the selection covers every root/workspace entry
-- (the default when all targets are selected). A selection matching no entry
-- yields @Just Set.empty@: no entry marks its dependencies as direct, so
-- pruning produces an empty graph ('analyze' warns when this happens).
scopedSelection :: Maybe (Set Text) -> PackageLockV3 -> Maybe (Set PackagePath)
scopedSelection Nothing _ = Nothing
scopedSelection (Just paths) pkgLockV3 =
  if selected == topLevelEntries
    then Nothing
    else Just selected
  where
    topLevelEntries :: Set PackagePath
    topLevelEntries = Set.filter isTopLevelEntry (Map.keysSet (packages pkgLockV3))

    selected :: Set PackagePath
    selected = Set.map parsePathKey paths `Set.intersection` topLevelEntries

    isTopLevelEntry :: PackagePath -> Bool
    isTopLevelEntry = \case
      PackageLockV3Root -> True
      PackageLockV3WorkSpace _ -> True
      PackageLockV3PathKey _ _ -> False

-- | Builds a graph for package lock v3.
--
-- In summary, when resolving a dependency, npm checks if the version spec of
-- dependency can be met by storing a dependency at top level, or checking if
-- existing dependency (at top level) is compatible with the spec. If it finds,
-- that version spec, cannot be met it vendors this dependency from it's own node_modules
-- directory.
--
--  Consider,
--
--  A --> B@^1.0.0
--  C --> B@^1.0.0
--  D --> B@^2.0.0
--
--  Would yield following file tree,
--
--  package.json
--  package-lock.json
--  node_modules/
--    A/
--    B/ (at 1.0.0) (refer to as top level path)
--    C/
--    D/
--      node_modules/
--        B@2.0.0 (refer to as vendored path)
--
--  And PackageLockV3, packages key reflect filepath of nodule.
--
--  So to make a resolution, we work backwards to top level from
--  a given module,
--
--  For example, to resolve B as a dependency of D in the above example, we first search for a
--  package like "node_modules/A/node_modules/D". If it doesn't exist, we then search for
--  "node_modules/A".
--
--  Although, this is not precisely what npm does when loading
--  package-lock.json, this is good approximation without having
--  to port npm/cli/arborist/virtualTree.
--
--  Since workspaces are considered to a it's own "root", we also
--  check if package's dependency exists at node_modules of workspace.
--  in resolution process.
--
--  References:
--    - https://github.com/npm/arborist/blob/main/lib/arborist/load-virtual.js
--    - https://docs.npmjs.com/cli/v8/using-npm/workspaces
--    - http://npm.github.io/how-npm-works-docs/npm3/how-npm3-works.html
--
--  Target scoping:
--
--  When a proper subset of the project's build targets is selected (via
--  --only-target / --exclude-target), only the selected root/workspace
--  entries mark their dependencies as direct, and the graph is pruned to
--  what is reachable from those directs. Selected build target names are
--  resolved to root-relative workspace paths via their package.json
--  manifests (see 'Strategy.Node.resolveNpmV3WorkspacePaths'), and those
--  paths are matched against the lockfile's top-level path keys, so
--  selection works even when a lockfile entry omits its "name" field. Only
--  when all targets are selected (the default) or the project has no
--  targets is the whole unscoped graph produced, preserving pre-scoping
--  output exactly. A selection matching no entry produces an empty graph
--  ('analyze' warns when this happens, honoring the user's filter as v1/v2
--  scoping does).
--
--  --
buildGraph :: Maybe (Set Text) -> PackageLockV3 -> Graphing Dependency
buildGraph selectedPaths pkgLockV3 = pruneIfScoped . run . evalGrapher $ do
  for_ (Map.toList $ packages pkgLockV3) $ \(pkgPathKey, pkgMetadata) -> do
    case pkgPathKey of
      -- For root package,
      --
      -- "": {
      --   "name": "project-name",
      --   "version": "1.0.0",
      --   "dependencies": {
      --     "someDirectDep": "^1.0.0",
      --   }
      --  },
      --
      -- Now this, @someDirectDep, will be resolved at:
      --    - top path: node_modules/someDirectDep
      --
      -- "node_modules/someDirectDep": {
      --       "version": "1.2.3",
      --       "resolved": "https://registry.npmjs.org/someDirectDep/-/someDirectDep-1.2.3.tgz",
      --       "dependencies": {
      --         "someTransitiveDepB": "^1.0.0"
      --       }
      --     },
      --
      -- We need to resolve dependency from a top path, since we
      -- want to identify precise version (e.g. 1.2.3) used by project as opposed
      -- to version spec (e.g. ^1.0.0) provided in 'dependencies' field.
      --
      -- Root's direct dependencies are always resolved at top-path.
      PackageLockV3Root ->
        when (isSelected PackageLockV3Root) $ do
          let directDeps :: [PackagePath]
              directDeps =
                map toTopLevelPackage $
                  concatMap
                    Map.keys
                    [ plV3PkgDependencies pkgMetadata
                    , plV3PkgDevDependencies pkgMetadata
                    , plV3PkgPeerDependencies pkgMetadata
                    , plV3PkgOptionalDependencies pkgMetadata
                    ]

          let resolvedDeps :: [Dependency]
              resolvedDeps = resolveDirectDeps PackageLockV3Root directDeps

          traverse_ direct resolvedDeps

      -- For workspace packages, for instance:
      --
      -- "workspace-a": {
      --   "name": "someWorkspacePkgName",
      --   "version": "1.0.0",
      --   "dependencies": {
      --     "someWorkspaceDirectDep": "^1.0.0",
      --   }
      --  },
      --
      -- Now this, @someWorkspaceDirectDep, might be resolved at:
      --    - top path: node_modules/someWorkspaceDirectDep
      --    - vendor path: workspace-a/node_modules/someWorkspaceDirectDep
      --
      -- If the @someWorkspaceDirectDep was resolved with top path,
      -- vendored path (within workspace-a) will not exist. For this reason,
      -- always prefer vendored path, if such path does not exist fallback to
      -- top level path.
      --
      -- Direct Dependencies within workspace pkg are treated as direct.
      -- Direct Peer Dependencies within workspace pkg are treated as direct.
      -- Development Dependencies within workspace pkg are treated as direct development.
      PackageLockV3WorkSpace workspaceRootPath ->
        when (isSelected (PackageLockV3WorkSpace workspaceRootPath)) $ do
          let directDeps :: [PackagePath]
              directDeps =
                map (vendoredPathElseTopLevelPath workspaceRootPath) $
                  concatMap
                    Map.keys
                    [ plV3PkgDependencies pkgMetadata
                    , plV3PkgDevDependencies pkgMetadata
                    , plV3PkgPeerDependencies pkgMetadata
                    , plV3PkgOptionalDependencies pkgMetadata
                    ]

          let resolvedDeps :: [Dependency]
              resolvedDeps = resolveDirectDeps (PackageLockV3WorkSpace workspaceRootPath) directDeps

          traverse_ direct resolvedDeps

      -- For typical dependency packages, for instance:
      --
      -- "node_modules/someDirectDep": {
      --   "version": "1.2.3",
      --   "resolved": "https://registry.npmjs.org/someDirectDep/-/someDirectDep-1.2.3.tgz",
      --   "dependencies": {
      --     "someTransitiveDepB": "^1.0.0",
      --   }
      --  },
      --
      -- Now this, @someTransitiveDepB, might be resolved at:
      --    - top path: node_modules/someTransitiveDepB
      --    - vendor path: node_modules/a/node_modules/someTransitiveDepB
      --
      -- If the @someTransitiveDepB was resolved with top path,
      -- vendored path (within node_modules/a/) will not exist. For this reason,
      -- always prefer vendored path, if such path does not exist fallback to
      -- top level path.
      --
      -- Only 'dependencies' field is used to infer transitive dependencies.
      --
      -- For each transitive dependency, Adds edge between parent package, and transitive dep.
      PackageLockV3PathKey prefixPath depPackageName -> do
        let currentDep :: Maybe Dependency
            currentDep = toDependency (PackageLockV3PathKey prefixPath depPackageName)

        let vendorPathPrefix :: Text
            vendorPathPrefix = prefixPath <> (unPackageName depPackageName)

        let deepDeps :: [PackagePath]
            deepDeps =
              map (vendoredPathElseTopLevelPath vendorPathPrefix) $
                concatMap
                  Map.keys
                  [ plV3PkgDependencies pkgMetadata
                  , plV3PkgPeerDependencies pkgMetadata
                  , plV3PkgOptionalDependencies pkgMetadata
                  ]

        -- When scoping, drop edges to workspace link stubs: they have no
        -- version, and the linked workspace's deps are attributed via
        -- 'resolveDirectDeps' when that workspace (or a consumer) is selected.
        let resolvedDeepDeps :: [Dependency]
            resolvedDeepDeps =
              mapMaybe toDependency $
                if isScoped
                  then filter (not . isLinkEntry) deepDeps
                  else deepDeps

        case currentDep of
          Nothing -> pure () -- This will never happen, given that we are iterating on same package path.
          Just parentDep -> do
            for_ resolvedDeepDeps $ edge parentDep
  where
    selection :: Maybe (Set PackagePath)
    selection = scopedSelection selectedPaths pkgLockV3

    isScoped :: Bool
    isScoped = isJust selection

    isSelected :: PackagePath -> Bool
    isSelected pkgPath = maybe True (Set.member pkgPath) selection

    pruneIfScoped :: Graphing Dependency -> Graphing Dependency
    pruneIfScoped = if isScoped then Graphing.pruneUnreachable else id

    -- Converts the resolved direct dependency paths of a root/workspace entry
    -- to dependencies. When scoping, workspace link stubs are expanded into
    -- the linked workspace's own dependencies (see 'expandLinks'); unscoped
    -- analysis reports them as-is to preserve pre-scoping output.
    resolveDirectDeps :: PackagePath -> [PackagePath] -> [Dependency]
    resolveDirectDeps selfPath depPaths
      | isScoped =
          concatMap
            (mapMaybe toDependency . expandLinks (Set.singleton selfPath))
            depPaths
      | otherwise = mapMaybe toDependency depPaths

    -- Expands a workspace link stub (e.g. "node_modules/some-ws-name" with
    -- "link": true and "resolved" pointing at the workspace's path) into the
    -- linked workspace's own resolved prod/peer/optional dependencies, so a
    -- selected workspace's dependency on a sibling workspace is attributed
    -- to the sibling's real dependencies instead of a versionless link stub.
    -- The visited set guards against mutually-dependent workspaces.
    expandLinks :: Set PackagePath -> PackagePath -> [PackagePath]
    expandLinks visited pkgPath = case linkTarget pkgPath of
      Nothing -> [pkgPath]
      Just linkedPath
        | linkedPath `Set.member` visited -> []
        | otherwise -> case (linkedPath, Map.lookup linkedPath (packages pkgLockV3)) of
            (PackageLockV3WorkSpace workspacePath, Just workspaceMeta) ->
              concatMap (expandLinks (Set.insert linkedPath visited) . vendoredPathElseTopLevelPath workspacePath) $
                concatMap
                  Map.keys
                  [ plV3PkgDependencies workspaceMeta
                  , plV3PkgPeerDependencies workspaceMeta
                  , plV3PkgOptionalDependencies workspaceMeta
                  ]
            _ -> []

    -- Resolves a link stub to the entry it points at.
    linkTarget :: PackagePath -> Maybe PackagePath
    linkTarget pkgPath = case Map.lookup pkgPath (packages pkgLockV3) of
      Just meta | plV3PkgLink meta -> parsePathKey <$> unNpmLockV3Resolved (plV3PkgResolved meta)
      _ -> Nothing

    isLinkEntry :: PackagePath -> Bool
    isLinkEntry pkgPath = maybe False plV3PkgLink $ Map.lookup pkgPath $ packages pkgLockV3

    -- Prefer resolution path in following order of precedent:
    --
    --  1) {prefix}/node_modules/{pkgName}
    --  2) {root of prefix}/node_modules/{pkgName} (handles workspace root projects)
    --  3) {parent root of prefix}/node_modules/{pkgName}
    --  4) node_modules/{pkgName}
    --
    -- Example, for prefix of "workspace-a/node_modules/a/" for 'PackageName b' we will attempt,
    --  1) workspace-a/node_modules/a/node_modules/b/
    --  2) workspace-a/node_modules/b/
    --  3) node_modules/b/ (same as step 5 from above)
    vendoredPathElseTopLevelPath :: Text -> PackageName -> PackagePath
    vendoredPathElseTopLevelPath prefix pkgName =
      case find (`Map.member` allDependencyPackages) $ possiblePkgPaths prefix pkgName of
        Just foundPath -> foundPath
        Nothing -> toTopLevelPackage pkgName

    possiblePkgPaths :: Text -> PackageName -> [PackagePath]
    possiblePkgPaths prefix pkgName =
      map (`PackageLockV3PathKey` pkgName) $
        vendorPrefixes prefix (getRootWorkspace prefix)

    toDependency :: PackagePath -> Maybe Dependency
    toDependency pkgPath =
      case (pkgPath, Map.lookup pkgPath $ packages pkgLockV3) of
        (PackageLockV3PathKey _ packageName, Just meta) ->
          Just $
            Dependency
              { dependencyType = NodeJSType
              , dependencyName = unPackageName packageName
              , dependencyVersion = CEq <$> (plV3PkgVersion meta)
              , dependencyLocations = catMaybes [unNpmLockV3Resolved $ plV3PkgResolved meta]
              , dependencyEnvironments =
                  if plV3PkgDev meta
                    then Set.singleton EnvDevelopment
                    else Set.singleton EnvProduction
              , dependencyTags = Map.empty
              }
        _ -> Nothing -- don't report workspace or root project as dependency, or pkg not found
    allDependencyPackages :: Map PackagePath PackageLockV3Package
    allDependencyPackages =
      Map.filterWithKey
        (\k _ -> isReportableDependencyModule k)
        (packages pkgLockV3)

    -- True if package path leads to dependency, Otherwise false.
    isReportableDependencyModule :: PackagePath -> Bool
    isReportableDependencyModule pkgPath = case pkgPath of
      PackageLockV3PathKey _ _ -> True
      _ -> False

    -- Provides candidate path in top level tree
    -- >> toTopLevelPackage pkgName = PackagePath "node_modules/" pkgName
    toTopLevelPackage :: PackageName -> PackagePath
    toTopLevelPackage = PackageLockV3PathKey defaultVendorDir

    -- Retrieves root workspace module.
    --
    -- >> getRootWorkspace "myWorkspace/myPkg/node_modules/a" = Just "myWorkspace/myPkg"
    -- >> getRootWorkspace "myWorkspace/myPkg/node_modules/a/node_modules/b" = Just "myWorkspace/myPkg"
    -- >> getRootWorkspace "node_modules/a" = Nothing
    -- >> getRootWorkspace "node_modules/a/node_module/b" = Nothing
    -- >> getRootWorkspace "myWorkspace/myPkg" = Just "myWorkspace/myPkg"
    getRootWorkspace :: Text -> Maybe Text
    getRootWorkspace pkgPath = case Text.breakOn "/node_modules" pkgPath of
      ("", "") -> Nothing
      (firstModule, _)
        | (Map.size (Map.filterWithKey (\k _ -> isWorkSpace firstModule k) (packages pkgLockV3)) > 0) -> Just firstModule
        | otherwise -> Nothing

    isWorkSpace :: Text -> PackagePath -> Bool
    isWorkSpace name pkgPath = pkgPath == PackageLockV3WorkSpace name

-- | Creates vendor prefixes, in order of precedent in lockfile.
--
-- >> vendorPrefixes "node_modules/a/node_modules/b" Nothing =
--      [ "node_modules/a/node_modules/b/node_modules/"
--      , "node_modules/a/node_modules/"
--      , "node_modules/"
--      ]
--
-- >> vendorPrefixes "ws/myPkg/node_modules/a/node_modules/b" (Just "ws/myPkg") =
--      [ "ws/myPkg/node_modules/a/node_modules/b/node_modules/"
--      , "ws/myPkg/node_modules/a/node_modules/"
--      , "ws/myPkg/node_modules/"
--      ]
vendorPrefixes :: Text -> Maybe Text -> [Text]
vendorPrefixes path ws = prefixWithWS . reverse $ possiblePrefixes
  where
    -- ["node_modules/", "node_modules/a/node_modules/", "node_modules/a/node_modules/b/node_modules/"]
    possiblePrefixes :: [Text]
    possiblePrefixes =
      map (defaultVendorDir <>) $
        scanl1 (\prev curr -> prev <> curr <> "/" <> defaultVendorDir) withoutNodeModules

    -- ["", "a", "b"]
    withoutNodeModules :: [Text]
    withoutNodeModules = map (Text.dropAround (== '/')) $ Text.splitOn defaultVendorDir prefixWithoutWS

    prefixWithoutWS :: Text
    prefixWithoutWS = case ws of
      Nothing -> path
      Just ws' -> Text.replace ws' "" path

    prefixWithWS :: [Text] -> [Text]
    prefixWithWS result = case ws of
      Nothing -> result
      Just ws' -> map (\r -> ws' <> "/" <> r) result
