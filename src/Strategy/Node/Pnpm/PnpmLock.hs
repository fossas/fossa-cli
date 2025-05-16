module Strategy.Node.Pnpm.PnpmLock (
  -- | Analyzes a pnpm lockfile and returns a graph of dependencies.
  analyze,

  -- * for testing

  -- | Builds a graph of dependencies from a pnpm lockfile.
  dispatchPnpmGraphBuilder,
  buildGraph,
  PnpmLockfile,
) where

import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, Has, context)
import Control.Monad (when)
import Data.Aeson.Extra (TextLike (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Foldable (for_)
import Data.Functor.Identity (runIdentity)
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set qualified as Set
import Data.String.Conversion (toString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read (decimal)
import Data.Yaml (FromJSON, Object, Parser, (.!=), (.:), (.:?))
import Data.Yaml qualified as Yaml
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (GitType, NodeJSType, URLType, UserType),
  Dependency (Dependency, dependencyEnvironments, dependencyType),
  VerConstraint (CEq),
 )
import Effect.Grapher (deep, direct, edge, evalGrapher, GrapherC)
import Effect.Logger (
  Logger,
  logWarn,
  pretty,
 )
import Effect.ReadFS (ReadFS, readContentsYaml)
import Graphing (Graphing, shrink)
import Path (Abs, File, Path)

withoutLocalPackages :: Graphing Dependency -> Graphing Dependency
withoutLocalPackages = shrink (\dep -> dependencyType dep /= UserType)

-- Sometimes package versions include resolved peer dependency version
-- in parentheses. This is used by pnpm for dependency resolution, we do
-- not care about them, as they do not represent package version.
--
-- >> withoutPeerDepSuffix "1.2.0" = "1.2.0"
-- >> withoutPeerDepSuffix "1.2.0(babel@1.0.0)" = "1.2.0"
withoutPeerDepSuffix :: Text -> Text
withoutPeerDepSuffix = fst . Text.breakOn "("

-- Sometimes package versions include symlinked paths
-- of sibling dependencies used for resolution.
--
-- >> withoutSymConstraint "1.2.0" = "1.2.0"
-- >> withoutSymConstraint "1.2.0_vue@3.0" = "1.2.0"
withoutSymConstraint :: Text -> Text
withoutSymConstraint version = fst $ Text.breakOn "_" version

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
--  Pnpm lockfile (v9) differs (v6), in following manner:
--  -----------------------------------------------------
--
--    * Introduction of a top-level `snapshots` section. This section becomes the
--      canonical representation of the dependency graph.
--      Keys are often like `/<pkgName>@<version>(<peerDep>@<peerVersion>...)` or
--      `npm:<actualPkgName>@<version>(...)` if an alias was used.
--      Each entry details its own `dependencies`, `optionalDependencies`, etc.,
--      pointing to other keys within `snapshots`.
--      @
--      > snapshots:
--      >   /foo@1.2.3:
--      >     dependencies:
--      >       bar: /bar@2.0.0
--      >   /bar@2.0.0: {}
--      @
--
--    * The `packages` section is refined. While in v6 it held graph-like information,
--      in v9 it primarily stores metadata such as `resolution` (e.g., integrity hashes),
--      the package's declared `dependencies` and `peerDependencies` from its own
--      `package.json`, `name`, `version`, and `dev` status. The graph structure itself
--      is now chiefly in `snapshots`.
--      @
--      > packages:
--      >   /foo@1.2.3:
--      >     resolution: {integrity: sha512-...}
--      >     dependencies: # As declared in foo's package.json
--      >       bar: ^2.0.0
--      >     dev: false
--      @
--
--    * `importers` section structure remains similar to v6, but the `version` field for
--      a dependency can now resolve in more varied ways:
--        - Directly to a version string (e.g., `1.2.3`).
--        - To a key in the `snapshots` section (e.g., `foo@1.2.3` or `/foo@1.2.3`).
--        - To a catalog alias (e.g., `catalog:shared-react`).
--        - To a local workspace link (e.g., `link:../shared-lib`).
--      @
--      > importers:
--      >   .:
--      >     dependencies:
--      >       foo:
--      >         specifier: ^1.0.0
--      >         version: /foo@1.2.3  # Points to a snapshot
--      >       shared-ui:
--      >         specifier: '*'
--      >         version: catalog:common-ui # Points to a catalog entry
--      @
--
--    * Introduction of an optional top-level `catalogs` section. This allows defining
--      named groups of shared dependency versions, useful for monorepos.
--      @
--      > catalogs:
--      >   default:
--      >     common-ui:
--      >       specifier: ^2.0.0
--      >       version: /some-ui-lib@2.1.0
--      @
--
--    * For the most precise and up-to-date details on the lockfile format, especially for v9
--      and later, refer to the TypeScript type definitions within the official PNPM repository.
--      You can explore the repository at <https://github.com/pnpm/pnpm> and search for files
--      and type names related to the lockfile structure (e.g., looking for `Lockfile`,
--      `Importers`, `Snapshots`, `PackagesLog` type definitions).
--
--  References:
--    - [pnpm](https://pnpm.io/)
--    - [pnpm-lockfile](https://github.com/pnpm/pnpm/blob/5cfd6d01946edcce86f62580bddc788d02f93ed6/packages/lockfile-types/src/index.ts) -- (Covers up to v6 style)
--    - [pnpm-lockfile-v6](https://github.com/pnpm/pnpm/pull/5810/files)
data PnpmLockfile = PnpmLockfile
  { importers :: Map Text ProjectMap
  , packages :: Map Text PackageData
  , catalogs :: Map Text CatalogMap
  , snapshots :: Map Text SnapshotPackageData
  , lockFileVersion :: PnpmLockFileVersion
  }
  deriving (Show, Eq, Ord)

data PnpmLockFileVersion
  = PnpmLockLt4 Text
  | PnpmLock4Or5
  | PnpmLock6
  | PnpmLock9
  | PnpmLockGt9 Text
  deriving (Show, Eq, Ord)

instance FromJSON PnpmLockfile where
  parseJSON = Yaml.withObject "pnpm-lock content" $ \obj -> do
    rawLockFileVersion <- getVersion =<< obj .:? "lockfileVersion" .!= (TextLike mempty)
    importers <- obj .:? "importers" .!= mempty
    packages <- obj .:? "packages" .!= mempty
    catalogs <- obj .:? "catalogs" .!= mempty
    snapshots <- obj .:? "snapshots" .!= mempty

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

    pure $ PnpmLockfile refinedImporters packages catalogs snapshots rawLockFileVersion
    where
      getVersion (TextLike verText) =
        -- Attempt to parse the major version number from the start of the string.
        -- Handles versions like "9.0", "10.1", etc.
        case decimal @Integer verText of -- Explicitly use Integer to resolve type defaulting
          Right (majorInt, rest) ->
            -- For the top-level lockfileVersion, we expect `rest` to be empty (e.g., "9")
            -- or start with a dot (e.g., "9.0").
            if Text.null rest || Text.isPrefixOf "." rest
              then case majorInt of
                1 -> pure $ PnpmLockLt4 verText
                2 -> pure $ PnpmLockLt4 verText
                3 -> pure $ PnpmLockLt4 verText
                4 -> pure PnpmLock4Or5
                5 -> pure PnpmLock4Or5
                6 -> pure PnpmLock6
                9 -> pure PnpmLock9
                _
                  | majorInt > 9 -> pure $ PnpmLockGt9 verText
                  -- Versions 7.x and 8.x do not exist in the pnpm spec.
                  -- Other unexpected major versions (e.g., 0) also lead to failure.
                  | otherwise -> fail ("Unsupported or non-existent pnpm lockfile major version: " <> show majorInt <> " from full version string: " <> toString verText)
              else
                -- This case handles `Right (majorInt, rest)` where `rest` is not empty and doesn't start with '.'
                -- e.g., if verText was "9foo" or "9(react@18)".
                fail ("Malformed pnpm lockfileVersion string (unexpected characters after major version): " <> toString verText)
          Left _ ->
            -- Fallback for non-numeric or improperly formatted version strings at the beginning.
            fail ("Expected a numeric start for lockfileVersion, got: " <> toString verText)

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
  , name :: Maybe Text
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

-- | Catalog map contains package versions and their metadata
newtype CatalogMap = CatalogMap
  { catalogEntries :: Map Text CatalogEntry
  }
  deriving (Show, Eq, Ord)

instance FromJSON CatalogMap where
  parseJSON = Yaml.withObject "CatalogMap" $ \obj ->
    CatalogMap <$> traverse Yaml.parseJSON (Map.mapKeys Key.toText $ KeyMap.toMap obj)

data CatalogEntry = CatalogEntry
  { specifier :: Text
  , catalogVersion :: Text
  }
  deriving (Show, Eq, Ord)

instance FromJSON CatalogEntry where
  parseJSON = Yaml.withObject "CatalogEntry" $ \obj ->
    CatalogEntry
      <$> obj .: "specifier"
      <*> obj .: "version"

analyze :: (Has ReadFS sig m, Has Logger sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze file = context "Analyzing Pnpm Lockfile" $ do
  pnpmLockFile <- context "Parsing pnpm-lock file" $ readContentsYaml file

  case lockFileVersion pnpmLockFile of
    PnpmLockLt4 raw -> logWarn . pretty $ "pnpm-lock file is using older lockFileVersion: " <> raw <> ", which is not officially supported!"
    PnpmLockGt9 raw -> logWarn . pretty $ "pnpm-lock file is using version: " <> raw <> ". Support for versions greater than 9 may be limited."
    _ -> pure ()

  context "Building dependency graph" $ dispatchPnpmGraphBuilder pnpmLockFile

mkPkgKey :: PnpmLockfile -> Text -> Text -> Text
mkPkgKey lf name version = case lockFileVersion lf of
  PnpmLock4Or5 -> "/" <> name <> "/" <> version -- e.g., /scope/name/1.0.0 or /name/1.0.0
  PnpmLock6 -> "/" <> name <> "@" <> version -- e.g., /scope/name@1.0.0 or /name@1.0.0
  PnpmLockLt4 _ -> "/" <> name <> "/" <> version -- Older versions, assume slash format
  PnpmLock9 ->
    -- For v9, the key format can vary. We try multiple common formats found in `packages` or `snapshots`.
    -- The goal is to find a key that exists in the `packages` map of the lockfile.
    let keyWithSlash = "/" <> name <> "/" <> version -- e.g., /name/1.0.0
        keyWithAt = "/" <> name <> "@" <> version -- e.g., /name@1.0.0
        keyWithoutLeadingSlashAt = name <> "@" <> version -- e.g., name@1.0.0 (often seen in snapshots section)
        -- Less common, but seen in some older or aliased snapshot contexts for completeness:
        keyWithoutLeadingSlashSlash = name <> "/" <> version -- e.g., name/1.0.0
     in fromMaybe keyWithAt $
          -- Order of preference for checking existence in `packages` map:
          find
            (`Map.member` packages lf)
            [ keyWithSlash -- /name/1.0.0
            , keyWithAt -- /name@1.0.0
            , keyWithoutLeadingSlashAt -- name@1.0.0
            , keyWithoutLeadingSlashSlash -- name/1.0.0
            -- Note: Pnpm v9 often uses keys with peer dependency suffixes like /name@version(peer@version)
            -- in the `snapshots` section. This function primarily generates the base key for lookup
            -- in the `packages` section or as a component for snapshot keys.
            -- The `withoutPeerDepSuffix` helper is used elsewhere to strip those suffixes when needed.
            ]
  PnpmLockGt9 _ -> "/" <> name <> "@" <> version -- Future versions, assume 'at' format as a sensible default

buildGraph :: PnpmLockfile -> Graphing Dependency
buildGraph lockFile =
  withoutLocalPackages $
    runIdentity $
      evalGrapher $ do
        -- START: Local helpers for buildGraphLegacy, mimicking master's buildGraph logic
        -- Gets package name and version from package's key (v5 format).
        --
        -- >> getPkgNameVersionV5Legacy "" = Nothing
        -- >> getPkgNameVersionV5Legacy "github.com/something" = Nothing
        -- >> getPkgNameVersionV5Legacy "/pkg-a/1.0.0" = Just ("pkg-a", "1.0.0")
        -- >> getPkgNameVersionV5Legacy "/@angular/core/1.0.0" = Just ("@angular/core", "1.0.0")
        let getPkgNameVersionV5Legacy :: Text -> Maybe (Text, Text)
            getPkgNameVersionV5Legacy pkgKey = case Text.stripPrefix "/" pkgKey of
              Nothing -> Nothing
              Just txt ->
                let (nameWithSlash, version) = Text.breakOnEnd "/" txt
                 in case (Text.stripSuffix "/" nameWithSlash, version) of
                      (Just name, v) -> Just (name, v)
                      _ -> Nothing

            -- Gets package name and version from package's key (v6 format).
            --
            -- >> getPkgNameVersionV6Legacy "" = Nothing
            -- >> getPkgNameVersionV6Legacy "github.com/something" = Nothing
            -- >> getPkgNameVersionV6Legacy "/pkg-a@1.0.0" = Just ("pkg-a", "1.0.0")
            -- >> getPkgNameVersionV6Legacy "/@angular/core@1.0.0" = Just ("@angular/core", "1.0.0")
            -- >> getPkgNameVersionV6Legacy "/@angular/core@1.0.0(babel@1.0.0)" = Just ("@angular/core", "1.0.0(babel@1.0.0)")
            getPkgNameVersionV6Legacy :: Text -> Maybe (Text, Text)
            getPkgNameVersionV6Legacy pkgKey = case Text.stripPrefix "/" pkgKey of
              Nothing -> Nothing
              Just txt ->
                let (nameAndVersion, peerDepInfo) = Text.breakOn "(" txt -- Includes peer suffix
                    (nameWithSlash, version) = Text.breakOnEnd "@" nameAndVersion
                 in case (Text.stripSuffix "@" nameWithSlash, version) of
                      (Just name, v) -> Just (name, v <> peerDepInfo) -- Pass with peer suffix
                      _ -> Nothing

            -- Dispatches to the correct legacy version parser based on lockFileVersion.
            getPkgNameVersionLegacyDispatch :: PnpmLockFileVersion -> Text -> Maybe (Text, Text)
            getPkgNameVersionLegacyDispatch lockVer key = case lockVer of
              PnpmLock4Or5 -> getPkgNameVersionV5Legacy key
              PnpmLock6 -> getPkgNameVersionV6Legacy key
              PnpmLockLt4 _ -> getPkgNameVersionV5Legacy key -- v3 or below are deprecated, fallback to closest
              _ -> Nothing -- Should not be called by buildGraph for v9+
              -- END: Local helpers
        for_ (Map.toList $ importers lockFile) $ \(_, projectSnapshot) -> do
          let allDirectDependencies =
                Map.toList (directDependencies projectSnapshot)
                  <> Map.toList (directDevDependencies projectSnapshot)

          for_ allDirectDependencies $ \(depName, ProjectMapDepMetadata version) ->
            maybe (pure ()) direct $ toResolvedDependency depName version

        for_ (Map.toList $ packages lockFile) $ \(pkgKey, pkgMeta) -> do
          let deepDependencies =
                Map.toList (dependencies pkgMeta)
                  <> Map.toList (peerDependencies pkgMeta)

          let (depName, depVersion) = case getPkgNameVersionLegacyDispatch (lockFileVersion lockFile) pkgKey of
                Nothing -> (pkgKey, Nothing)
                Just (name, version) -> (name, Just version)
          let parentDep = toDependency depName depVersion pkgMeta

          deep parentDep

          for_ deepDependencies $ \(childName, childVersion) -> do
            maybe (pure ()) (edge parentDep) (toResolvedDependency childName childVersion)
  where
    -- This toDependency is scoped to buildGraphLegacy and mirrors master's logic
    toDependency :: Text -> Maybe Text -> PackageData -> Dependency
    toDependency name maybeVersion (PackageData isDev _ (RegistryResolve _) _ _) =
      toDep NodeJSType name (withoutPeerDepSuffix . withoutSymConstraint <$> maybeVersion) isDev
    toDependency _ _ (PackageData isDev _ (GitResolve (GitResolution url rev)) _ _) =
      toDep GitType url (Just rev) isDev
    toDependency _ _ (PackageData isDev _ (TarballResolve (TarballResolution url)) _ _) =
      toDep URLType url Nothing isDev
    toDependency _ _ (PackageData isDev (Just nameFromPackage) (DirectoryResolve _) _ _) =
      toDep UserType nameFromPackage Nothing isDev
    toDependency nameFromImporter _ (PackageData isDev Nothing (DirectoryResolve _) _ _) =
      toDep UserType nameFromImporter Nothing isDev

    toDep :: DepType -> Text -> Maybe Text -> Bool -> Dependency
    toDep depType name version dev = Dependency depType name (CEq <$> version) mempty (toEnv dev) mempty

    toEnv :: Bool -> Set.Set DepEnvironment
    toEnv isNotRequired = Set.singleton $ if isNotRequired then EnvDevelopment else EnvProduction

    -- Non-registry resolvers (tarball, git, directory) use non-version identifier
    -- as version value in dependencies map, as well as for it's `packages` key.
    --
    -- @
    -- >  dependencies:
    -- >    chokidar: 1.0.0 # resolved with registry
    -- >    some-other-project: file:../local-package # resolved with non-registry resolver
    -- @
    --
    -- For any dependency resolved via registry resolver, it will use
    -- the following format for its `packages` key:
    --
    --   - /${depName}/${depVersion} -- for v5 fmt
    --   - /${depName}@${depVersion} -- for v6 fmt
    --
    -- For dependency resolved via non-registry resolvers,
    -- it will use the dependency's version value for its `packages` key:
    --
    --    e.g.
    --      file:../local-package
    --
    toResolvedDependency :: Text -> Text -> Maybe Dependency
    toResolvedDependency depName depVersion =
      let -- This local mkPkgKey is specific to legacy formats (v5, v6)
          maybeNonRegistrySrcPackage = Map.lookup depVersion (packages lockFile)
          maybeRegistrySrcPackage = Map.lookup (mkPkgKey lockFile depName depVersion) (packages lockFile)
       in case (maybeNonRegistrySrcPackage, maybeRegistrySrcPackage) of
            (Nothing, Nothing) -> Nothing
            (Just nonRegistryPkg, _) -> Just $ toDependency depName Nothing nonRegistryPkg
            (_, Just registryPkg) -> Just $ toDependency depName (Just depVersion) registryPkg

-- Dispatches to the appropriate graph building logic based on lockfile version.
dispatchPnpmGraphBuilder :: (Has Logger sig m) => PnpmLockfile -> m (Graphing Dependency)
dispatchPnpmGraphBuilder lockFile = do
  case lockFileVersion lockFile of
    PnpmLock9 -> buildGraphWithSnapshots lockFile
    -- For older versions (pre-v9), call the legacy 'buildGraph' function.
    _ -> do
      logWarn "Using legacy pnpm graph builder (pre-v9 format)."
      pure (buildGraph lockFile)

-- Define SnapshotPackageData (focus on dependencies for now)
newtype SnapshotPackageData = SnapshotPackageData
  { snapshotDependencies :: Map Text Text
  -- TODO: Consider adding snapshotOptionalDependencies, snapshotTransitivePeerDependencies if needed later
  }
  deriving (Show, Eq, Ord)

instance FromJSON SnapshotPackageData where
  parseJSON = Yaml.withObject "SnapshotPackageData" $ \obj ->
    SnapshotPackageData
      <$> obj .:? "dependencies" .!= mempty

buildGraphWithSnapshots :: (Has Logger sig m) => PnpmLockfile -> m (Graphing Dependency)
buildGraphWithSnapshots lockFile =
  fmap withoutLocalPkgsSnap $
    evalGrapher $ do
      let catalogVersionMap = buildCatalogVersionMapSnapshots lockFile
      let snapshotsMap = snapshots lockFile

      let resolveSnapshotDependency :: Text -> Text -> Bool -> Maybe Dependency
          resolveSnapshotDependency canonicalDepName snapKeyOrRef isCtxDev = do
            -- `snapKeyOrRef` could be "name@version", "/name@version", "npm:name@version", etc.
            -- `canonicalDepName` is the name used in the importer's dependencies list (e.g., the alias)

            (trueNameFromSnapRef, versionFromSnapRef) <- parseSnapshotKey snapKeyOrRef
            -- trueNameFromSnapRef is now the actual package name (e.g. "safe-execa")
            -- versionFromSnapRef is the actual version (e.g. "0.1.2")

            -- The key for looking up in `packages` should use `trueNameFromSnapRef`
            let packageKeyInPackagesMap = mkPkgKey lockFile trueNameFromSnapRef versionFromSnapRef
            let maybePkgData = Map.lookup packageKeyInPackagesMap (packages lockFile)

            let finalIsDev = maybe isCtxDev isDev maybePkgData

            -- The name of the Dependency node should be the canonicalDepName (alias)
            -- The version should be versionFromSnapRef
            pure $ createDepSimple NodeJSType canonicalDepName (Just versionFromSnapRef) finalIsDev

      -- Define processImporterEntries here, after resolveSnapshotDependency
      let processImporterEntries :: (Has Logger sig m') => [(Text, ProjectMapDepMetadata)] -> Bool -> GrapherC Dependency m' ()
          processImporterEntries entries isDepDevFlag =
            for_ entries $ \(canonicalName, ProjectMapDepMetadata{version = resolvedRefStr}) -> do
              -- The 'resolvedRefStr' from an importer's 'version' field in a v9+ lockfile can take several forms:
              -- 1. Catalog alias: "catalog:<alias>" (e.g., "catalog:shared-react")
              -- 2. Local link: "link:<path>" (e.g., "link:../shared-lib")
              -- 3. Workspace link: "workspace:<path_or_version>" (e.g., "workspace:packages/foo", "workspace:^1.0.0")
              -- 4. Direct snapshot key: A key that should exist in the 'snapshots' section,
              --    often like "/<pkgName>@<version>", "<pkgName>@<version>", or "npm:<trueName>@<version>"
              --    (potentially with peer dependency suffixes like "(react@18.0.0)").
              -- 5. Plain version string: A simple version like "1.2.3".
              -- 6. URL/File path: For dependencies resolved via git, tarball, or local file paths
              --    that are used as keys in the 'packages' section (e.g., "git+ssh://...", "file:../foo.tgz").

              -- Case 1: Handle catalog aliases (e.g., "catalog:shared-react")
              if "catalog:" `Text.isPrefixOf` resolvedRefStr
                then do
                  let catalogAlias = Text.drop (Text.length "catalog:") resolvedRefStr
                  case Map.lookup catalogAlias catalogVersionMap of
                    Just actualVersion -> do
                      -- Try to find the package in the packages map first using the resolved catalog version
                      let pkgKeyToLookup = mkPkgKey lockFile canonicalName actualVersion
                      case Map.lookup pkgKeyToLookup (packages lockFile) of
                        Just pkgData -> do
                          let dep = resolveDependencySnapshots catalogVersionMap canonicalName (Just actualVersion) pkgData isDepDevFlag
                          deep dep >> direct dep
                        Nothing -> do
                          -- If not in packages, try to form a snapshot key and resolve via snapshots
                          let snapshotKeyAttempt = canonicalName <> "@" <> actualVersion
                          case resolveSnapshotDependency canonicalName snapshotKeyAttempt isDepDevFlag of
                            Just resolvedDep -> deep resolvedDep >> direct resolvedDep
                            Nothing -> logWarn $ pretty ("Catalog-resolved dep not found in packages or snapshots: " <> canonicalName <> "@" <> actualVersion)
                    Nothing -> logWarn $ pretty ("Unresolved catalog alias: " <> catalogAlias <> " for " <> canonicalName)
                -- Case 2: Handle local links (e.g., "link:../shared-lib")
                else
                  if "link:" `Text.isPrefixOf` resolvedRefStr
                    then do
                      -- Linked local packages are treated as UserType dependencies
                      let userDep = createDepSimple UserType canonicalName (Just resolvedRefStr) isDepDevFlag
                      deep userDep >> direct userDep
                    -- Case 3: Handle workspace links (e.g., "workspace:packages/foo")
                    else
                      if "workspace:" `Text.isPrefixOf` resolvedRefStr
                        then do
                          -- TODO: PNPM Workspace protocol handling is not yet fully implemented.
                          -- This might involve resolving to another local package or a version.
                          logWarn $ pretty ("Workspace protocol NYI for importer: " <> canonicalName <> " ref: " <> resolvedRefStr)
                          pure ()
                        -- Cases 4, 5, or 6: Not a catalog, link, or workspace reference.
                        -- This could be a direct snapshot key, a plain version, or a URL/file path.
                        else do
                          -- Attempt to resolve as a direct snapshot key first (Case 4).
                          -- `resolveSnapshotDependency` handles various snapshot key formats (e.g., "/foo@1.0", "foo@1.0", "npm:bar@2.0").
                          case resolveSnapshotDependency canonicalName resolvedRefStr isDepDevFlag of
                            Just resolvedDep -> deep resolvedDep >> direct resolvedDep
                            -- `resolvedRefStr` was not a direct snapshot key.
                            Nothing -> do
                              -- Strip potential peer dependency suffix (e.g., "(react@18.0.0)") before further checks.
                              let justVersionNoSuffix = withoutPeerDepSuffix resolvedRefStr

                              -- Case 6: Check if it's a URL or file path (used as a key in `packages` for non-registry deps).
                              if Text.isInfixOf "://" justVersionNoSuffix || Text.isPrefixOf "file:" justVersionNoSuffix
                                then do
                                  -- This path is for direct URL/file references in importers that also act as keys in the `packages` section.
                                  case Map.lookup justVersionNoSuffix (packages lockFile) of
                                    Just pkgData ->
                                      case getPkgNameVersionForV9Snapshots lockFile justVersionNoSuffix pkgData of
                                        Just (pkgNameFromData, versionMaybe) -> do
                                          let dep = resolveDependencySnapshots catalogVersionMap pkgNameFromData versionMaybe pkgData isDepDevFlag
                                          deep dep >> direct dep
                                        _ -> logWarn $ pretty ("Cannot get name/version for URL/file package key: " <> justVersionNoSuffix)
                                    Nothing -> logWarn $ pretty ("Importer URL/file reference not found in packages: " <> justVersionNoSuffix)
                                -- Case 5: Check if it's a plain version string.
                                else
                                  if not (Text.null justVersionNoSuffix)
                                    then do
                                      -- This path assumes `justVersionNoSuffix` is a simple version string (e.g., "1.2.3").
                                      -- Try to look up in `packages` using `canonicalName` and this version.
                                      let pkgKeyToLookup = mkPkgKey lockFile canonicalName justVersionNoSuffix
                                      case Map.lookup pkgKeyToLookup (packages lockFile) of
                                        Just pkgData -> do
                                          let dep = resolveDependencySnapshots catalogVersionMap canonicalName (Just justVersionNoSuffix) pkgData isDepDevFlag
                                          deep dep >> direct dep
                                        Nothing -> do
                                          -- Final fallback: Attempt to form a snapshot key "name@version" and resolve via snapshots.
                                          let snapshotKeyAttempt = canonicalName <> "@" <> justVersionNoSuffix
                                          case resolveSnapshotDependency canonicalName snapshotKeyAttempt isDepDevFlag of
                                            Just resolvedDepFromSnap -> deep resolvedDepFromSnap >> direct resolvedDepFromSnap
                                            Nothing -> logWarn $ pretty ("Importer dependency could not be resolved (plain version not in packages/snapshots): " <> canonicalName <> "@" <> justVersionNoSuffix)
                                    -- `justVersionNoSuffix` was empty, likely from something like "()"
                                    else
                                      logWarn $ pretty ("Empty version for importer dependency (after peer suffix removal): " <> canonicalName <> " ref: " <> resolvedRefStr)

      -- Step 1: Process importers to establish direct dependencies
      for_ (Map.toList $ importers lockFile) $ \(_, projectSnapshot) -> do
        processImporterEntries (Map.toList $ directDependencies projectSnapshot) False
        processImporterEntries (Map.toList $ directDevDependencies projectSnapshot) True

      -- Step 2: Process snapshots for transitive dependencies
      for_ (Map.toList snapshotsMap) $ \(parentSnapKeyStr, snapshotData) -> do
        case parseSnapshotKey parentSnapKeyStr of
          Just (parentCanonicalName, _) ->
            case resolveSnapshotDependency parentCanonicalName parentSnapKeyStr False of
              Just parentDep -> do
                deep parentDep
                let parentIsDev = Set.member EnvDevelopment (dependencyEnvironments parentDep)
                let childDeps = Map.toList $ snapshotDependencies snapshotData
                for_ childDeps $ \(childCanonicalName, childVersionRef) -> do
                  let fullChildSnapKey = childCanonicalName <> "@" <> childVersionRef
                  case resolveSnapshotDependency childCanonicalName fullChildSnapKey parentIsDev of
                    Just childDep -> do
                      deep childDep
                      edge parentDep childDep
                    Nothing ->
                      when (Text.isInfixOf "://" childVersionRef || Text.isPrefixOf "file:" childVersionRef) $
                        case Map.lookup childVersionRef (packages lockFile) of
                          Just pkgData ->
                            case getPkgNameVersionForV9Snapshots lockFile childVersionRef pkgData of
                              Just (pkgNameFromData, versionMaybe) -> do
                                let concreteChildDep =
                                      resolveDependencySnapshots
                                        catalogVersionMap
                                        pkgNameFromData
                                        versionMaybe
                                        pkgData
                                        parentIsDev
                                deep concreteChildDep
                                edge parentDep concreteChildDep
                              _ -> pure ()
                          _ -> pure ()
              Nothing -> pure () -- Couldn't resolve parentDep from parentSnapKeyStr
          Nothing -> pure () -- Couldn't parse parentSnapKeyStr
  where
    withoutLocalPkgsSnap = withoutLocalPackages

    parseSnapshotKey :: Text -> Maybe (Text, Text)
    parseSnapshotKey rawKey =
      let keyNoPeers = withoutPeerDepSuffix rawKey
          -- Strip "npm:" prefix if present, to get the actual package_name@version string
          keyToParse = fromMaybe keyNoPeers (Text.stripPrefix "npm:" keyNoPeers)
       in case Text.splitOn "@" keyToParse of
            -- Handle scoped packages like @scope/name@version
            ["", scopeAndName, version]
              | not (Text.null scopeAndName) && not (Text.null version) ->
                  Just ("@" <> scopeAndName, version)
            -- Handle non-scoped packages like name@version (ensure it's not an empty part before @)
            [name, version]
              | not (Text.null name) && not (Text.null version) && name /= "" ->
                  Just (name, version)
            _ -> Nothing -- Invalid format or unhandled case (e.g. multiple @ in non-scoped name part)
    createDepSimple :: DepType -> Text -> Maybe Text -> Bool -> Dependency
    createDepSimple depType name version isDepGraphDev =
      Dependency depType name (CEq <$> version) mempty (Set.singleton $ if isDepGraphDev then EnvDevelopment else EnvProduction) mempty

    buildCatalogVersionMapSnapshots :: PnpmLockfile -> Map Text Text
    buildCatalogVersionMapSnapshots lf =
      let defaultCatalog =
            Map.findWithDefault Map.empty "default" (Map.map catalogEntriesMap (catalogs lf))
       in Map.map (cleanupVersionSnapshots lf) defaultCatalog
      where
        catalogEntriesMap = Map.map catalogVersion . catalogEntries

    -- Improved version cleanup for v9 snapshots
    cleanupVersionSnapshots :: PnpmLockfile -> Text -> Text
    cleanupVersionSnapshots lf version =
      let cleaned = removeLinks $ removePrefixes $ withoutPeerDepSuffix version
       in case lockFileVersion lf of
            PnpmLock9 -> cleaned -- For v9, respect shouldApplySymConstraint which returns False
            _ -> if shouldApplySymConstraint lf then withoutSymConstraint cleaned else cleaned
      where
        removePrefixes v
          | "@" `Text.isInfixOf` v && Text.count "@" v > 1 =
              let parts = Text.splitOn "@" v
               in if length parts >= 3
                    then fromMaybe v (listToMaybe (reverse parts))
                    else v
          | otherwise = v
        removeLinks v
          | "link:" `Text.isPrefixOf` v = ""
          | "workspace:" `Text.isPrefixOf` v = ""
          | "catalog:" `Text.isPrefixOf` v = ""
          | otherwise = v

    -- Restore required local helpers
    resolveDependencySnapshots :: Map Text Text -> Text -> Maybe Text -> PackageData -> Bool -> Dependency
    resolveDependencySnapshots catMap depName maybeVersion pkgData contextIsDev =
      case resolution pkgData of
        GitResolve (GitResolution url rev) ->
          createDepSimple GitType url (Just rev) (isDev pkgData || contextIsDev)
        TarballResolve (TarballResolution url) ->
          createDepSimple URLType url Nothing (isDev pkgData || contextIsDev)
        DirectoryResolve _ ->
          createDepSimple UserType (fromMaybe depName (name pkgData)) Nothing (isDev pkgData || contextIsDev)
        RegistryResolve _ ->
          let resolvedVersion =
                maybeVersion >>= \v ->
                  if "catalog:" `Text.isPrefixOf` v
                    then Map.lookup depName catMap
                    else Just $ cleanupVersionSnapshots lockFile v
           in createDepSimple NodeJSType depName resolvedVersion (isDev pkgData || contextIsDev)

    getPkgNameVersionForV9Snapshots :: PnpmLockfile -> Text -> PackageData -> Maybe (Text, Maybe Text)
    getPkgNameVersionForV9Snapshots _ key pkgMeta =
      case resolution pkgMeta of
        GitResolve (GitResolution url rev) -> Just (url, Just rev)
        TarballResolve (TarballResolution url) -> Just (url, Nothing)
        DirectoryResolve _ -> (,) <$> name pkgMeta <*> pure Nothing
        RegistryResolve _ ->
          case parseSnapshotKey key of
            Just (n, v) -> Just (n, Just v)
            Nothing ->
              (,) <$> name pkgMeta <*> pure (extractVersionFromKey key)
      where
        extractVersionFromKey :: Text -> Maybe Text
        extractVersionFromKey k =
          let parts = Text.splitOn "@" k
           in if length parts > 1
                then listToMaybe (reverse parts)
                else
                  let slashParts = Text.splitOn "/" k
                   in if length slashParts >= 3 then slashParts `atMay` 2 else Nothing

        atMay :: [a] -> Int -> Maybe a
        atMay xs i = listToMaybe (drop i xs)

    shouldApplySymConstraint :: PnpmLockfile -> Bool
    shouldApplySymConstraint lock = case lockFileVersion lock of
      PnpmLock9 -> False
      PnpmLockGt9 _ -> False
      _ -> True
