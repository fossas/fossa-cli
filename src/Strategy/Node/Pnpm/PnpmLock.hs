{-# LANGUAGE ScopedTypeVariables #-}

module Strategy.Node.Pnpm.PnpmLock (
  analyze,
  dispatchPnpmGraphBuilder,
  buildGraph,
  PnpmLockfile (..),
  PnpmLockFileVersion (..),
) where

import Control.Applicative (Alternative (..))
import Control.Effect.Diagnostics (Diagnostics, Has, context)
import Data.Aeson.Extra (TextLike (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Foldable (for_)
import Data.Functor.Identity (runIdentity)
import Data.List (find, nub)
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
import DepTypes (DepEnvironment (..), DepType (..), Dependency (..), VerConstraint (CEq))
import Effect.Grapher (GrapherC, deep, direct, edge, evalGrapher)
import Effect.Logger (Logger, logWarn, pretty)
import Effect.ReadFS (ReadFS, readContentsYaml)
import Graphing (Graphing, edgesList, promoteToDirect, shrink)
import Path (Abs, File, Path)

-- Helper to create DepEnvironment set from a boolean (True for Development)
toEnv :: Bool -> Set.Set DepEnvironment
toEnv isDev = Set.singleton $ if isDev then EnvDevelopment else EnvProduction

-- | Parses a snapshot key (e.g., "@angular/core@18.2.9(rxjs@7.8.2)(zone.js@0.14.10)")
--   into (package name, version), stripping any peer dependency context.
parseSnapshotKey :: Text -> Maybe (Text, Text)
parseSnapshotKey rawKey =
  let noPeers = withoutPeerDepSuffix rawKey
      atIndex = ((\i -> Text.length noPeers - i - 1) <$> Text.findIndex (== '@') (Text.reverse noPeers))
   in case atIndex of
        Just i ->
          let (namePart, versionPart) = Text.splitAt i noPeers
              name = namePart
              version = Text.drop 1 versionPart
           in if Text.null name || Text.null version
                then Nothing
                else Just (name, version)
        Nothing -> Nothing

-- | Remove local packages (UserType) **except** the synthetic workspace root ".".
--   Keeping that node lets us inspect importer-level relationships later if needed.
withoutLocalPackages :: Graphing Dependency -> Graphing Dependency
withoutLocalPackages = shrink (\dep -> dependencyType dep /= UserType || dependencyName dep == syntheticRootPath)

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
  -- ^ Declared name of the package (present for non-registry resolutions)
  , pkgVersion :: Maybe Text
  -- ^ Populated only when the package was resolved via a non-registry
  --   resolver (git, tarball, directory). For registry resolutions the
  --   version is embedded in the key itself and this field is absent.
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
      <*> obj .:? "version"
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

-- | Analyzes a pnpm lockfile and returns a graph of dependencies.
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
        -- Local helpers for buildGraphLegacy, for pre-v9 lockfile logic
        -- Gets package name and version from package's key (v5 format).
        --
        -- >> getPkgNameVersionV5Legacy "" = Nothing
        -- >> getPkgNameVersionV5Legacy "github.com/something" = Nothing
        -- >> getPkgNameVersionV5Legacy "/pkg-a/1.0.0" = Just ("pkg-a", "1.0.0")
        -- >> getPkgNameVersionV5Legacy "/@angular/core/1.0.0" = Just ("@angular/core", "1.0.0")
        let getPkgNameVersionV5Legacy :: Text -> Maybe (Text, Text)
            getPkgNameVersionV5Legacy pkgKey = do
              txt <- Text.stripPrefix "/" pkgKey
              let (nameWithSlash, version) = Text.breakOnEnd "/" txt
              name <- Text.stripSuffix "/" nameWithSlash
              pure (name, version)

            -- Gets package name and version from package's key (v6 format).
            --
            -- >> getPkgNameVersionV6Legacy "" = Nothing
            -- >> getPkgNameVersionV6Legacy "github.com/something" = Nothing
            -- >> getPkgNameVersionV6Legacy "/pkg-a@1.0.0" = Just ("pkg-a", "1.0.0")
            -- >> getPkgNameVersionV6Legacy "/@angular/core@1.0.0" = Just ("@angular/core", "1.0.0")
            -- >> getPkgNameVersionV6Legacy "/@angular/core@1.0.0(babel@1.0.0)" = Just ("@angular/core", "1.0.0(babel@1.0.0)")
            getPkgNameVersionV6Legacy :: Text -> Maybe (Text, Text)
            getPkgNameVersionV6Legacy pkgKey = do
              txt <- Text.stripPrefix "/" pkgKey
              let (nameAndVersion, peerDepInfo) = Text.breakOn "(" txt -- Includes peer suffix
                  (nameWithSlash, version) = Text.breakOnEnd "@" nameAndVersion
              name <- Text.stripSuffix "@" nameWithSlash
              pure (name, version <> peerDepInfo)

            -- Dispatches to the correct legacy version parser based on lockFileVersion.
            getPkgNameVersionLegacyDispatch :: PnpmLockFileVersion -> Text -> Maybe (Text, Text)
            getPkgNameVersionLegacyDispatch lockVer key = case lockVer of
              PnpmLock4Or5 -> getPkgNameVersionV5Legacy key
              PnpmLock6 -> getPkgNameVersionV6Legacy key
              PnpmLockLt4 _ -> getPkgNameVersionV5Legacy key -- v3 or below are deprecated and are not used in practice, fallback to closest
              _ -> Nothing -- Should not be called by buildGraph for v9+
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
          let parentDep = toDependencyLegacy depName depVersion pkgMeta

          -- It is ok, if this dependency was already graphed as direct
          -- @direct 1 <> deep 1 = direct 1@
          deep parentDep

          for_ deepDependencies $ \(childName, childVersion) -> do
            maybe (pure ()) (edge parentDep) (toResolvedDependency childName childVersion)
  where
    toDependencyLegacy :: Text -> Maybe Text -> PackageData -> Dependency
    toDependencyLegacy name maybeVersion (PackageData isDev _ _ (RegistryResolve _) _ _) =
      toDep NodeJSType name (withoutPeerDepSuffix . withoutSymConstraint <$> maybeVersion) isDev
    toDependencyLegacy _ _ (PackageData isDev _ _ (GitResolve (GitResolution url rev)) _ _) =
      toDep GitType url (Just rev) isDev
    -- For legacy (v5/v6), use the tarball URL as the dependency name
    toDependencyLegacy _ _ (PackageData isDev _ _ (TarballResolve (TarballResolution url)) _ _) =
      toDep URLType url Nothing isDev
    toDependencyLegacy _ _ (PackageData isDev (Just nameFromPackage) _ (DirectoryResolve _) _ _) =
      toDep UserType nameFromPackage Nothing isDev
    toDependencyLegacy nameFromImporter _ (PackageData isDev Nothing _ (DirectoryResolve _) _ _) =
      toDep UserType nameFromImporter Nothing isDev

    toDep :: DepType -> Text -> Maybe Text -> Bool -> Dependency
    toDep depType name version dev = Dependency depType name (CEq <$> version) mempty (toEnv dev) mempty

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
      let maybeNonRegistrySrcPackage = Map.lookup depVersion (packages lockFile)
          maybeRegistrySrcPackage = Map.lookup (mkPkgKey lockFile depName depVersion) (packages lockFile)
       in case (maybeNonRegistrySrcPackage, maybeRegistrySrcPackage) of
            (Nothing, Nothing) -> Nothing
            (Just nonRegistryPkg, _) -> Just $ toDependencyLegacy depName Nothing nonRegistryPkg
            (_, Just registryPkg) -> Just $ toDependencyLegacy depName (Just depVersion) registryPkg

-- Dispatches to the appropriate graph building logic based on lockfile version.
dispatchPnpmGraphBuilder :: (Has Logger sig m, Has Diagnostics sig m) => PnpmLockfile -> m (Graphing Dependency)
dispatchPnpmGraphBuilder lockFile = do
  case lockFileVersion lockFile of
    PnpmLock9 -> buildGraphWithSnapshots lockFile
    PnpmLockGt9 _ -> buildGraphWithSnapshots lockFile
    -- For older versions (pre-v9), call the legacy 'buildGraph' function.
    _ -> do
      logWarn "Using legacy pnpm graph builder (pre-v9 format)."
      pure (buildGraph lockFile)

-- Define SnapshotPackageData (focus on dependencies for now)
data SnapshotPackageData = SnapshotPackageData
  { snapshotDependencies :: Map Text Text
  , snapshotOptionalDependencies :: Map Text Text
  , snapshotIsDev :: Bool -- Renamed from 'dev'
  -- TODO: Consider adding snapshotTransitivePeerDependencies if needed later
  }
  deriving (Show, Eq, Ord)

instance FromJSON SnapshotPackageData where
  parseJSON = Yaml.withObject "SnapshotPackageData" $ \obj ->
    SnapshotPackageData
      <$> obj .:? "dependencies" .!= mempty
      <*> obj .:? "optionalDependencies" .!= mempty
      <*> obj .:? "dev" .!= False -- Field name in YAML is still 'dev'

-- | This is the new graph builder for PNPM v9+ lockfiles that primarily uses the `snapshots` section.
buildGraphWithSnapshots ::
  forall sig m.
  (Has Logger sig m, Has Diagnostics sig m) =>
  PnpmLockfile ->
  m (Graphing Dependency)
buildGraphWithSnapshots lockFile = do
  -- First build the raw graph (including the synthetic workspace root " . ")
  initial <- evalGrapher (processImporterEntriesV9 lockFile)

  -- Identify all nodes that have the synthetic workspace root as their ONLY parent.
  let dotChildren =
        Set.fromList
          [ child
          | (parent, child) <- edgesList initial
          , isSyntheticRootDep parent
          ]

  let promotePredicate dep =
        Set.member dep dotChildren
          && not
            ( any
                (\(p', c) -> c == dep && dependencyName p' /= syntheticRootPath)
                (edgesList initial)
            )

  -- Promote those nodes to direct, then drop other local packages (but keep ".").
  let promoted = promoteToDirect promotePredicate initial

  pure $ withoutLocalPackages promoted

--
-- V9+ Graph Building Helpers
--

data V9GraphingContext = V9GraphingContext
  { ctxSnapshots :: Map Text SnapshotPackageData
  , ctxPackages :: Map Text PackageData
  }

-- | Alias representing a canonical key used in the @snapshots@ map of a v9
--   pnpm-lockfile.  Using a named type clarifies intent without incurring any
--   runtime cost (it is just @Text@ underneath).
type SnapshotKey = Text

-- Parses "name@version(peers)" into ("name", "version(peers)")
-- or "name@version" into ("name", "version")
parseSnapshotKeyPreservingPeers :: SnapshotKey -> Maybe (Text, Text)
parseSnapshotKeyPreservingPeers key =
  case Text.findIndex (== '@') (Text.reverse key) of -- Find last '@'
    Nothing -> Nothing -- No '@' found
    Just revIdx ->
      let atIndex = Text.length key - revIdx - 1
          (namePart, versionPartWithAt) = Text.splitAt atIndex key
       in if Text.null namePart || Text.null (Text.drop 1 versionPartWithAt)
            then Nothing -- Empty name or version
            else Just (namePart, Text.drop 1 versionPartWithAt)

-- | Normalizes a raw snapshot key reference (often from a dependency value in snapshots or importers)
--   to a more canonical form that is likely to match a key in the `snapshots` map.
--   Handles cases like missing leading slashes or `npm:` prefixes.
normalizeRawSnapshotKeyRef :: SnapshotKey -> SnapshotKey
normalizeRawSnapshotKeyRef keyRef
  | "@@" `Text.isInfixOf` keyRef =
      let parts = Text.splitOn "@@" keyRef
          aliasedPart = fromMaybe keyRef (listToMaybe (reverse parts))
       in normalizeRawSnapshotKeyRef aliasedPart
  | Text.isPrefixOf "npm:" keyRef =
      let strippedNpm = Text.drop 4 keyRef
       in normalizeRawSnapshotKeyRef strippedNpm
  | Text.isPrefixOf "/" keyRef =
      Text.drop 1 keyRef
  | otherwise = keyRef

-- | Parses a version string that may contain nested/sibling peer dependencies in parentheses.
--   Returns a list of all possible base versions, from most specific to least.
--   e.g., "A(B(C))" -> ["A(B(C))", "A(B)", "A"]
--   e.g., "1.0.0(react@17.0.0)(redux@5.0.0)" -> ["1.0.0(react@17.0.0)(redux@5.0.0)", "1.0.0(react@17.0.0)", "1.0.0"]
generateVersionCandidates :: Text -> [Text]
generateVersionCandidates ver = nub $ ver : go ver
  where
    go current =
      case Text.breakOnEnd "(" current of
        (_, "") -> [] -- No more parens
        (base, _) ->
          case Text.unsnoc base of
            Just (safeBase, _) -> safeBase : go safeBase
            Nothing -> []

generateCandidateSnapshotKeysV9 :: Maybe Text -> SnapshotKey -> [SnapshotKey]
generateCandidateSnapshotKeysV9 maybeDepNameFromContext snapKeyRefFromParent =
  let -- 1. Canonical form of the reference (strip npm:, @@ alias etc.)
      canonical = normalizeRawSnapshotKeyRef snapKeyRefFromParent

      -- keep only two base variants: with and without leading slash
      rawKeyCandidates = nub [canonical, "/" <> canonical]

      -- 2. Versions with progressively stripped peer-suffixes
      versionCandidates = generateVersionCandidates snapKeyRefFromParent

      -- 3. Combine each version string with a package name context
      nameAndVersionCandidates =
        case maybeDepNameFromContext of
          Nothing ->
            -- The raw ref itself might be a full name@ver string
            nub
              . concatMap
                ( \ver -> case parseSnapshotKeyPreservingPeers ver of
                    Just (nm, verWithPeers) ->
                      let base = nm <> "@" <> verWithPeers in [base, "/" <> base]
                    Nothing -> []
                )
              $ versionCandidates
          Just depName ->
            nub
              . concatMap
                ( \ver ->
                    let base = depName <> "@" <> ver in [base, "/" <> base]
                )
              $ versionCandidates
   in nub $ rawKeyCandidates ++ nameAndVersionCandidates

lookupSnapshotV9 :: (Has Logger sig m, Has Diagnostics sig m) => Map SnapshotKey SnapshotPackageData -> Maybe Text -> SnapshotKey -> GrapherC Dependency m (Maybe (SnapshotPackageData, SnapshotKey))
lookupSnapshotV9 snapshotsMap maybeDepNameFromContext snapKeyRefFromParent = do
  let candidates = generateCandidateSnapshotKeysV9 maybeDepNameFromContext snapKeyRefFromParent
  let existingCandidates = filter (`Map.member` snapshotsMap) candidates
  let maybeBestKey = listToMaybe existingCandidates
  let maybeResult = maybeBestKey >>= (\key -> (key,) <$> Map.lookup key snapshotsMap)
  case maybeResult of
    Just (bestKey, packageData) -> do
      pure (Just (packageData, bestKey))
    Nothing -> do
      context "lookupSnapshotV9" $
        logWarn
          ( "[PNPM v9] Snapshot data missing."
              <> "\n  Original reference from parent: "
              <> pretty snapKeyRefFromParent
              <> ( case maybeDepNameFromContext of
                    Just n -> pretty ("\n  Context name from parent: " :: Text) <> pretty n
                    Nothing -> mempty
                 )
          )
      pure Nothing

createRootDepV9 :: Text -> Dependency
createRootDepV9 importerPath = createDepSimple UserType importerPath Nothing False

createDepFromUnresolvedV9 :: Dependency -> Text -> Text -> Text -> Dependency
createDepFromUnresolvedV9 parentDep childDepName childDepVersionRef reason =
  let message = reason <> " (Parent: " <> dependencyName parentDep <> ", ChildRef: " <> childDepName <> "@" <> childDepVersionRef <> ")"
   in createDepSimple NodeJSType childDepName (Just message) False

buildThisDepV9 :: (Has Logger sig m) => Map Text PackageData -> Maybe Dependency -> Text -> SnapshotPackageData -> Text -> GrapherC Dependency m Dependency
buildThisDepV9 packagesMetaMap maybeParentDep depKeyName snapshotData resolvedSnapKey = do
  let maybePackageMeta = Map.lookup resolvedSnapKey packagesMetaMap
  let declaredName = name =<< maybePackageMeta
  let declaredVersion = pkgVersion =<< maybePackageMeta
  let isDevFromSnapshot = snapshotIsDev snapshotData
  let isDevFromPackageMeta = maybe False isDev maybePackageMeta
  let (parsedNameFromSnapKey, parsedVersionFromSnapKey) =
        case parseSnapshotKey (normalizeRawSnapshotKeyRef resolvedSnapKey) of
          Just (n, v) -> (n, Just v)
          Nothing -> (depKeyName, Nothing)
  let finalName = fromMaybe parsedNameFromSnapKey declaredName
  let finalVersion = declaredVersion <|> parsedVersionFromSnapKey
  let currentIsDev = isDevFromSnapshot || isDevFromPackageMeta
  let depType = NodeJSType
  let thisDep = createDepSimple depType finalName finalVersion currentIsDev
  case maybeParentDep of
    Just parent -> edge parent thisDep
    Nothing -> pure ()
  pure thisDep

processSnapshotTreeV9 ::
  (Has Logger sig m, Has Diagnostics sig m) =>
  V9GraphingContext ->
  Dependency ->
  SnapshotPackageData ->
  Set.Set Text ->
  GrapherC Dependency m ()
processSnapshotTreeV9 v9ctx currentDep currentSnapshotData visited = do
  let childRefs =
        Map.toList (snapshotDependencies currentSnapshotData)
          <> Map.toList (snapshotOptionalDependencies currentSnapshotData)
  for_ childRefs $ \(childKeyName, childSnapKeyRef) -> do
    mResolvedChildSnap <- lookupSnapshotV9 (ctxSnapshots v9ctx) (Just childKeyName) childSnapKeyRef
    case mResolvedChildSnap of
      Just (childSnapshotData, childResolvedSnapKey) ->
        if Set.member childResolvedSnapKey visited
          then pure ()
          else do
            childDep <- buildThisDepV9 (ctxPackages v9ctx) (Just currentDep) childKeyName childSnapshotData childResolvedSnapKey
            deep childDep
            processSnapshotTreeV9 v9ctx childDep childSnapshotData (Set.insert childResolvedSnapKey visited)
      Nothing -> do
        let fallbackDep = createDepFromUnresolvedV9 currentDep childKeyName childSnapKeyRef "Snapshot missing, fallback for child in processSnapshotTree"
        deep fallbackDep

resolveSnapshotDependencyOrFallback ::
  (Has Logger sig m, Has Diagnostics sig m) =>
  V9GraphingContext ->
  Dependency ->
  Bool ->
  Text ->
  Text ->
  GrapherC Dependency m ()
resolveSnapshotDependencyOrFallback v9ctx _ entryIsDev originalDepName (depVersionRef :: SnapshotKey) = do
  mSnapshot <- lookupSnapshotV9 (ctxSnapshots v9ctx) (Just originalDepName) depVersionRef
  case mSnapshot of
    Just (snapData, snapKey) -> do
      context "resolveSnapshotDependencyOrFallback" $ logWarn $ pretty $ "[PNPM v9] Snapshot found for " <> originalDepName <> "@" <> depVersionRef <> " using key: " <> snapKey
      createdDep <- buildThisDepV9 (ctxPackages v9ctx) Nothing originalDepName snapData snapKey
      direct createdDep
      processSnapshotTreeV9 v9ctx createdDep snapData (Set.singleton snapKey)
    Nothing -> do
      context "resolveSnapshotDependencyOrFallback" $ logWarn $ pretty $ "[PNPM v9] Snapshot MISSING for " <> originalDepName <> "@" <> depVersionRef
      let dep = createDepSimple NodeJSType originalDepName (Just depVersionRef) entryIsDev
      direct dep

resolveLinkDependency ::
  (Has Logger sig m, Has Diagnostics sig m) =>
  V9GraphingContext ->
  Dependency ->
  Text ->
  Text ->
  Text ->
  Bool ->
  GrapherC Dependency m ()
resolveLinkDependency v9ctx _ importerKey depName depVersionRef entryIsDev = do
  let linkTarget = Text.drop (Text.length "link:") depVersionRef
      resolvedPath = resolveLinkPath importerKey linkTarget

  context "resolveLinkDependency" $
    logWarn $
      pretty
        ( "[PNPM v9] Resolving link dependency:"
            <> "\n  Importer: "
            <> importerKey
            <> "\n  Target: "
            <> depVersionRef
            <> "\n  Resolved Path: "
            <> resolvedPath
        )
  case Map.lookup resolvedPath (ctxPackages v9ctx) of
    Just pkgMeta -> do
      let linkedName = fromMaybe depName (name pkgMeta)
          isDevDep = isDev pkgMeta || entryIsDev
      case pkgVersion pkgMeta of
        Just ver -> do
          -- The linked package has a version, try to find it in snapshots to traverse deeper.
          mSnap <- lookupSnapshotV9 (ctxSnapshots v9ctx) (Just linkedName) ver
          case mSnap of
            Just (snapData, key) -> do
              -- Found in snapshots, build the full dependency tree from here.
              dep <- buildThisDepV9 (ctxPackages v9ctx) Nothing linkedName snapData key
              direct dep
              processSnapshotTreeV9 v9ctx dep snapData (Set.singleton key)
            Nothing -> do
              -- Not in snapshots, so it's a terminal node.
              let fallbackDep = createDepSimple NodeJSType linkedName (Just ver) isDevDep
              direct fallbackDep
        Nothing -> do
          -- No version, so it's a UserType dependency (local workspace package not published).
          let fallbackDep = createDepSimple UserType linkedName Nothing isDevDep
          direct fallbackDep
    Nothing -> do
      -- The resolved path doesn't match any package. This is a problem.
      logWarn $
        pretty
          ( "[PNPM v9] Could not find package metadata for linked dependency."
              <> "\n  Importer Key: "
              <> importerKey
              <> "\n  Dependency Name: "
              <> depName
              <> "\n  Version Ref (Link): "
              <> depVersionRef
              <> "\n  Resolved Path: "
              <> resolvedPath
          )
      -- Create a fallback dependency to not lose the information entirely.
      let fallbackDep = createDepSimple UserType depName (Just depVersionRef) entryIsDev
      direct fallbackDep

processSingleImporterEntryV9 ::
  (Has Logger sig m, Has Diagnostics sig m) =>
  V9GraphingContext ->
  Dependency ->
  Text ->
  Bool ->
  Text ->
  Text ->
  GrapherC Dependency m ()
processSingleImporterEntryV9 v9ctx rootDep importerKey entryIsDev originalDepName depVersionRef = do
  context "processSingleImporterEntryV9" $ logWarn $ pretty $ "[PNPM v9] Processing importer " <> importerKey <> " dep: " <> originalDepName <> "@" <> depVersionRef
  if "link:" `Text.isPrefixOf` depVersionRef
    then resolveLinkDependency v9ctx rootDep importerKey originalDepName depVersionRef entryIsDev
    else resolveSnapshotDependencyOrFallback v9ctx rootDep entryIsDev originalDepName (depVersionRef :: SnapshotKey)

processImporterEntriesV9 :: (Has Logger sig m, Has Diagnostics sig m) => PnpmLockfile -> GrapherC Dependency m ()
processImporterEntriesV9 lockFile = do
  let v9ctx = V9GraphingContext (snapshots lockFile) (packages lockFile)
  for_ (Map.toList $ importers lockFile) $ \(_, projectSnapshot) -> do
    let allDirectDependencies =
          Map.toList (directDependencies projectSnapshot)
            <> Map.toList (directDevDependencies projectSnapshot)
    let rootDep = createRootDepV9 syntheticRootPath
    for_ allDirectDependencies $ \(depName, ProjectMapDepMetadata version) ->
      processSingleImporterEntryV9 v9ctx rootDep syntheticRootPath False depName version

createDepSimple :: DepType -> Text -> Maybe Text -> Bool -> Dependency
createDepSimple depType name versionM isDev =
  let cleanedVersionM = withoutPeerDepSuffix <$> versionM
   in Dependency depType name (CEq <$> cleanedVersionM) mempty (toEnv isDev) mempty

-- | Normalizes a path by:
--   1. Converting to a proper path format
--   2. Resolving ".." segments by removing parent directories
--   3. Removing empty segments and "." segments
--   4. Joining segments back together
normalizePath :: Text -> Text
normalizePath path =
  let segments = Text.splitOn "/" path
      normalizedSegments = go [] segments
   in Text.intercalate "/" normalizedSegments
  where
    go acc [] = reverse acc
    go acc (".." : rest) = go acc rest
    go acc ("." : rest) = go acc rest
    go acc (segment : rest) = go (segment : acc) rest

-- | Resolves a link path relative to an importer path
--   1. Handles both absolute and relative paths
--   2. Normalizes the resulting path
--   3. Strips leading/trailing slashes for consistency
resolveLinkPath :: Text -> Text -> Text
resolveLinkPath importerPath linkPath =
  let cleanImporter = Text.strip $ Text.dropWhileEnd (== '/') importerPath
      cleanLink = Text.strip $ Text.dropWhile (== '/') linkPath
      resolvedPath =
        if Text.isPrefixOf "/" cleanLink
          then cleanLink
          else normalizePath $ cleanImporter <> "/" <> cleanLink
   in Text.strip resolvedPath

-- -----------------------------------------------------------------------------
-- Constants / helpers shared across legacy and v9 builders
-- -----------------------------------------------------------------------------

-- | The key that pnpm uses to represent the workspace root importer in
--   `importers` (i.e. the package.json at repository root).
syntheticRootPath :: Text
syntheticRootPath = "."

-- | Predicate for the synthetic workspace-root dependency node we synthesize
--   in v9 graphs (type @UserType@ and name ".").
isSyntheticRootDep :: Dependency -> Bool
isSyntheticRootDep dep = dependencyType dep == UserType && dependencyName dep == syntheticRootPath
