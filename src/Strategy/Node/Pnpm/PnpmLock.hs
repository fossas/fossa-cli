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
import Control.Monad (unless)
import Data.Aeson.Extra (TextLike (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Foldable (for_)
import Data.Functor.Identity (runIdentity)
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.String.Conversion (toString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read (decimal)
import Data.Yaml (FromJSON, Object, Parser, (.!=), (.:), (.:?))
import Data.Yaml qualified as Yaml
import DepTypes (DepEnvironment (..), DepType (..), Dependency (..), VerConstraint (CEq))
import Effect.Grapher (GrapherC, deep, direct, edge, evalGrapher)
import Effect.Logger (
  Logger,
  logWarn,
  pretty,
 )
import Effect.ReadFS (ReadFS, readContentsYaml)
import Graphing (Graphing, shrink)
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
          let parentDep = toDependency depName depVersion pkgMeta

          -- It is ok, if this dependency was already graphed as direct
          -- @direct 1 <> deep 1 = direct 1@
          deep parentDep

          for_ deepDependencies $ \(childName, childVersion) -> do
            maybe (pure ()) (edge parentDep) (toResolvedDependency childName childVersion)
  where
    toDependency :: Text -> Maybe Text -> PackageData -> Dependency
    toDependency name maybeVersion (PackageData isDev _ (RegistryResolve _) _ _) =
      toDep NodeJSType name (withoutPeerDepSuffix . withoutSymConstraint <$> maybeVersion) isDev
    toDependency _ _ (PackageData isDev _ (GitResolve (GitResolution url rev)) _ _) =
      toDep GitType url (Just rev) isDev
    toDependency depName _ (PackageData isDev mName (TarballResolve _) _ _) =
      let logicalName = fromMaybe depName mName
       in toDep URLType logicalName Nothing isDev
    toDependency _ _ (PackageData isDev (Just nameFromPackage) (DirectoryResolve _) _ _) =
      toDep UserType nameFromPackage Nothing isDev
    toDependency nameFromImporter _ (PackageData isDev Nothing (DirectoryResolve _) _ _) =
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
      let -- This local mkPkgKey is specific to legacy formats (v5, v6)
          maybeNonRegistrySrcPackage = Map.lookup depVersion (packages lockFile)
          maybeRegistrySrcPackage = Map.lookup (mkPkgKey lockFile depName depVersion) (packages lockFile)
       in case (maybeNonRegistrySrcPackage, maybeRegistrySrcPackage) of
            (Nothing, Nothing) -> Nothing
            (Just nonRegistryPkg, _) -> Just $ toDependency depName Nothing nonRegistryPkg
            (_, Just registryPkg) -> Just $ toDependency depName (Just depVersion) registryPkg

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
  , dev :: Bool
  -- TODO: Consider adding snapshotTransitivePeerDependencies if needed later
  }
  deriving (Show, Eq, Ord)

instance FromJSON SnapshotPackageData where
  parseJSON = Yaml.withObject "SnapshotPackageData" $ \obj ->
    SnapshotPackageData
      <$> obj .:? "dependencies" .!= mempty
      <*> obj .:? "optionalDependencies" .!= mempty
      <*> obj .:? "dev" .!= False

-- | This is the new graph builder for PNPM v9+ lockfiles that primarily uses the `snapshots` section.
buildGraphWithSnapshots ::
  forall sig m.
  (Has Logger sig m, Has Diagnostics sig m) =>
  PnpmLockfile ->
  m (Graphing Dependency)
buildGraphWithSnapshots lockFile = evalGrapher $ do
  let snapshotsMap = snapshots lockFile

  let processSnapshotTree :: Text -> Bool -> Maybe Dependency -> Set.Set Text -> GrapherC Dependency m ()
      processSnapshotTree snapKey parentIsDev maybeParentDep visited =
        unless (Set.member snapKey visited) $
          case Map.lookup snapKey snapshotsMap of
            Just snapshot ->
              let visited' = Set.insert snapKey visited
               in case parseSnapshotKey snapKey of
                    Just (name, version) ->
                      let thisDep = createDepSimple NodeJSType name (Just version) parentIsDev
                       in do
                            deep thisDep
                            for_ maybeParentDep $ \parentDep -> edge parentDep thisDep
                            let isDev = Set.member EnvDevelopment (dependencyEnvironments thisDep)
                            let allDeps = Map.toList (snapshotDependencies snapshot) ++ Map.toList (snapshotOptionalDependencies snapshot)
                            for_ allDeps $ \(childName, childVersion) ->
                              let childSnapKey = childName <> "@" <> childVersion
                               in processSnapshotTree childSnapKey isDev (Just thisDep) visited'
                    Nothing -> logWarn $ pretty ("[PNPM v9] Failed to parse snapshot key: " <> snapKey)
            Nothing -> logWarn $ pretty ("[PNPM v9] Snapshot missing for: " <> snapKey)

  -- Process importers for direct edges
  let processImporters :: Map Text ProjectMap -> GrapherC Dependency m ()
      processImporters impMap = for_ (Map.toList impMap) $ \(importerPath, importerData) ->
        context ("importer " <> importerPath) $ do
          let processEntries entries isDevFlag =
                for_ entries $ \(depName, ProjectMapDepMetadata{version = resolvedRefStr}) ->
                  let ref = Text.strip resolvedRefStr
                   in case parseSnapshotKey (depName <> "@" <> ref) of
                        Just (pkgName, pkgVersion) ->
                          let depNode = createDepSimple NodeJSType pkgName (Just pkgVersion) isDevFlag
                           in direct depNode
                        Nothing -> logWarn $ pretty ("[PNPM v9] Failed to parse importer dep: " <> depName <> "@" <> ref)
          processEntries (Map.toList $ directDependencies importerData) False
          processEntries (Map.toList $ directDevDependencies importerData) True

  processImporters (importers lockFile)

  -- Recursively walk all snapshots for transitive dependencies
  for_ (Map.keys snapshotsMap) $ \snapKey ->
    processSnapshotTree snapKey False Nothing Set.empty

-- | Creates a simple Dependency record.
createDepSimple :: DepType -> Text -> Maybe Text -> Bool -> Dependency
createDepSimple depType name versionM isDev =
  let cleanedVersionM = withoutPeerDepSuffix <$> versionM
   in Dependency depType name (CEq <$> cleanedVersionM) mempty (toEnv isDev) mempty
