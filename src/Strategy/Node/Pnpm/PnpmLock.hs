module Strategy.Node.Pnpm.PnpmLock (
  -- | Analyzes a pnpm lockfile and returns a graph of dependencies.
  analyze,

  -- * for testing

  -- | Builds a graph of dependencies from a pnpm lockfile.
  buildGraph,
) where

import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, Has, context)
import Control.Monad (when, guard)
import Data.Aeson.Extra (TextLike (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Foldable (for_)
import Data.Map (Map, toList)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set qualified as Set
import Data.String.Conversion (toString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Yaml (FromJSON, Object, Parser, (.!=), (.:), (.:?))
import Data.Yaml qualified as Yaml
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (GitType, NodeJSType, URLType, UserType),
  Dependency (Dependency, dependencyType),
  VerConstraint (CEq),
 )
import Effect.Grapher (deep, direct, edge, evalGrapher, run)
import Effect.Logger (
  Logger,
  logWarn,
  pretty,
 )
import Effect.ReadFS (ReadFS, readContentsYaml)
import Graphing (Graphing, shrink)
import Path (Abs, File, Path)

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
  , catalogs :: Map Text CatalogMap
  , lockFileVersion :: PnpmLockFileVersion
  }
  deriving (Show, Eq, Ord)

data PnpmLockFileVersion
  = PnpmLockLt4 Text
  | PnpmLock4Or5
  | PnpmLock6
  | PnpmLockV678 Text
  | PnpmLockV9
  deriving (Show, Eq, Ord)

instance FromJSON PnpmLockfile where
  parseJSON = Yaml.withObject "pnpm-lock content" $ \obj -> do
    rawLockFileVersion <- getVersion =<< obj .:? "lockfileVersion" .!= (TextLike mempty)
    importers <- obj .:? "importers" .!= mempty
    packages <- obj .:? "packages" .!= mempty
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

    pure $ PnpmLockfile refinedImporters packages catalogs rawLockFileVersion
    where
      getVersion (TextLike ver) = case (listToMaybe . toString $ ver) of
        (Just '1') -> pure $ PnpmLockLt4 ver
        (Just '2') -> pure $ PnpmLockLt4 ver
        (Just '3') -> pure $ PnpmLockLt4 ver
        (Just '4') -> pure PnpmLock4Or5
        (Just '5') -> pure PnpmLock4Or5
        (Just '6') -> pure PnpmLock6
        (Just x) | x `elem` ['7', '8'] -> pure $ PnpmLockV678 ver
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
  { depVersion :: Text
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
analyze file = context "Analyzing Npm Lockfile (v3)" $ do
  pnpmLockFile <- context "Parsing pnpm-lock file" $ readContentsYaml file

  case lockFileVersion pnpmLockFile of
    PnpmLockLt4 raw -> logWarn . pretty $ "pnpm-lock file is using older lockFileVersion: " <> raw <> " of, which is not officially supported!"
    PnpmLockV678 raw -> logWarn . pretty $ "pnpm-lock file is using newer lockFileVersion: " <> raw <> " of, which is not officially supported!"
    _ -> pure ()

  context "Building dependency graph" $ pure $ buildGraph pnpmLockFile

buildGraph :: PnpmLockfile -> Graphing Dependency
buildGraph lockFile = withoutLocalPackages $
  run . evalGrapher $ do
    -- First, build a map of all catalog references to their actual versions
    let catalogVersionMap = buildCatalogVersionMap lockFile
    -- Process direct dependencies from importers
    for_ (toList $ importers lockFile) $ \(_, projectSnapshot) -> do
      -- Track which packages are dev dependencies from the importers section
      let devDeps = Set.fromList $ map fst $ toList (directDevDependencies projectSnapshot)
      let allDirectDependencies =
            toList (directDependencies projectSnapshot)
              <> toList (directDevDependencies projectSnapshot)

      -- Process direct dependencies
      for_ allDirectDependencies $ \(depName, (ProjectMapDepMetadata depVersion)) ->
        maybe (pure ()) direct $
          toResolvedDependency catalogVersionMap depName depVersion (Set.member depName devDeps)

    -- Add edges and deep dependencies by iterating over all packages.
    for_ (toList $ packages lockFile) $ \(pkgKey, pkgMeta) -> do
      let deepDependencies = toList $ dependencies pkgMeta
      let (depName, depVersion) = case getPkgNameVersion pkgKey of
            Nothing -> (pkgKey, Nothing)
            Just (name, version) -> (name, Just version)
      let parentDep = toDependency catalogVersionMap depName depVersion pkgMeta False -- Not a direct dependency, use package's isDev

      -- Only add deep dependency if it hasn't been added as a direct dependency
      -- This prevents duplicates in the graph
      deep parentDep

      for_ deepDependencies $ \(deepName, deepVersion) ->
        maybe (pure ()) (edge parentDep) (toResolvedDependency catalogVersionMap deepName deepVersion False)
  where
    -- Get the actual version for a package, checking catalogs if needed
    getPackageVersion :: CatalogMap -> Text -> Text -> Maybe Text
    getPackageVersion catalog name version =
      let cleanVersion = withoutPeerDepSuffix $ withoutSymConstraint version
      in case Map.lookup (name <> "/" <> cleanVersion) (catalogEntries catalog) of
        Just entry -> Just $ catalogVersion entry
        Nothing -> Just cleanVersion

    toResolvedDependency :: Map Text Text -> Text -> Text -> Bool -> Maybe Dependency
    toResolvedDependency catalogMap depName depVersion isImporterDevDep = do
      -- First try to resolve through catalog map if it's a catalog reference
      let resolvedVersion =
            if Text.pack "catalog:" `Text.isPrefixOf` depVersion || depVersion == Text.pack "catalog:"
              then Map.lookup depName catalogMap
              else Just depVersion

      case resolvedVersion of
        Just ver -> do
          -- Try to find the package in the packages section
          let maybeNonRegistrySrcPackage = Map.lookup (withoutPeerDepSuffix . withoutSymConstraint $ depVersion) (packages lockFile)
          let maybeRegistrySrcPackage =
                let key = mkPkgKey depName (withoutPeerDepSuffix . withoutSymConstraint $ ver)
                 in Map.lookup key (packages lockFile)

          -- If we have both a non-registry and registry package, prefer the registry one
          -- to avoid duplicates
          case (maybeNonRegistrySrcPackage, maybeRegistrySrcPackage) of
            (Nothing, Nothing) ->
              -- If we can't find the package in the packages section, create a dependency
              -- with the version we have
              Just $ toDep NodeJSType depName (Just $ withoutPeerDepSuffix . withoutSymConstraint $ ver) isImporterDevDep
            (Just nonRegistryPkg, Nothing) -> Just $ toDependency catalogMap depName Nothing nonRegistryPkg isImporterDevDep
            (_, Just registryPkg) -> Just $ toDependency catalogMap depName (Just $ withoutPeerDepSuffix . withoutSymConstraint $ ver) registryPkg isImporterDevDep
        Nothing -> Nothing -- Skip if we can't resolve the version at all

    -- Build a map of package names to their actual versions from the catalogs section
    buildCatalogVersionMap :: PnpmLockfile -> Map Text Text
    buildCatalogVersionMap pnpmLockFile =
      case Map.lookup (Text.pack "default") (catalogs pnpmLockFile) of
        Nothing -> Map.empty
        Just defaultCatalog ->
          Map.map catalogVersion (catalogEntries defaultCatalog)

    toDep :: DepType -> Text -> Maybe Text -> Bool -> Dependency
    toDep depType name version isDev = Dependency depType name (CEq <$> version) mempty (toEnv isDev) mempty

    toEnv :: Bool -> Set.Set DepEnvironment
    toEnv isNotRequired = Set.singleton $ if isNotRequired then EnvDevelopment else EnvProduction

    withoutLocalPackages :: Graphing Dependency -> Graphing Dependency
    withoutLocalPackages = Graphing.shrink (\dep -> dependencyType dep /= UserType)

    toDependency :: Map Text Text -> Text -> Maybe Text -> PackageData -> Bool -> Dependency
    toDependency catalogMap name maybeVersion (PackageData isDev _ (RegistryResolve _) _ _) isImporterDevDep =
      -- For registry packages, resolve any catalog/workspace references first
      let resolvedVersion = case maybeVersion of
            Nothing -> Nothing
            Just ver -> 
              let catalogEntries = Map.mapWithKey (\k v -> CatalogEntry v v) catalogMap
                  catalog = CatalogMap catalogEntries
              in case getPackageVersion catalog name ver of
                Just resolved -> Just $ withoutPeerDepSuffix . withoutSymConstraint $ resolved
                Nothing -> Just $ withoutPeerDepSuffix . withoutSymConstraint $ ver
       in toDep NodeJSType name resolvedVersion (isDev || isImporterDevDep)
    toDependency _ _ _ (PackageData isDev _ (GitResolve (GitResolution url rev)) _ _) isImporterDevDep =
      toDep GitType url (Just rev) (isDev || isImporterDevDep)
    toDependency _ _ _ (PackageData isDev _ (TarballResolve (TarballResolution url)) _ _) isImporterDevDep =
      toDep URLType url Nothing (isDev || isImporterDevDep)
    toDependency _ _ _ (PackageData isDev (Just name) (DirectoryResolve _) _ _) isImporterDevDep =
      toDep UserType name Nothing (isDev || isImporterDevDep)
    toDependency _ name _ (PackageData isDev Nothing (DirectoryResolve _) _ _) isImporterDevDep =
      toDep UserType name Nothing (isDev || isImporterDevDep)

    -- Sometimes package versions include symlinked paths
    -- of sibling dependencies used for resolution.
    --
    -- >> withoutSymConstraint "1.2.0" = "1.2.0"
    -- >> withoutSymConstraint "1.2.0_vue@3.0" = "1.2.0"
    withoutSymConstraint :: Text -> Text
    withoutSymConstraint version = fst $ Text.breakOn "_" version

    -- Sometimes package versions include resolved peer dependency version
    -- in parentheses. This is used by pnpm for dependency resolution, we do
    -- not care about them, as they do not represent package version.
    --
    -- >> withoutPeerDepSuffix "1.2.0" = "1.2.0"
    -- >> withoutPeerDepSuffix "1.2.0(babel@1.0.0)" = "1.2.0"
    withoutPeerDepSuffix :: Text -> Text
    withoutPeerDepSuffix version = fst $ Text.breakOn "(" version

    -- Parse a package key into its name and version components
    getPkgNameVersion :: Text -> Maybe (Text, Text)
    getPkgNameVersion pkgKey = case lockFileVersion lockFile of
      PnpmLock4Or5 -> parseSlashFormat pkgKey
      PnpmLock6 -> parseAtFormat pkgKey
      PnpmLockLt4 _ -> parseSlashFormat pkgKey
      PnpmLockV678 _ -> parseAtFormat pkgKey
      PnpmLockV9 -> parseAtFormat pkgKey <|> parseSlashFormat pkgKey
      where
        parseSlashFormat key = do
          let parts = Text.splitOn "/" key
          guard $ length parts >= 3
          let name = parts !! 1
          let version = parts !! 2
          pure (name, version)
        parseAtFormat key = do
          let parts = Text.splitOn "@" key
          guard $ length parts >= 2
          let name = Text.dropWhile (== '/') $ parts !! 0
          let version = fst $ Text.breakOn "(" $ parts !! 1
          pure (name, version)

    mkPkgKey :: Text -> Text -> Text
    mkPkgKey name version = case lockFileVersion lockFile of
      PnpmLock4Or5 -> "/" <> name <> "/" <> version
      PnpmLock6 -> "/" <> name <> "@" <> version
      PnpmLockLt4 _ -> "/" <> name <> "/" <> version
      PnpmLockV678 _ -> "/" <> name <> "@" <> version
      -- For PNPM v9, we need to handle both formats: with and without leading slash
      PnpmLockV9 ->
        let keyWithoutSlash = name <> "@" <> version
            keyWithSlash = "/" <> keyWithoutSlash
         in if Map.member keyWithSlash (packages lockFile)
              then keyWithSlash
              else keyWithoutSlash -- Default to key without slash for PNPM v9
