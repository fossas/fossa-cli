module Strategy.Node.Pnpm.PnpmLock (
  -- | Analyzes a pnpm lockfile and returns a graph of dependencies.
  analyze,

  -- * for testing

  -- | Builds a graph of dependencies from a pnpm lockfile.
  buildGraph,
) where

import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, Has, context)
import Control.Monad (guard)
import Data.Aeson.Extra (TextLike (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Foldable (for_)
import Data.Map (Map)
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
  | PnpmLockV789 Text
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
        (Just '9') -> pure PnpmLockV9
        (Just _) -> pure $ PnpmLockV789 ver
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
analyze file = context "Analyzing Pnpm Lockfile" $ do
  pnpmLockFile <- context "Parsing pnpm-lock file" $ readContentsYaml file

  case lockFileVersion pnpmLockFile of
    PnpmLockLt4 raw -> logWarn . pretty $ "pnpm-lock file is using older lockFileVersion: " <> raw <> ", which is not officially supported!"
    PnpmLockV789 raw -> logWarn . pretty $ "pnpm-lock file is using version: " <> raw <> ", support may be limited."
    _ -> pure ()

  context "Building dependency graph" $ pure $ buildGraph pnpmLockFile

buildGraph :: PnpmLockfile -> Graphing Dependency
buildGraph lockFile = withoutLocalPackages $
  run . evalGrapher $ do
    -- Build a catalog version map for version resolution
    let catalogVersionMap = buildCatalogVersionMap lockFile

    -- Process direct dependencies from importers
    for_ (Map.toList $ importers lockFile) $ \(_, projectSnapshot) -> do
      -- Track dev dependencies from importers
      let devDeps = Set.fromList $ map fst $ Map.toList (directDevDependencies projectSnapshot)
      let allDirectDependencies =
            Map.toList (directDependencies projectSnapshot)
              <> Map.toList (directDevDependencies projectSnapshot)

      -- Add direct dependencies to the graph
      for_ allDirectDependencies $ \(depName, ProjectMapDepMetadata depVersion) ->
        maybe (pure ()) direct $
          resolveDepFromImporter catalogVersionMap depName depVersion (Set.member depName devDeps)

    -- Process all packages to add edges between dependencies
    for_ (Map.toList $ packages lockFile) $ \(pkgKey, pkgMeta) -> do
      -- Get package name and version from the key
      let pkgNameAndVersion = case getPkgNameVersion pkgKey of
            Nothing -> (pkgKey, Nothing)
            Just (name, version) -> (name, Just version)
      let (depName, depVersionMaybe) = pkgNameAndVersion
      let parentDep = resolveDependency catalogVersionMap depName depVersionMaybe pkgMeta False

      -- Add the package as a "deep" dependency (transitive)
      deep parentDep

      -- Add edges to its dependencies
      let pkgDependencies = Map.toList (dependencies pkgMeta) <> Map.toList (peerDependencies pkgMeta)
      for_ pkgDependencies $ \(childName, childVersion) ->
        maybe
          (pure ())
          (edge parentDep)
          (resolveDepFromPackage catalogVersionMap childName childVersion False)
  where
    -- Build a map of package names to their resolved versions from the catalogs section
    buildCatalogVersionMap :: PnpmLockfile -> Map Text Text
    buildCatalogVersionMap pnpmLockFile =
      let defaultCatalog = Map.findWithDefault 
                             Map.empty 
                             "default" 
                             (Map.map catalogEntriesMap (catalogs pnpmLockFile))
      in Map.map cleanupVersion defaultCatalog
      where
        catalogEntriesMap :: CatalogMap -> Map Text Text
        catalogEntriesMap = Map.map catalogVersion . catalogEntries

    -- Resolve a dependency from the importers section
    resolveDepFromImporter :: Map Text Text -> Text -> Text -> Bool -> Maybe Dependency
    resolveDepFromImporter catalogMap depName depVersion isDev = do
      -- Skip any workspace links or dependencies with link: prefix
      if "link:" `Text.isPrefixOf` depVersion
        then Nothing  -- Skip workspace links
        else do
          let isCatalogRef = "catalog:" `Text.isPrefixOf` depVersion
          let cleanVersion = cleanupVersion depVersion
          
          -- Handle catalog references
          let resolvedVersion = 
                if isCatalogRef
                  then Map.lookup depName catalogMap
                  else Just cleanVersion
          
          case resolvedVersion of
            Nothing -> Nothing
            Just ver -> 
              -- Don't try to lookup empty versions (like those from links)
              if Text.null ver 
                then Nothing
                else do
                  let pkgKey = mkPkgKey depName ver
                  let maybePackage = Map.lookup pkgKey (packages lockFile)
                  pure $ case maybePackage of
                      Nothing -> createDep NodeJSType depName (Just ver) isDev
                      Just pkg -> resolveDependency catalogMap depName (Just ver) pkg isDev

    -- Resolve a dependency from the packages section (for edges)
    resolveDepFromPackage :: Map Text Text -> Text -> Text -> Bool -> Maybe Dependency
    resolveDepFromPackage catalogMap depName depVersion isDev = do
      -- Skip link: dependencies
      if "link:" `Text.isPrefixOf` depVersion
        then Nothing
        else do
          -- Handle special case where the "version" might actually be a separate package
          -- This happens with dependencies like "safe-execa@0.1.2" where the value should be
          -- treated as a separate package name + version, not a version of the parent package
          let (actualDepName, actualVersion) = 
                if "@" `Text.isInfixOf` depVersion && not ("/" `Text.isInfixOf` depVersion)
                  then 
                    case Text.splitOn "@" depVersion of
                      [name, ver] -> (name, ver)  -- Format: "safe-execa@0.1.2"
                      _ -> (depName, cleanupVersion depVersion)
                  else (depName, cleanupVersion depVersion)
          
          -- Skip empty versions (like those from links)
          if Text.null actualVersion
            then Nothing
            else do
              -- Try to find the package in packages section
              let pkgKey = mkPkgKey actualDepName actualVersion
              let maybePackage = Map.lookup pkgKey (packages lockFile)
              
              -- Either use the package if we found it, or create a simple dependency
              pure $ case maybePackage of
                Nothing -> createDep NodeJSType actualDepName (Just actualVersion) isDev
                Just pkg -> resolveDependency catalogMap actualDepName (Just actualVersion) pkg isDev

    -- Create a dependency object based on package type
    resolveDependency :: Map Text Text -> Text -> Maybe Text -> PackageData -> Bool -> Dependency
    resolveDependency _ _ _ (PackageData pkgIsDev _ (GitResolve (GitResolution url rev)) _ _) isDev =
      createDep GitType url (Just rev) (pkgIsDev || isDev)
    resolveDependency _ _ _ (PackageData pkgIsDev _ (TarballResolve (TarballResolution url)) _ _) isDev =
      createDep URLType url Nothing (pkgIsDev || isDev)
    resolveDependency _ _ _ (PackageData pkgIsDev (Just name) (DirectoryResolve _) _ _) isDev =
      createDep UserType name Nothing (pkgIsDev || isDev)
    resolveDependency _ name _ (PackageData pkgIsDev Nothing (DirectoryResolve _) _ _) isDev =
      createDep UserType name Nothing (pkgIsDev || isDev)
    resolveDependency catalogMap name maybeVersion (PackageData pkgIsDev _ (RegistryResolve _) _ _) isDev =
      -- For registry packages, use the provided version or try to resolve from catalogs
      let resolvedVersion = maybeVersion >>= resolveVersionFromCatalog catalogMap name
       in createDep NodeJSType name resolvedVersion (pkgIsDev || isDev)

    -- Helper to resolve a version from the catalog if needed
    resolveVersionFromCatalog :: Map Text Text -> Text -> Text -> Maybe Text
    resolveVersionFromCatalog catalogMap name version =
      let isCatalogRef = "catalog:" `Text.isPrefixOf` version
          cleanVersion = 
            if isCatalogRef 
              then Text.drop (Text.length "catalog:") version 
              else version
       in if isCatalogRef
          then Map.lookup name catalogMap
          else Just $ cleanupVersion cleanVersion

    -- Helper to create a dependency object
    createDep :: DepType -> Text -> Maybe Text -> Bool -> Dependency
    createDep depType name version isDev =
      Dependency depType name (CEq <$> version) mempty (toEnv isDev) mempty

    -- Create environment tags
    toEnv :: Bool -> Set.Set DepEnvironment
    toEnv isDev = Set.singleton $ if isDev then EnvDevelopment else EnvProduction

    -- Remove local packages from the graph
    withoutLocalPackages :: Graphing Dependency -> Graphing Dependency
    withoutLocalPackages = shrink (\dep -> dependencyType dep /= UserType)

    -- Clean up version strings by removing peer deps suffix and symlinked constraints
    cleanupVersion :: Text -> Text
    cleanupVersion = removeLinks . removePrefixes . withoutPeerDepSuffix . withoutSymConstraint
      where
        withoutSymConstraint :: Text -> Text
        withoutSymConstraint = fst . Text.breakOn "_"
        
        withoutPeerDepSuffix :: Text -> Text
        withoutPeerDepSuffix = fst . Text.breakOn "("
        
        -- Remove any namespace prefixes from version strings (e.g., @pnpm/hosted-git-info@1.0.0 -> 1.0.0)
        removePrefixes :: Text -> Text
        removePrefixes version
          | "@" `Text.isInfixOf` version && Text.count "@" version > 1 = 
              -- For scoped packages with a version, extract just the version part
              let parts = Text.splitOn "@" version
               in if length parts >= 3
                    then last parts  -- Take just the version number at the end
                    else version
          | otherwise = version
        
        -- Handle link: prefixes by removing them entirely (links are handled separately)
        removeLinks :: Text -> Text
        removeLinks version
          | "link:" `Text.isPrefixOf` version = ""
          | otherwise = version

    -- Parse package key into name and version based on lockfile version
    getPkgNameVersion :: Text -> Maybe (Text, Text)
    getPkgNameVersion pkgKey = case lockFileVersion lockFile of
      PnpmLock4Or5 -> parseSlashFormat pkgKey
      PnpmLock6 -> parseAtFormat pkgKey
      PnpmLockLt4 _ -> parseSlashFormat pkgKey
      PnpmLockV789 _ -> parseAtFormat pkgKey
      PnpmLockV9 -> parseAtFormat pkgKey <|> parseSlashFormat pkgKey -- Try both formats for v9
      where
        parseSlashFormat :: Text -> Maybe (Text, Text)
        parseSlashFormat key = do
          let parts = Text.splitOn "/" key
          guard $ length parts >= 3
          let name = parts !! 1
          let version = parts !! 2
          pure (name, version)

        parseAtFormat :: Text -> Maybe (Text, Text)
        parseAtFormat key = do
          let trimmedKey = Text.dropWhile (== '/') key
          
          -- Handle scoped packages first (those starting with @)
          if "@" `Text.isPrefixOf` trimmedKey && Text.count "@" trimmedKey >= 2
            then do
              -- For scoped packages like '@user/pkg@1.0.0'
              let scopeEndPos = textIndexOf (Text.drop 1 trimmedKey) "@"
              guard $ scopeEndPos > 0
              let fullPos = scopeEndPos + 1 -- +1 because we dropped the first char
              let (nameWithScope, versionWithExtra) = Text.splitAt fullPos trimmedKey
              let version = Text.drop 1 versionWithExtra -- drop the @ symbol
              pure (nameWithScope, cleanupVersion version)
            else do
              -- Regular packages like 'safe-execa@0.1.2'
              let parts = Text.splitOn "@" trimmedKey
              guard $ length parts >= 2
              
              -- Get the complete name (everything before the last @)
              let name = Text.intercalate "@" (init parts)
              -- Get just the version (everything after the last @)
              let version = last parts
              pure (name, cleanupVersion version)

    -- Helper for finding the position of a substring in text
    textIndexOf :: Text -> Text -> Int
    textIndexOf haystack needle = 
      case Text.breakOn needle haystack of
        (prefix, suffix) -> 
          if Text.null suffix 
            then -1 
            else Text.length prefix

    -- Create a package key based on name and version according to lockfile version
    mkPkgKey :: Text -> Text -> Text
    mkPkgKey name version = case lockFileVersion lockFile of
      PnpmLock4Or5 -> mkPkgKeyFormat name version "slash"
      PnpmLock6 -> mkPkgKeyFormat name version "at"
      PnpmLockLt4 _ -> mkPkgKeyFormat name version "slash"
      PnpmLockV789 _ -> mkPkgKeyFormat name version "at"
      PnpmLockV9 ->
        -- For v9, try both formats - first check if exists with slash format
        let keyWithSlash = mkPkgKeyFormat name version "slash"
            keyWithAt = mkPkgKeyFormat name version "at"
            -- Also try without leading slash for v9
            keyWithoutSlash = name <> "@" <> version
         in fromMaybe keyWithAt $
              findFirst (`Map.member` packages lockFile) [keyWithSlash, keyWithAt, keyWithoutSlash]
      where
        findFirst :: (a -> Bool) -> [a] -> Maybe a
        findFirst _ [] = Nothing
        findFirst p (x : xs) = if p x then Just x else findFirst p xs

        mkPkgKeyFormat :: Text -> Text -> Text -> Text
        mkPkgKeyFormat nm ver format = case format of
          "slash" -> "/" <> nm <> "/" <> ver
          "at" ->
            if not (Text.null nm) && Text.head nm == '@'
              then "/" <> nm <> "@" <> ver -- Scoped package
              else "/" <> nm <> "@" <> ver -- Regular package
          _ -> "/" <> nm <> "@" <> ver -- Default to at format
