module Strategy.Node.Pnpm.PnpmLock (
  -- | Analyzes a pnpm lockfile and returns a graph of dependencies.
  analyze,

  -- * for testing

  -- | Builds a graph of dependencies from a pnpm lockfile.
  buildGraph,
) where

import Control.Applicative ((<|>))
import Control.Carrier.Simple (runSimpleC)
import Control.Effect.Diagnostics (Diagnostics, Has, context)
import Control.Monad (guard, when)
import Data.Aeson.Extra (TextLike (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (for_)
import Data.Functor.Identity (runIdentity)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as Set
import Data.String.Conversion (toString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Yaml (FromJSON, Object, Parser, (.!=), (.:), (.:?))
import qualified Data.Yaml as Yaml
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (GitType, NodeJSType, URLType, UserType),
  Dependency (Dependency, dependencyType),
  VerConstraint (CEq),
 )
import Effect.Grapher (deep, direct, edge, evalGrapher, run)
import Effect.Logger (
  Logger,
  ignoreLogger,
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
  , snapshots :: Map Text SnapshotPackageData
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

analyze :: (Has ReadFS sig m, Has Logger sig m, Has Diagnostics sig m, Ord Dependency) => Path Abs File -> m (Graphing Dependency)
analyze file = context "Analyzing Pnpm Lockfile" $ do
  pnpmLockFile <- context "Parsing pnpm-lock file" $ readContentsYaml file

  case lockFileVersion pnpmLockFile of
    PnpmLockLt4 raw -> logWarn . pretty $ "pnpm-lock file is using older lockFileVersion: " <> raw <> ", which is not officially supported!"
    PnpmLockV789 raw -> logWarn . pretty $ "pnpm-lock file is using version: " <> raw <> ", support may be limited."
    _ -> pure ()

  context "Building dependency graph" $ buildGraph pnpmLockFile

-- Moved mkPkgKey to be a top-level helper function
mkPkgKey :: PnpmLockfile -> Text -> Text -> Text
mkPkgKey lf name version = case lockFileVersion lf of
  PnpmLock4Or5 -> mkPkgKeyFormat name version "slash"
  PnpmLock6 -> mkPkgKeyFormat name version "at"
  PnpmLockLt4 _ -> mkPkgKeyFormat name version "slash"
  PnpmLockV789 _ -> mkPkgKeyFormat name version "at"
  PnpmLockV9 ->
    let keyWithSlash = mkPkgKeyFormat name version "slash"
        keyWithAt = mkPkgKeyFormat name version "at"
        keyWithoutSlash = name <> "@" <> version
        keyWithoutLeadingSlash = Text.dropWhile (== '/') keyWithAt
        keyWithoutLeadingSlashSlash = Text.dropWhile (== '/') keyWithSlash
     in -- For v9, try all possible key formats since the format can vary
        -- between packages and snapshots
        fromMaybe keyWithAt $ -- Default to keyWithAt if others not found
          findFirst
            (`Map.member` packages lf)
            [ keyWithSlash
            , keyWithAt
            , keyWithoutSlash
            , keyWithoutLeadingSlash
            , keyWithoutLeadingSlashSlash
            ]
  where
    findFirst :: (a -> Bool) -> [a] -> Maybe a
    findFirst _ [] = Nothing
    findFirst p (x : xs') = if p x then Just x else findFirst p xs'

    mkPkgKeyFormat :: Text -> Text -> Text -> Text
    mkPkgKeyFormat nm ver format = case format of
      "slash" -> "/" <> nm <> "/" <> ver
      "at" -> "/" <> nm <> "@" <> ver
      _ -> "/" <> nm <> "@" <> ver -- Default to 'at' format

-- Renamed original buildGraph to buildGraphLegacy
buildGraphLegacy :: PnpmLockfile -> Graphing Dependency
buildGraphLegacy lockFile = withoutLocalPackages $
  run . evalGrapher $ do
    -- Define helpers using let bindings
    let catalogVersionMap = buildCatalogVersionMap lockFile

        buildCatalogVersionMap :: PnpmLockfile -> Map Text Text
        buildCatalogVersionMap lf =
          let defaultCatalog =
                Map.findWithDefault
                  Map.empty
                  "default"
                  (Map.map catalogEntriesMap (catalogs lf))
           in Map.map (cleanupVersion lf) defaultCatalog
          where
            catalogEntriesMap :: CatalogMap -> Map Text Text
            catalogEntriesMap = Map.map catalogVersion . catalogEntries

        cleanupVersion :: PnpmLockfile -> Text -> Text
        cleanupVersion lf = removeLinks . removePrefixes . withoutPeerDepSuffix . withoutWorkspacePrefix . (if shouldApplySymConstraint lf then withoutSymConstraint else id)
          where
            shouldApplySymConstraint :: PnpmLockfile -> Bool
            shouldApplySymConstraint lock = case lockFileVersion lock of
              PnpmLockV9 -> False
              _ -> True
            withoutSymConstraint :: Text -> Text
            withoutSymConstraint = fst . Text.breakOn "_"

            withoutPeerDepSuffix :: Text -> Text
            withoutPeerDepSuffix = fst . Text.breakOn "("

            withoutWorkspacePrefix :: Text -> Text
            withoutWorkspacePrefix version
              | "workspace:" `Text.isPrefixOf` version = ""
              | otherwise = version

            removePrefixes :: Text -> Text
            removePrefixes version
              | "@" `Text.isInfixOf` version && Text.count "@" version > 1 =
                  let parts = Text.splitOn "@" version
                   in if length parts >= 3
                        then fromMaybe version (listToMaybe (reverse parts))
                        else version
              | otherwise = version

            removeLinks :: Text -> Text
            removeLinks version
              | "link:" `Text.isPrefixOf` version = ""
              | otherwise = version

        resolveVersionFromCatalog :: Map Text Text -> Text -> (Text -> Text) -> Text -> Maybe Text
        resolveVersionFromCatalog catMap name cleanupFunc version =
          let isCatalogRef = "catalog:" `Text.isPrefixOf` version
              cleanVer =
                if isCatalogRef
                  then Text.drop (Text.length "catalog:") version
                  else version
           in if isCatalogRef
                then Map.lookup name catMap
                else Just $ cleanupFunc cleanVer

        createDep :: DepType -> Text -> Maybe Text -> Bool -> Dependency
        createDep depType name version isDepGraphDev =
          Dependency depType name (CEq <$> version) mempty (toEnv isDepGraphDev) mempty

        toEnv :: Bool -> Set.Set DepEnvironment
        toEnv isDepGraphDev = Set.singleton $ if isDepGraphDev then EnvDevelopment else EnvProduction

        resolveDependency :: Map Text Text -> Text -> Maybe Text -> PackageData -> Bool -> Dependency
        resolveDependency _ _ _ (PackageData pkgIsDev _ (GitResolve (GitResolution url rev)) _ _) contextIsDev =
          createDep GitType url (Just rev) (pkgIsDev || contextIsDev)
        resolveDependency _ _ _ (PackageData pkgIsDev _ (TarballResolve (TarballResolution url)) _ _) contextIsDev =
          createDep URLType url Nothing (pkgIsDev || contextIsDev)
        resolveDependency _ _ _ (PackageData pkgIsDev (Just name') (DirectoryResolve _) _ _) contextIsDev =
          createDep UserType name' Nothing (pkgIsDev || contextIsDev)
        resolveDependency _ name' _ (PackageData pkgIsDev Nothing (DirectoryResolve _) _ _) contextIsDev =
          createDep UserType name' Nothing (pkgIsDev || contextIsDev)
        resolveDependency catMap name' maybeVersion (PackageData pkgIsDev _ (RegistryResolve _) _ _) contextIsDev =
          let resolvedVersion = maybeVersion >>= resolveVersionFromCatalog catMap name' (cleanupVersion lockFile)
           in createDep NodeJSType name' resolvedVersion (pkgIsDev || contextIsDev)

        resolveDepFromImporter :: Map Text Text -> PnpmLockfile -> Text -> Text -> Bool -> Maybe Dependency
        resolveDepFromImporter catMap lf depName depVersion isImporterDev = do
          if "link:" `Text.isPrefixOf` depVersion
            then Nothing
            else do
              let isCatalogRef = "catalog:" `Text.isPrefixOf` depVersion
              let cleanVer = cleanupVersion lf depVersion

              let resolvedVersion =
                    if isCatalogRef
                      then Map.lookup depName catMap
                      else Just cleanVer

              case resolvedVersion of
                Nothing -> Nothing
                Just ver ->
                  if Text.null ver
                    then Nothing
                    else do
                      let pk = mkPkgKey lf depName ver
                      let maybePackage = Map.lookup pk (packages lf)
                      pure $ case maybePackage of
                        Nothing -> createDep NodeJSType depName (Just ver) isImporterDev
                        Just pkg -> resolveDependency catMap depName (Just ver) pkg isImporterDev

        resolveDepFromPackage :: Map Text Text -> PnpmLockfile -> Text -> Text -> Bool -> Maybe Dependency
        resolveDepFromPackage catMap lf depName depVersion parentIsDev = do
          if "link:" `Text.isPrefixOf` depVersion
            then Nothing
            else do
              let (actualDepName, actualVersion) =
                    if "@" `Text.isInfixOf` depVersion && not ("/" `Text.isInfixOf` depVersion)
                      then case Text.splitOn "@" depVersion of
                        [name', ver] -> (name', ver)
                        _ -> (depName, cleanupVersion lf depVersion)
                      else (depName, cleanupVersion lf depVersion)

              if Text.null actualVersion
                then Nothing
                else do
                  let pk = mkPkgKey lf actualDepName actualVersion
                  let maybePackage = Map.lookup pk (packages lf)
                  pure $ case maybePackage of
                    Nothing -> createDep NodeJSType actualDepName (Just actualVersion) parentIsDev
                    Just pkg -> resolveDependency catMap actualDepName (Just actualVersion) pkg parentIsDev

        getPkgNameVersion :: PnpmLockfile -> Text -> Maybe (Text, Text)
        getPkgNameVersion lf pkgKey = case lockFileVersion lf of
          PnpmLock4Or5 -> parseSlashFormat pkgKey
          PnpmLock6 -> parseAtFormat lf pkgKey
          PnpmLockLt4 _ -> parseSlashFormat pkgKey
          PnpmLockV789 _ -> parseAtFormat lf pkgKey
          PnpmLockV9 -> parseAtFormat lf pkgKey <|> parseSlashFormat pkgKey
          where
            parseSlashFormat :: Text -> Maybe (Text, Text)
            parseSlashFormat key = do
              let parts = Text.splitOn "/" key
              guard $ length parts >= 3
              name' <- parts `atMay` 1
              version' <- parts `atMay` 2
              pure (name', version')
              where
                atMay :: [a] -> Int -> Maybe a
                atMay xs i = if i >= 0 && i < length xs then Just (xs !! i) else Nothing

            parseAtFormat :: PnpmLockfile -> Text -> Maybe (Text, Text)
            parseAtFormat lock key = do
              let trimmedKey = Text.dropWhile (== '/') key
              if "@" `Text.isPrefixOf` trimmedKey && Text.count "@" trimmedKey >= 2
                then do
                  let scopeEndPos = textIndexOf (Text.drop 1 trimmedKey) "@"
                  guard $ scopeEndPos > 0
                  let fullPos = scopeEndPos + 1
                  let (nameWithScope, versionWithExtra) = Text.splitAt fullPos trimmedKey
                  let version' = Text.drop 1 versionWithExtra
                  pure (nameWithScope, cleanupVersion lock version')
                else do
                  let parts = Text.splitOn "@" trimmedKey
                  guard $ length parts >= 2
                  let mName = if null parts then Nothing else safeInit parts
                  let mVersion = if null parts then Nothing else listToMaybe (reverse parts)
                  case (mName, mVersion) of
                    (Just nameParts, Just version') ->
                      pure (Text.intercalate "@" nameParts, cleanupVersion lock version')
                    _ -> Nothing
              where
                safeInit :: [a] -> Maybe [a]
                safeInit [] = Nothing
                safeInit xs = Just (init xs)

            textIndexOf :: Text -> Text -> Int
            textIndexOf haystack needle =
              case Text.breakOn needle haystack of
                (prefix, suffix) ->
                  if Text.null suffix
                    then -1
                    else Text.length prefix

        withoutLocalPackages :: Graphing Dependency -> Graphing Dependency
        withoutLocalPackages = shrink (\dep -> dependencyType dep /= UserType)

    -- Main logic of buildGraphLegacy starts here, using the let-bound helpers
    for_ (Map.toList $ importers lockFile) $ \(_, projectSnapshot) -> do
      let devDeps = Set.fromList $ map fst $ Map.toList (directDependencies projectSnapshot)
      let allDirectDependencies =
            Map.toList (directDependencies projectSnapshot)
              <> Map.toList (directDevDependencies projectSnapshot)

      for_ allDirectDependencies $ \(depName, ProjectMapDepMetadata depVersion) ->
        maybe (pure ()) direct $
          resolveDepFromImporter catalogVersionMap lockFile depName depVersion (Set.member depName devDeps)

    for_ (Map.toList $ packages lockFile) $ \(pkgKey, pkgMeta) -> do
      let pkgNameAndVersion = case getPkgNameVersion lockFile pkgKey of
            Nothing -> (pkgKey, Nothing)
            Just (name, version) -> (name, Just version)
      let (depName, depVersionMaybe) = pkgNameAndVersion
      let parentDep = resolveDependency catalogVersionMap depName depVersionMaybe pkgMeta False

      deep parentDep

      let pkgDependencies = Map.toList (dependencies pkgMeta) <> Map.toList (peerDependencies pkgMeta)
      for_ pkgDependencies $ \(childName, childVersion) ->
        maybe
          (pure ())
          (edge parentDep)
          (resolveDepFromPackage catalogVersionMap lockFile childName childVersion False)
  where
    withoutLocalPackages :: Graphing Dependency -> Graphing Dependency
    withoutLocalPackages = shrink (\dep -> dependencyType dep /= UserType)

-- Main buildGraph function that dispatches based on lockfile version
buildGraph :: (Has Logger sig m, Ord Dependency) => PnpmLockfile -> m (Graphing Dependency)
buildGraph lockFile = do
  case lockFileVersion lockFile of
    PnpmLockV9 -> buildGraphWithSnapshots lockFile
    -- For older versions, we need to lift the pure buildGraphLegacy into the effectful context
    _ -> pure $ buildGraphLegacy lockFile

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

-- Placeholder for the new v9+ graph building logic using snapshots
buildGraphWithSnapshots :: (Has Logger sig m, Ord Dependency) => PnpmLockfile -> m (Graphing Dependency)
buildGraphWithSnapshots lockFile =
  fmap withoutLocalPackagesSnapshot $
    evalGrapher $ do
      let catalogVersionMap = buildCatalogVersionMapSnapshots lockFile
      let snapshotsMap = snapshots lockFile

      -- Helper to create a Dependency from snapshot data
      let resolveSnapshotDependency :: Text -> Text -> Bool -> Maybe Dependency
          resolveSnapshotDependency canonicalDepName snapKeyOrRef isCtxDev = do
            (nameFromSnapKey, versionFromSnapKey) <- parseSnapshotKey snapKeyOrRef

            let packageKeyInPackagesMap = mkPkgKey lockFile nameFromSnapKey versionFromSnapKey
            let maybePkgData = Map.lookup packageKeyInPackagesMap (packages lockFile)

            let finalIsDev = maybe isCtxDev isDev maybePkgData
            pure $ createDepSimple NodeJSType canonicalDepName (Just versionFromSnapKey) finalIsDev

      -- Define processImporterEntries here, after resolveSnapshotDependency
      let processImporterEntries entries isDepDevFlag =
            for_ entries $ \(canonicalName, ProjectMapDepMetadata{depVersion = resolvedRefStr}) -> do
              if "catalog:" `Text.isPrefixOf` resolvedRefStr
                then do
                  let catalogAlias = Text.drop (Text.length "catalog:") resolvedRefStr
                  case Map.lookup catalogAlias catalogVersionMap of
                    Just actualVersion -> do
                      -- Try to find the package in the packages map first
                      let pkgKeyToLookup = mkPkgKey lockFile canonicalName actualVersion
                      case Map.lookup pkgKeyToLookup (packages lockFile) of
                        Just pkgData -> do
                          let dep = resolveDependencySnapshots catalogVersionMap canonicalName (Just actualVersion) pkgData isDepDevFlag
                          deep dep >> direct dep
                        Nothing -> do
                          -- If not in packages, try snapshots
                          let snapshotKeyAttempt = canonicalName <> "@" <> actualVersion
                          case resolveSnapshotDependency canonicalName snapshotKeyAttempt isDepDevFlag of
                            Just resolvedDep -> deep resolvedDep >> direct resolvedDep
                            Nothing -> logWarn $ pretty ("Catalog-resolved dep not found: " <> canonicalName <> "@" <> actualVersion)
                    Nothing -> logWarn $ pretty ("Unresolved catalog alias: " <> catalogAlias <> " for " <> canonicalName)
                else
                  if "link:" `Text.isPrefixOf` resolvedRefStr
                    then do
                      let userDep = createDepSimple UserType canonicalName (Just resolvedRefStr) isDepDevFlag
                      deep userDep >> direct userDep
                    else
                      if "workspace:" `Text.isPrefixOf` resolvedRefStr
                        then do
                          logWarn $ pretty ("Workspace protocol NYI for importer: " <> canonicalName <> " ref: " <> resolvedRefStr)
                          pure ()
                        else do
                          -- Assumed to be a plain version string or a direct snapshot key string
                          case resolveSnapshotDependency canonicalName resolvedRefStr isDepDevFlag of
                            Just resolvedDep -> deep resolvedDep >> direct resolvedDep
                            Nothing -> do
                              -- Fallback: resolvedRefStr was not a direct snapshot key like name@version
                              let justVersionNoSuffix = withoutPeerDepSuffix resolvedRefStr
                              if Text.isInfixOf "://" justVersionNoSuffix || Text.isPrefixOf "file:" justVersionNoSuffix
                                then do
                                  -- Fallback Case 1: resolvedRefStr is a URL/File path (used as key in packages)
                                  case Map.lookup justVersionNoSuffix (packages lockFile) of
                                    Just pkgData ->
                                      case getPkgNameVersionForV9Snapshots lockFile justVersionNoSuffix pkgData of
                                        Just (pkgNameFromData, versionMaybe) -> do
                                          let dep = resolveDependencySnapshots catalogVersionMap pkgNameFromData versionMaybe pkgData isDepDevFlag
                                          deep dep >> direct dep
                                        _ -> logWarn $ pretty ("Cannot get name/ver for pkg key: " <> justVersionNoSuffix)
                                    Nothing -> logWarn $ pretty ("Importer URL-like ref not in packages: " <> justVersionNoSuffix)
                                else
                                  if not (Text.null justVersionNoSuffix)
                                    then do
                                      -- Fallback Case 2: resolvedRefStr was a simple version string (e.g. "1.2.3")
                                      -- Try lookup in packages using canonicalName + justVersionNoSuffix
                                      let pkgKeyToLookup = mkPkgKey lockFile canonicalName justVersionNoSuffix
                                      case Map.lookup pkgKeyToLookup (packages lockFile) of
                                        Just pkgData -> do
                                          let dep = resolveDependencySnapshots catalogVersionMap canonicalName (Just justVersionNoSuffix) pkgData isDepDevFlag
                                          deep dep >> direct dep
                                        Nothing -> do
                                          -- Not in packages, final attempt: try forming name@version and looking up in snapshots
                                          let snapshotKeyAttempt = canonicalName <> "@" <> justVersionNoSuffix
                                          case resolveSnapshotDependency canonicalName snapshotKeyAttempt isDepDevFlag of
                                            Just resolvedDepFromSnap -> deep resolvedDepFromSnap >> direct resolvedDepFromSnap
                                            Nothing -> logWarn $ pretty ("Importer dep not in snaps/pkgs (plain ver): " <> canonicalName <> "@" <> justVersionNoSuffix)
                                    else
                                      -- Fallback Case 3: Empty version string after suffix removal
                                      logWarn $ pretty ("Empty version for importer dep (after suffix): " <> canonicalName <> " ref: " <> resolvedRefStr)

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
                let childDeps = Map.toList $ snapshotDependencies snapshotData
                for_ childDeps $ \(childCanonicalName, childVersionRef) -> do
                  -- Attempt to form the full snapshot key for the child
                  -- childVersionRef is often just the version string from the snapshot's dependencies map.
                  let fullChildSnapKey = childCanonicalName <> "@" <> childVersionRef
                  case resolveSnapshotDependency childCanonicalName fullChildSnapKey False of
                    Just childDep -> do
                      deep childDep
                      edge parentDep childDep
                    Nothing ->
                      -- Fallback: if childVersionRef was perhaps a path or non-standard ref
                      -- that parseSnapshotKey (even with the formed key) couldn't handle,
                      -- or if it was a git/file path that should be looked up differently.
                      -- This part needs to be robust. For now, let's try to look up childVersionRef
                      -- directly in packages IF it seems like it could be a key (e.g. contains '://' or 'file:')
                      -- OR if it's just a version, this path won't help much directly.
                      -- Consider if childVersionRef itself could be a key in `packages` for git/file.
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
                                        False
                                deep concreteChildDep
                                edge parentDep concreteChildDep
                              _ -> pure ()
                          _ -> pure ()
              Nothing -> pure () -- Couldn't resolve parentDep from parentSnapKeyStr
          Nothing -> pure () -- Couldn't parse parentSnapKeyStr
  where
    withoutLocalPackagesSnapshot = shrink (\dep -> dependencyType dep /= UserType)

    withoutPeerDepSuffix :: Text -> Text
    withoutPeerDepSuffix = fst . Text.breakOn "("

    parseSnapshotKey :: Text -> Maybe (Text, Text)
    parseSnapshotKey rawKey =
      let key = withoutPeerDepSuffix rawKey
       in case Text.splitOn "@" key of
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
            PnpmLockV9 -> cleaned -- For v9, we don't need to handle sym constraints
            _ -> if shouldApplySymConstraint lf then withoutSymConstraint cleaned else cleaned
      where
        withoutSymConstraint = fst . Text.breakOn "_"
        withoutPeerDepSuffix = fst . Text.breakOn "("
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
        shouldApplySymConstraint lock = case lockFileVersion lock of
          PnpmLockV9 -> False
          _ -> True

    -- Resolve dependency using PackageData, for non-snapshot packages (git, tarball, etc.)
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

    -- Helper to get name/version from a PnpmLockV9 package key (which might be a path for git/file)
    -- or from PackageData if key is not directly parsable by simple means.
    getPkgNameVersionForV9Snapshots :: PnpmLockfile -> Text -> PackageData -> Maybe (Text, Maybe Text) -- Return type changed
    getPkgNameVersionForV9Snapshots _ key pkgMeta =
      case resolution pkgMeta of
        GitResolve (GitResolution url rev) -> Just (url, Just rev) -- Use rev as version
        TarballResolve (TarballResolution url) -> Just (url, Nothing)
        DirectoryResolve _ -> (,) <$> name pkgMeta <*> pure Nothing -- Use name from PackageData, no specific version string
        RegistryResolve _ ->
          case parseSnapshotKey key of
            Just (n, v) -> Just (n, Just v) -- If key is name@version
            Nothing ->
              -- If key is not name@version (e.g. /@scope/name@version), use name from pkgMeta if available
              -- and try to extract version from the key heuristically.
              (,) <$> name pkgMeta <*> pure (extractVersionFromKey key)
      where
        extractVersionFromKey :: Text -> Maybe Text
        extractVersionFromKey k =
          let parts = Text.splitOn "@" k
           in if length parts > 1
                then Just (last parts)
                else
                  let slashParts = Text.splitOn "/" k
                   in if length slashParts >= 3 then Just (slashParts !! 2) else Nothing
