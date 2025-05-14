module Strategy.Node.Pnpm.PnpmLock (
  -- | Analyzes a pnpm lockfile and returns a graph of dependencies.
  analyze,

  -- * for testing

  -- | Builds a graph of dependencies from a pnpm lockfile.
  buildGraph,
  buildGraphLegacy,
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
  Dependency (Dependency, dependencyEnvironments, dependencyType),
  VerConstraint (CEq),
 )
import Effect.Grapher (deep, direct, edge, evalGrapher)
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

withoutPeerDepSuffix :: Text -> Text
withoutPeerDepSuffix = fst . Text.breakOn "("

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

analyze :: (Has ReadFS sig m, Has Logger sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
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
buildGraphLegacy lockFile =
  withoutLocalPackages $
    runIdentity $
      evalGrapher $ do
        -- START: Local helpers for buildGraphLegacy, mimicking master's buildGraph logic
        let getPkgNameVersionV5Legacy :: Text -> Maybe (Text, Text)
            getPkgNameVersionV5Legacy pkgKey = case Text.stripPrefix "/" pkgKey of
              Nothing -> Nothing
              Just txt ->
                let (nameWithSlash, version) = Text.breakOnEnd "/" txt
                 in case (Text.stripSuffix "/" nameWithSlash, version) of
                      (Just name, v) -> Just (name, v)
                      _ -> Nothing

            getPkgNameVersionV6Legacy :: Text -> Maybe (Text, Text)
            getPkgNameVersionV6Legacy pkgKey = case Text.stripPrefix "/" pkgKey of
              Nothing -> Nothing
              Just txt ->
                let (nameAndVersion, peerDepInfo) = Text.breakOn "(" txt -- Includes peer suffix
                    (nameWithSlash, version) = Text.breakOnEnd "@" nameAndVersion
                 in case (Text.stripSuffix "@" nameWithSlash, version) of
                      (Just name, v) -> Just (name, v <> peerDepInfo) -- Pass with peer suffix
                      _ -> Nothing

            getPkgNameVersionLegacyDispatch :: PnpmLockFileVersion -> Text -> Maybe (Text, Text)
            getPkgNameVersionLegacyDispatch lockVer key = case lockVer of
              PnpmLock4Or5 -> getPkgNameVersionV5Legacy key
              PnpmLock6 -> getPkgNameVersionV6Legacy key
              PnpmLockLt4 _ -> getPkgNameVersionV5Legacy key -- Fallback for v1-v3
              _ -> Nothing -- Should not be called by buildGraphLegacy for v9+

        -- mkPkgKeyLegacyLocal was removed as it's unused here.
        -- toResolvedDependency uses its own mkPkgKeyLegacyForResolved.

        -- toDependency, toDep, toEnv, toResolvedDependency, withoutSymConstraint are defined below,
        -- local to buildGraphLegacy's where clause, as they were in the previous correct structure.
        -- END: Local helpers

        for_ (Map.toList $ importers lockFile) $ \(_, projectSnapshot) -> do
          let allDirectDependencies =
                Map.toList (directDependencies projectSnapshot)
                  <> Map.toList (directDevDependencies projectSnapshot)

          for_ allDirectDependencies $ \(depName, ProjectMapDepMetadata depVersion) ->
            maybe (pure ()) direct $ toResolvedDependency depName depVersion

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

    -- This mkPkgKeyLegacyLocal needs to be accessible by toResolvedDependency
    -- It is defined in the let block above now.
    -- For toResolvedDependency to use the mkPkgKeyLegacyLocal from the let block,
    -- it needs to be passed explicitly or buildGraphLegacy needs to be a single do block
    -- with the helpers in a where clause that also contains toResolvedDependency.
    -- The current structure has toResolvedDependency in the where clause, separate from the let.

    -- Let's adjust toResolvedDependency to use the PnpmLockFileVersion directly for its local mkPkgKey call
    -- or ensure mkPkgKeyLegacyLocal is in its scope.
    -- The simplest way is to pass PnpmLockFileVersion to a local mkPkgKey inside toResolvedDependency or make it part of its 'where' if needed.

    toResolvedDependency :: Text -> Text -> Maybe Dependency
    toResolvedDependency depName depVersion =
      let -- Define mkPkgKeyLegacyLocal again here for toResolvedDependency's scope, or pass it.
          -- For simplicity, let's redefine it here if it's purely based on its inputs.
          -- Alternatively, ensure the top-level mkPkgKey is NOT used for legacy paths.
          -- The let-bound mkPkgKeyLegacyLocal is not in scope here.
          -- So, we must make a choice.
          -- Replicating the logic here for clarity of buildGraphLegacy's self-containment for this helper:
          mkPkgKeyLegacyForResolved :: PnpmLockFileVersion -> Text -> Text -> Text
          mkPkgKeyLegacyForResolved lockVer name ver = case lockVer of
            PnpmLock4Or5 -> "/" <> name <> "/" <> ver
            PnpmLock6 -> "/" <> name <> "@" <> ver
            PnpmLockLt4 _ -> "/" <> name <> "/" <> ver
            _ -> "/" <> name <> "@" <> ver

          maybeNonRegistrySrcPackage = Map.lookup depVersion (packages lockFile)
          maybeRegistrySrcPackage = Map.lookup (mkPkgKeyLegacyForResolved (lockFileVersion lockFile) depName depVersion) (packages lockFile)
       in case (maybeNonRegistrySrcPackage, maybeRegistrySrcPackage) of
            (Nothing, Nothing) -> Nothing
            (Just nonRegistryPkg, _) -> Just $ toDependency depName Nothing nonRegistryPkg
            (_, Just registryPkg) -> Just $ toDependency depName (Just depVersion) registryPkg

    withoutSymConstraint :: Text -> Text
    withoutSymConstraint = fst . Text.breakOn "_"

-- Main buildGraph function that dispatches based on lockfile version
buildGraph :: (Has Logger sig m) => PnpmLockfile -> m (Graphing Dependency)
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
buildGraphWithSnapshots :: (Has Logger sig m) => PnpmLockfile -> m (Graphing Dependency)
buildGraphWithSnapshots lockFile =
  fmap withoutLocalPkgsSnap $
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
      PnpmLockV9 -> False
      _ -> True
