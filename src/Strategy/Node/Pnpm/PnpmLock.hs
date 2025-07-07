{-# LANGUAGE OverloadedRecordDot #-}

module Strategy.Node.Pnpm.PnpmLock (
  analyze,

  -- * for testing
  buildGraph,
)
where

import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, Has, context)
import Data.Aeson (FromJSON (..), withObject)
import Data.Aeson.Extra (TextLike (..))
import Data.Foldable (for_)
import Data.Map (Map, toList)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.String.Conversion (toString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Yaml (Object, Parser, (.!=), (.:), (.:?))
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
  , lockFileVersion :: PnpmLockFileVersion
  , lockFileSnapshots :: PnpmLockFileSnapshots
  -- ^ Dependency graph in lockfile version > 9
  }
  deriving (Show, Eq, Ord)

type SnapshotDepName = Text

type SnapShotDepRev = Text

-- | Lockfile versions > 9 use snapshots to represent the dependency graph.
newtype PnpmLockFileSnapshots = PnpmLockFileSnapshots
  {snapshots :: Map SnapshotDepName [(SnapshotDepName, SnapShotDepRev)]}
  deriving (Show, Eq, Ord, Semigroup, Monoid)

instance FromJSON PnpmLockFileSnapshots where
  parseJSON v = do
    let readTransitiveDepPairs o = o .:? "dependencies" .!= mempty
    snapshots <- withObject "Read PnpmLockFileSnapshots" readTransitiveDepPairs =<< parseJSON v
    pure $
      PnpmLockFileSnapshots{snapshots}

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
    -- PNPM 9 snapshots
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

    pure $ PnpmLockfile{importers = refinedImporters, packages = packages, lockFileVersion = rawLockFileVersion, lockFileSnapshots = snapshots}
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
  , name :: Maybe Text -- only provided when non-registry resolver is used
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
    for_ (toList lockFile.importers) $ \(_, projectSnapshot) -> do
      let allDirectDependencies =
            toList (directDependencies projectSnapshot)
              <> toList (directDevDependencies projectSnapshot)

      for_ allDirectDependencies $ \(depName, (ProjectMapDepMetadata depVersion)) ->
        maybe (pure ()) direct $ toResolvedDependency depName depVersion

    -- Add edges and deep dependencies by iterating over all packages.
    --
    -- Use `dev` to infer if this is production or non-production dependency.
    -- Use `dependencies` to infer the edge from this dependency (aws-sdk@2.1148.0)
    --   to its children. If the child entry cannot be found in `packages`,
    --   do not provision an edge.
    --
    -- @
    -- > packages:
    -- >
    -- >  /aws-sdk/2.1148.0:
    -- >    resolution: {integrity: sha512-FUYAyveKmS5eq..==}
    -- >    engines: {node: '>= 10.0.0'}
    -- >    dependencies:
    -- >      buffer: 4.9.2
    -- >      events: 1.1.1
    -- >    dev: false
    -- @
    for_ (toList lockFile.packages) $ \(pkgKey, pkgMeta) -> do
      let deepDependencies =
            Map.toList (dependencies pkgMeta)
              <> Map.toList (peerDependencies pkgMeta)
              <> fromMaybe mempty (Map.lookup pkgKey lockFile.lockFileSnapshots.snapshots)

      let (depName, depVersion) = case getPkgNameVersion pkgKey of
            Nothing -> (pkgKey, Nothing)
            Just (name, version) -> (name, Just version)
      let parentDep = toDependency depName depVersion pkgMeta

      -- It is ok, if this dependency was already graphed as direct
      -- @direct 1 <> deep 1 = direct 1@
      deep parentDep

      for_ deepDependencies $ \(deepName, deepVersion) -> do
        maybe (pure ()) (edge parentDep) (toResolvedDependency deepName deepVersion)
  where
    getPkgNameVersion :: Text -> Maybe (Text, Text)
    getPkgNameVersion = case lockFileVersion lockFile of
      PnpmLock4Or5 -> getPkgNameVersionV5
      PnpmLock6 -> getPkgNameVersionV6
      PnpmLockLt4 _ -> getPkgNameVersionV5 -- v3 or below are deprecated and are not used in practice, fallback to closest
      PnpmLockV678 _ -> getPkgNameVersionV6 -- at the time of writing there is no v7, so default to closest
      PnpmLockV9 -> getPkgNameVersionV9

    -- Gets package name and version from package's key.
    --
    -- >> getPkgNameVersionV6 "" = Nothing
    -- >> getPkgNameVersionV6 "github.com/something" = Nothing
    -- >> getPkgNameVersionV6 "/pkg-a@1.0.0" = Just ("pkg-a", "1.0.0")
    -- >> getPkgNameVersionV6 "/@angular/core@1.0.0" = Just ("@angular/core", "1.0.0")
    -- >> getPkgNameVersionV6 "/@angular/core@1.0.0(babel@1.0.0)" = Just ("@angular/core", "1.0.0(babel@1.0.0)")
    getPkgNameVersionV6 :: Text -> Maybe (Text, Text)
    getPkgNameVersionV6 pkgKey = case (Text.stripPrefix "/" pkgKey) of
      Nothing -> Nothing
      Just txt -> do
        let (nameAndVersion, peerDepInfo) = Text.breakOn "(" txt
        let (nameWithSlash, version) = Text.breakOnEnd "@" nameAndVersion
        case (Text.stripSuffix "@" nameWithSlash, version) of
          (Just name, v) -> Just (name, v <> peerDepInfo)
          _ -> Nothing
    -- Pnpm 9.0 registry packages may not have a leading slash, so it is not required.
    --
    -- >> getPkgNameVersionV9 "@angular/core@1.0.0(babel@1.0.0) = Just ("@angular/core", "1.0.0(babel@1.0.0")
    getPkgNameVersionV9 :: Text -> Maybe (Text, Text)
    getPkgNameVersionV9 pkgKey = do
      let txt = Maybe.fromMaybe pkgKey (Text.stripPrefix "/" pkgKey)
          (nameAndVersion, peerDepInfo) = Text.breakOn "(" txt
          (nameWithSlash, version) = Text.breakOnEnd "@" nameAndVersion
      case (Text.stripSuffix "@" nameWithSlash, version) of
        (Just name, v) -> Just (name, v <> peerDepInfo)
        _ -> Nothing

    -- Gets package name and version from package's key.
    --
    -- >> getPkgNameVersionV5 "" = Nothing
    -- >> getPkgNameVersionV5 "github.com/something" = Nothing
    -- >> getPkgNameVersionV5 "/pkg-a/1.0.0" = Just ("pkg-a", "1.0.0")
    -- >> getPkgNameVersionV5 "/@angular/core/1.0.0" = Just ("@angular/core", "1.0.0")
    getPkgNameVersionV5 :: Text -> Maybe (Text, Text)
    getPkgNameVersionV5 pkgKey = case (Text.stripPrefix "/" pkgKey) of
      Nothing -> Nothing
      Just txt -> do
        let (nameWithSlash, version) = Text.breakOnEnd "/" txt
        case (Text.stripSuffix "/" nameWithSlash, version) of
          (Just name, v) -> Just (name, v)
          _ -> Nothing

    -- Non-registry resolvers (tarball, git, directory) use non-version identifier
    -- as version value in dependencies map, as well as for it's `packages` key.
    --
    -- @
    -- >  dependencies:
    -- >    chokidar: 1.0.0 # resolved with registry
    -- >    some-other-project: file:../local-package # resolved with non-registry resolver
    -- @
    -- -
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
    toResolvedDependency depName depVersion = do
      let maybeNonRegistrySrcPackage = Map.lookup depVersion (packages lockFile)
      let maybeRegistrySrcPackage = Map.lookup (mkPkgKey depName depVersion) (packages lockFile)
      case (maybeNonRegistrySrcPackage, maybeRegistrySrcPackage) of
        (Nothing, Nothing) -> Nothing
        (Just nonRegistryPkg, _) -> Just $ toDependency depName Nothing nonRegistryPkg
        (Nothing, Just registryPkg) -> Just $ toDependency depName (Just depVersion) registryPkg

    -- Makes representative key if the package was
    -- resolved via registry resolver.
    --
    -- >> mkPkgKey "pkg-a" "1.0.0" = "/pkg-a/1.0.0" -- for v5 fmt
    -- >> mkPkgKey "pkg-a" "1.0.0" = "/pkg-a@1.0.0" -- for v6 fmt
    -- >> mkPkgKey "pkg-a" "1.0.0(babal@1.0.0)" = "/pkg-a@1.0.0(babal@1.0.0)" -- for v6 fmt
    mkPkgKey :: Text -> Text -> Text
    mkPkgKey name version = case lockFileVersion lockFile of
      PnpmLock4Or5 -> "/" <> name <> "/" <> version
      PnpmLock6 -> "/" <> name <> "@" <> version
      -- v3 or below are deprecated and are not used in practice, fallback to closest
      PnpmLockLt4 _ -> "/" <> name <> "/" <> version
      -- at the time of writing there is no v7, so default to closest
      PnpmLockV678 _ -> "/" <> name <> "@" <> version
      PnpmLockV9 -> name <> "@" <> version

    toDependency :: Text -> Maybe Text -> PackageData -> Dependency
    toDependency name maybeVersion (PackageData isDev _ (RegistryResolve _) _ _) =
      toDep NodeJSType name (withoutPeerDepSuffix . withoutSymConstraint <$> maybeVersion) isDev
    toDependency _ _ (PackageData isDev _ (GitResolve (GitResolution url rev)) _ _) =
      toDep GitType url (Just rev) isDev
    toDependency _ _ (PackageData isDev _ (TarballResolve (TarballResolution url)) _ _) =
      toDep URLType url Nothing isDev
    toDependency _ _ (PackageData isDev (Just name) (DirectoryResolve _) _ _) =
      toDep UserType name Nothing isDev
    toDependency name _ (PackageData isDev Nothing (DirectoryResolve _) _ _) =
      toDep UserType name Nothing isDev

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

    toDep :: DepType -> Text -> Maybe Text -> Bool -> Dependency
    toDep depType name version isDev = Dependency depType name (CEq <$> version) mempty (toEnv isDev) mempty

    toEnv :: Bool -> Set.Set DepEnvironment
    toEnv isNotRequired = Set.singleton $ if isNotRequired then EnvDevelopment else EnvProduction

    withoutLocalPackages :: Graphing Dependency -> Graphing Dependency
    withoutLocalPackages = Graphing.shrink (\dep -> dependencyType dep /= UserType)
