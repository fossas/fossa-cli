module Strategy.Node.Pnpm.PnpmLock (
  analyze,

  -- * for testing
  buildGraph,
) where

import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, Has, context)
import Data.Foldable (for_)
import Data.Map (Map, toList)
import Data.Map qualified as Map
import Data.Set qualified as Set
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
import Effect.ReadFS (ReadFS, readContentsYaml)
import Graphing (Graphing, shrink)
import Path (Abs, File, Path)

-- | Pnpm Lockfile
--
-- Pnpm lockfile (in yaml) has the following shape (irrelevant fields omitted):
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
--  References:
--    - [pnpm](https://pnpm.io/)
--    - [pnpm-lockfile](https://github.com/pnpm/pnpm/blob/5cfd6d01946edcce86f62580bddc788d02f93ed6/packages/lockfile-types/src/index.ts)
data PnpmLockfile = PnpmLockfile
  { lockFileVersion :: Float
  , importers :: Map Text ProjectMap
  , packages :: Map Text PackageData
  }
  deriving (Show, Eq, Ord)

instance FromJSON PnpmLockfile where
  parseJSON = Yaml.withObject "pnpm-lock content" $ \obj -> do
    lockFileVersion <- obj .: "lockfileVersion"
    importers <- obj .:? "importers" .!= mempty
    packages <- obj .:? "packages" .!= mempty

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

    pure $ PnpmLockfile lockFileVersion refinedImporters packages

data ProjectMap = ProjectMap
  { directDependencies :: Map Text Text
  , directDevDependencies :: Map Text Text
  }
  deriving (Show, Eq, Ord)

instance FromJSON ProjectMap where
  parseJSON = Yaml.withObject "ProjectMap" $ \obj ->
    ProjectMap
      <$> obj .:? "dependencies" .!= mempty
      <*> obj .:? "devDependencies" .!= mempty

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

analyze :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze file = context "Analyzing Npm Lockfile (v3)" $ do
  pnpmLockFile <- context "Parsing pnpm-lock file" $ readContentsYaml file
  context "Building dependency graph" $ pure $ buildGraph pnpmLockFile

buildGraph :: PnpmLockfile -> Graphing Dependency
buildGraph lockFile = withoutLocalPackages $
  run . evalGrapher $ do
    for_ (toList $ importers lockFile) $ \(_, projectSnapshot) -> do
      let allDirectDependencies =
            toList (directDependencies projectSnapshot)
              <> toList (directDevDependencies projectSnapshot)

      for_ allDirectDependencies $ \(depName, depVersion) ->
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
    for_ (toList $ packages lockFile) $ \(pkgKey, pkgMeta) -> do
      let deepDependencies =
            toList (dependencies pkgMeta)
              <> toList (peerDependencies pkgMeta)

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
    -- Gets package name and version from package's key.
    --
    -- >> getPkgNameVersion "" = Nothing
    -- >> getPkgNameVersion "github.com/something" = Nothing
    -- >> getPkgNameVersion "/pkg-a/1.0.0" = Just ("pkg-a", "1.0.0")
    -- >> getPkgNameVersion "/@angular/core/1.0.0" = Just ("@angular/core", "1.0.0")
    getPkgNameVersion :: Text -> Maybe (Text, Text)
    getPkgNameVersion pkgKey = case (Text.stripPrefix "/" pkgKey) of
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
    --   /${depName}/${depVersion}
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
    -- >> mkPkgKey "pkg-a" "1.0.0" = "/pkg-a/1.0.0"
    mkPkgKey :: Text -> Text -> Text
    mkPkgKey name version = "/" <> name <> "/" <> version

    toDependency :: Text -> Maybe Text -> PackageData -> Dependency
    toDependency name maybeVersion (PackageData isDev _ (RegistryResolve _) _ _) =
      toDep NodeJSType name (withoutSymConstraint <$> maybeVersion) isDev
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

    toDep :: DepType -> Text -> Maybe Text -> Bool -> Dependency
    toDep depType name version isDev = Dependency depType name (CEq <$> version) mempty (toEnv isDev) mempty

    toEnv :: Bool -> Set.Set DepEnvironment
    toEnv isNotRequired = Set.singleton $ if isNotRequired then EnvDevelopment else EnvProduction

    withoutLocalPackages :: Graphing Dependency -> Graphing Dependency
    withoutLocalPackages = Graphing.shrink (\dep -> dependencyType dep /= UserType)
