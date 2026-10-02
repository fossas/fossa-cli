{-# LANGUAGE OverloadedRecordDot #-}

module Strategy.Node.Pnpm.Types (
  -- * Lockfile types
  PnpmLockfileBase (..),
  PnpmLockfileV4Or5 (..),
  PnpmLockfileV678 (..),
  PnpmLockfileV9 (..),
  PnpmLockfile (..),
  mergeLockfiles,

  -- * Catalogs
  PnpmCatalogs (..),

  -- * Snapshots
  SnapshotDepName,
  SnapShotDepRev,
  PnpmLockFileSnapshots (..),

  -- * Project map
  ProjectMap (..),
  ProjectMapDepMetadata (..),

  -- * Package data
  PackageData (..),
  Resolution (..),
  GitResolution (..),
  TarballResolution (..),
  RegistryResolution (..),
  DirectoryResolution (..),

  -- * Graph configuration
  LabelingMode (..),
  BuildGraphConfig (..),

  -- * Helpers
  withoutPeerDepSuffix,
) where

import Control.Applicative ((<|>))

import Data.Aeson (FromJSON (..), withObject)
import Data.Aeson.Extra (TextLike (..))
import Data.Aeson.KeyMap (toHashMapText)
import Data.Char (isDigit)
import Data.HashMap.Strict qualified as HashMap
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.String.Conversion (toString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Yaml (Object, Parser, (.!=), (.:), (.:?))
import Data.Yaml qualified as Yaml
import DepTypes (DepEnvironment)
import Text.Read (readMaybe)

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
--    * 'importers': refers to configurations imported\/specified via package.json.
--      * Key of 'importers' (e.g. \".\") refers to package.json filepath.
--        * 'specifier': refers to version constraint specified in package.json.
--        * 'dependencies': refer to direct and resolved production dependencies.
--        * 'devDependencies': refer to direct and resolved development dependencies,
--
--    * 'packages': lists all resolved dependencies (it does not include root level workspace package, or root package itself)
--      * Key of 'packages' refer (e.g. \"/buffer\/4.9.2:\") denotes name of dependency and resolved version.
--          - For dependency resolved via registry resolver, format is: \"/${dependencyName}\/${resolvedVersion}\".
--          - For dependency resolved via tarball resolver, format is: \"${Url}\".
--          - For dependency resolved via git resolver, format is: \"${Url}\".
--          - For dependency resolved via directory resolver, format is: \"file:${relativePath}\".
--
--
--  Pnpm lockfile (v6) differs (v5), in following manner:
--  -----------------------------------------------------
--
--    * 'importers' shape merges specifiers and version, in singular object:
--      @
--      > importers:
--      >    dependencies:
--      >      aws-sdk:
--      >        specifier: 2.1148.0
--      >        version: 2.1148.0
--      @
--
--    * Key of 'packages' refer (e.g. \"/buffer@4.9.2\") denotes name of dependency and resolved version using '@' separator
--        - For dependency resolved via registry resolver, format is: \"/${dependencyName}@${resolvedVersion}${peerDepsInParenthesis}\".
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
--      work is required for newly introduced 'settings.autoInstallPeers' field.
--      This means, that if user has chosen, not to install peerDependencies, they
--      won't be included in the lock-file, so no additional work is required by fossa-cli.
--      Note that, fossa-cli by default includes peer dependencies.
--
--  References:
--    - [pnpm](https://pnpm.io/)
--    - [pnpm-lockfile](https://github.com/pnpm/pnpm/blob/5cfd6d01946edcce86f62580bddc788d02f93ed6/packages/lockfile-types/src/index.ts)
--    - [pnpm-lockfile-v6](https://github.com/pnpm/pnpm/pull/5810/files)

--
-- Lockfile types
--

-- | Shared fields present in all lockfile versions.
data PnpmLockfileBase = PnpmLockfileBase
  { lockfileImporters :: Map Text ProjectMap
  , lockfilePackages :: Map Text PackageData
  , lockfileRawVersion :: Text
  -- ^ Raw lockfileVersion string, used for warning about unsupported versions.
  }
  deriving (Show, Eq, Ord)

-- | Version-specific extension for v4\/v5 lockfiles.
newtype PnpmLockfileV4Or5 = PnpmLockfileV4Or5 PnpmLockfileBase
  deriving (Show, Eq, Ord, Semigroup)

-- | Version-specific extension for v6\/v7\/v8 lockfiles.
newtype PnpmLockfileV678 = PnpmLockfileV678 PnpmLockfileBase
  deriving (Show, Eq, Ord, Semigroup)

-- | Version-specific extension for v9+ lockfiles.
data PnpmLockfileV9 = PnpmLockfileV9
  { lockfileBase :: PnpmLockfileBase
  , lockfileSnapshots :: PnpmLockFileSnapshots
  , lockfileCatalogs :: PnpmCatalogs
  }
  deriving (Show, Eq, Ord)

-- | Pnpm lockfile, parameterized by version.
--
-- The type constructor encodes the version, so that each variant
-- carries only the data relevant to its format.
data PnpmLockfile
  = LockfileV4Or5 PnpmLockfileV4Or5
  | LockfileV678 PnpmLockfileV678
  | LockfileV9 PnpmLockfileV9
  deriving (Show, Eq, Ord)

--
-- Merging multi-document lockfiles
--
--
-- pnpm v11+ can write pnpm-lock.yaml as a multi-document YAML stream: an env
-- lockfile (config / package-manager dependencies) followed by the project
-- lockfile. Both documents carry the same lockfileVersion and each parses as a
-- valid 'PnpmLockfile'. A tool that reports everything the lockfile installs
-- (SBOM generators, vulnerability scanners) must union the documents rather
-- than keep only the first one: the env document parses successfully and would
-- otherwise shadow the project graph.

-- | Combine two documents of one multi-document stream. Documents are unioned
-- with the later document winning on conflicts, since pnpm writes the env
-- document first and the project document last.
--
-- This is a plain function rather than a 'Semigroup' instance because the sum
-- type has no lawful combination across constructors: 'PnpmLockfile' variants
-- of different versions have nothing sensible to merge, and every total
-- fallback (keep left, keep right) is non-associative — and would silently
-- drop one document's graph, the very bug class this fixes. A stream whose
-- documents disagree on lockfileVersion is malformed and must be rejected.
mergeLockfiles :: PnpmLockfile -> PnpmLockfile -> Either Text PnpmLockfile
mergeLockfiles (LockfileV4Or5 a) (LockfileV4Or5 b) = Right (LockfileV4Or5 (a <> b))
mergeLockfiles (LockfileV678 a) (LockfileV678 b) = Right (LockfileV678 (a <> b))
mergeLockfiles (LockfileV9 a) (LockfileV9 b) = Right (LockfileV9 (a <> b))
mergeLockfiles _ _ = Left "multi-document lockfile with inconsistent lockfileVersions across documents"

instance Semigroup PnpmLockfileBase where
  (<>) a b =
    PnpmLockfileBase
      { lockfileImporters = Map.unionWith (<>) a.lockfileImporters b.lockfileImporters
      , lockfilePackages = unionPreferringSecond a.lockfilePackages b.lockfilePackages
      , -- First non-empty: documents of one stream share one lockfileVersion,
        -- and the empty version is the identity, keeping this instance lawful.
        lockfileRawVersion =
          if Text.null a.lockfileRawVersion then b.lockfileRawVersion else a.lockfileRawVersion
      }

instance Semigroup PnpmLockfileV9 where
  (<>) a b =
    PnpmLockfileV9
      { lockfileBase = lockfileBase a <> lockfileBase b
      , lockfileSnapshots = mergeSnapshots (lockfileSnapshots a) (lockfileSnapshots b)
      , lockfileCatalogs =
          PnpmCatalogs
            ( unionPreferringSecond
                (catalogEntries (lockfileCatalogs a))
                (catalogEntries (lockfileCatalogs b))
            )
      }

-- | Union two maps giving precedence to the second argument, so that later
-- documents in a multi-document lockfile win on conflicting keys. ('Map.union'
-- prefers its first argument instead, which would let the env front-document
-- shadow the project lockfile's data.)
unionPreferringSecond :: Ord k => Map k v -> Map k v -> Map k v
unionPreferringSecond first second = Map.union second first

-- | Union two snapshot maps by key. On a shared key the later document's
-- dependency list wins (List.fromList keeps the last occurrence for a key).
mergeSnapshots :: PnpmLockFileSnapshots -> PnpmLockFileSnapshots -> PnpmLockFileSnapshots
mergeSnapshots (PnpmLockFileSnapshots a) (PnpmLockFileSnapshots b) =
  PnpmLockFileSnapshots (HashMap.fromList (HashMap.toList a <> HashMap.toList b))

--
-- Catalogs
--

-- | Catalogs parsed from lockfile. Maps catalog name to (package name -> resolved version).
-- See: https://pnpm.io/catalogs
newtype PnpmCatalogs = PnpmCatalogs
  { catalogEntries :: Map Text (Map Text Text)
  }
  deriving (Show, Eq, Ord, Semigroup, Monoid)

instance FromJSON PnpmCatalogs where
  parseJSON = withObject "PnpmCatalogs" $ \o -> do
    parsed <- traverse parseCatalog (toHashMapText o)
    pure $ PnpmCatalogs (Map.fromList $ HashMap.toList parsed)
    where
      parseCatalog :: Yaml.Value -> Parser (Map Text Text)
      parseCatalog = withObject "Catalog" $ \entries ->
        (Map.fromList . HashMap.toList)
          <$> traverse (withObject "CatalogEntry" (.: "version")) (toHashMapText entries)

--
-- Snapshots
--

type SnapshotDepName = Text

type SnapShotDepRev = Text

-- | Lockfile versions > 9 use snapshots to represent the dependency graph.
-- Example:
--   snapshots:
--
--     colorjs@0.1.9: {}
--
--     punycode@2.3.1: {}
--
--     uri-js@4.4.1:
--       dependencies:
--       punycode: 2.3.1
newtype PnpmLockFileSnapshots = PnpmLockFileSnapshots
  {snapshots :: HashMap.HashMap SnapshotDepName [(SnapshotDepName, SnapShotDepRev)]}
  deriving (Show, Eq, Ord, Semigroup, Monoid)

instance FromJSON PnpmLockFileSnapshots where
  parseJSON = withObject "Read PnpmLockFileSnapshots" $ \o ->
    do
      let readTransitiveDepPairs = withObject "Parse dependencies" $
            \ds -> do
              deps <- ds .:? "dependencies" .!= mempty
              pure . HashMap.toList $ deps
      snapshots <- traverse readTransitiveDepPairs o

      -- Remove the peer dependency suffix. It's present in the snapshot entry, but it's not present in packages
      -- section which is where we look the dependency up.
      let snapshots' = (HashMap.mapKeys withoutPeerDepSuffix) . toHashMapText $ snapshots
      pure $
        PnpmLockFileSnapshots{snapshots = snapshots'}

--
-- Project map
--

data ProjectMap = ProjectMap
  { directDependencies :: Map Text ProjectMapDepMetadata
  , directDevDependencies :: Map Text ProjectMapDepMetadata
  , configDependencies :: Map Text ProjectMapDepMetadata
  -- ^ pnpm config dependencies: real packages, installed into
  -- @node_modules/.pnpm-config@, so they must be reported too.
  , packageManagerDependencies :: Map Text ProjectMapDepMetadata
  -- ^ The pnpm version resolved for the project.
  }
  deriving (Show, Eq, Ord)

instance FromJSON ProjectMap where
  parseJSON = Yaml.withObject "ProjectMap" $ \obj ->
    ProjectMap
      <$> obj .:? "dependencies" .!= mempty
      <*> obj .:? "devDependencies" .!= mempty
      <*> obj .:? "configDependencies" .!= mempty
      <*> obj .:? "packageManagerDependencies" .!= mempty

-- | Union two importers field-wise. The env and the project lockfile document
-- both use the @.@ importer key; a whole-value overwrite would lose whichever
-- graph the dropped side held (pnpm's lockfile docs warn about exactly this).
instance Semigroup ProjectMap where
  (<>) a b =
    ProjectMap
      { directDependencies = unionPreferringSecond (directDependencies a) (directDependencies b)
      , directDevDependencies = unionPreferringSecond (directDevDependencies a) (directDevDependencies b)
      , configDependencies = unionPreferringSecond (configDependencies a) (configDependencies b)
      , packageManagerDependencies = unionPreferringSecond (packageManagerDependencies a) (packageManagerDependencies b)
      }

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

--
-- Package data
--

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

--
-- Version classification
--

-- | Internal version classifier used during JSON parsing.
data VersionClassifier
  = VersionV4Or5
  | VersionV678
  | VersionV9
  deriving (Show, Eq, Ord)

-- | Classify the lockfile version string.
--
-- >> classifyVersion (TextLike "5.4") = VersionV4Or5
-- >> classifyVersion (TextLike "9.0") = VersionV9
-- >> classifyVersion (TextLike "10.0") = VersionV9
classifyVersion :: TextLike -> Parser VersionClassifier
classifyVersion (TextLike ver) =
  case readMaybe (toString (Text.takeWhile isDigit ver)) :: Maybe Int of
    Nothing -> fail $ "expected numeric lockfileVersion, got: " <> show ver
    Just v
      | v >= 1 && v <= 5 -> pure VersionV4Or5
      | v >= 6 && v <= 8 -> pure VersionV678
      | v >= 9 -> pure VersionV9
      | otherwise -> fail $ "unsupported lockfileVersion: " <> show ver

-- | Parse the shared base fields (importers + packages) common to all versions.
parseBaseLockfile :: TextLike -> Object -> Parser PnpmLockfileBase
parseBaseLockfile (TextLike rawVer) obj = do
  -- Map pnpm non-workspace lockfile format to pnpm workspace lockfile format.
  --
  -- For lockfile without workspaces, the 'importers' field is not included in
  -- the lockfile. And 'dependencies' and 'devDependencies' are instead shown
  -- at the root level.
  --
  -- A project without a workspace is the same as having a single workspace at
  -- the path of \".\".
  importers <- obj .:? "importers" .!= mempty
  packages <- obj .:? "packages" .!= mempty
  dependencies <- obj .:? "dependencies" .!= mempty
  devDependencies <- obj .:? "devDependencies" .!= mempty
  let virtualRootWs = ProjectMap dependencies devDependencies mempty mempty
  let refinedImporters =
        if Map.null importers
          then Map.insert "." virtualRootWs importers
          else importers
  pure $
    PnpmLockfileBase
      { lockfileImporters = refinedImporters
      , lockfilePackages = packages
      , lockfileRawVersion = rawVer
      }

-- | FromJSON for the sum type — reads 'lockfileVersion' and dispatches.
instance FromJSON PnpmLockfile where
  parseJSON = Yaml.withObject "pnpm-lock content" $ \obj -> do
    rawVer <- obj .:? "lockfileVersion" .!= (TextLike mempty)
    base <- parseBaseLockfile rawVer obj

    versionClass <- classifyVersion rawVer
    case versionClass of
      VersionV4Or5 -> pure $ LockfileV4Or5 $ PnpmLockfileV4Or5 base
      VersionV678 -> pure $ LockfileV678 $ PnpmLockfileV678 base
      VersionV9 -> do
        snapshots <- obj .:? "snapshots" .!= mempty
        catalogs <- obj .:? "catalogs" .!= mempty
        pure $
          LockfileV9 $
            PnpmLockfileV9
              { lockfileBase = base
              , lockfileSnapshots = snapshots
              , lockfileCatalogs = catalogs
              }

--
-- Graph configuration
--

-- | Whether to label direct dependencies so that 'hydrateDepEnvs'
-- propagates environments through the graph.
--
-- V9 lockfiles start with empty environments and rely on label
-- propagation. Older versions set environments inline from the @dev@ field.
data LabelingMode = LabelingOff | LabelingOn
  deriving (Show, Eq, Ord)

-- | Configuration for graph building.
--
-- Each field controls version-specific behavior:
--
--   - 'bgcGetPkgNameVersion': parse (name, version) from a package key
--   - 'bgcMkPkgKey': build a registry package key from (name, version)
--   - 'bgcToEnv': derive environment from @dev@ field (or ignore it for v9)
--   - 'bgcLabelingMode': whether to label direct deps for hydrateDepEnvs propagation
--   - 'bgcSnapshotEdges': additional edges from v9 snapshots (empty for older versions)
--   - 'bgcCatalogs': catalog name -> (package name -> resolved version) mappings
data BuildGraphConfig = BuildGraphConfig
  { bgcGetPkgNameVersion :: Text -> Maybe (Text, Text)
  -- ^ Parse (name, version) from a package key
  , bgcMkPkgKey :: Text -> Text -> Text
  -- ^ Build a registry package key from (name, version)
  , bgcToEnv :: Bool -> Set.Set DepEnvironment
  -- ^ Derive environment from @dev@ field (or ignore for v9)
  , bgcLabelingMode :: LabelingMode
  -- ^ Whether to label direct deps for hydrateDepEnvs propagation
  , bgcSnapshotEdges :: [(SnapshotDepName, [(SnapshotDepName, SnapShotDepRev)])]
  -- ^ Additional edges from v9 snapshots
  , bgcCatalogs :: PnpmCatalogs
  -- ^ Catalog name -> (package name -> resolved version) mappings
  }

--
-- Helpers
--

-- | Strip peer dependency suffix from version strings.
--
-- >> withoutPeerDepSuffix "1.2.0" = "1.2.0"
-- >> withoutPeerDepSuffix "1.2.0(babel@1.0.0)" = "1.2.0"
withoutPeerDepSuffix :: Text -> Text
withoutPeerDepSuffix = fst . Text.breakOn "("
