module Strategy.Python.Poetry.PoetryLock (
  PoetryLock (..),
  PoetryLockPackage (..),
  PackageName (..),
  PoetryLockPackageSource (..),
  PoetryLockDependencySpec (..),

  -- * for testing only
  ObjectVersion (..),
  PoetryMetadata (..),
) where

import Data.Map (Map)
import Data.Text (Text)
import Toml qualified
import Toml.Schema qualified

-- | Represents the content of the poetry lock file.
data PoetryLock = PoetryLock
  { poetryLockPackages :: [PoetryLockPackage]
  , poetryLockMetadata :: PoetryMetadata
  }
  deriving (Eq, Ord, Show)

instance Toml.Schema.FromValue PoetryLock where
  fromValue =
    Toml.Schema.parseTableFromValue $
      PoetryLock
        <$> Toml.Schema.reqKey "package"
        <*> Toml.Schema.reqKey "metadata"

newtype PackageName = PackageName {unPackageName :: Text} deriving (Eq, Ord, Show)

-- | Metadata of poetry lock file.
data PoetryMetadata = PoetryMetadata
  { poetryMetadataLockVersion :: Text
  , poetryMetadataContentHash :: Text
  , poetryMetadataPythonVersions :: Text
  }
  deriving (Eq, Ord, Show)

instance Toml.Schema.FromValue PoetryMetadata where
  fromValue =
    Toml.Schema.parseTableFromValue $
      PoetryMetadata
        <$> Toml.Schema.reqKey "lock-version"
        <*> Toml.Schema.reqKey "content-hash"
        <*> Toml.Schema.reqKey "python-versions"

-- | A PoetryLockPackageSource represents [package.source] field in poetry.lock.
-- Source indicates from where the package was retrieved.
data PoetryLockPackageSource = PoetryLockPackageSource
  { poetryLockPackageSourceType :: Text
  , poetryLockPackageSourceUrl :: Text
  , poetryLockPackageSourceReference :: Maybe Text
  , poetryLockPackageSourceResolvedReference :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance Toml.Schema.FromValue PoetryLockPackageSource where
  fromValue =
    Toml.Schema.parseTableFromValue $
      PoetryLockPackageSource
        <$> Toml.Schema.reqKey "type"
        <*> Toml.Schema.reqKey "url"
        <*> Toml.Schema.optKey "reference"
        <*> Toml.Schema.optKey "resolved_reference"

-- | Poetry package entry found in poetry lock file.
data PoetryLockPackage = PoetryLockPackage
  { poetryLockPackageName :: PackageName
  , poetryLockPackageVersion :: Text
  , poetryLockPackageCategory :: Maybe Text
  , poetryLockPackageOptional :: Bool
  , poetryLockPackagePythonVersions :: Text
  , poetryLockPackageDependencies :: Map Text PoetryLockDependencySpec
  , poetryLockPackageSource :: Maybe PoetryLockPackageSource
  }
  deriving (Eq, Ord, Show)

instance Toml.Schema.FromValue PoetryLockPackage where
  fromValue =
    Toml.Schema.parseTableFromValue $
      (PoetryLockPackage . PackageName <$> Toml.Schema.reqKey "name")
        <*> Toml.Schema.reqKey "version"
        <*> Toml.Schema.optKey "category"
        <*> Toml.Schema.reqKey "optional"
        <*> Toml.Schema.reqKey "python-versions"
        <*> Toml.Schema.pickKey [Toml.Schema.Key "dependencies" Toml.Schema.fromValue, Toml.Schema.Else $ pure mempty]
        <*> Toml.Schema.optKey "source"

data PoetryLockDependencySpec
  = TextVersion Text
  | ObjectVersionSpec ObjectVersion
  | MultipleObjectVersionSpec [ObjectVersion]
  deriving (Eq, Ord, Show)

instance Toml.Schema.FromValue PoetryLockDependencySpec where
  fromValue (Toml.Schema.Text' _ t) = pure $ TextVersion t
  fromValue v@(Toml.Schema.Table' _ _) = ObjectVersionSpec <$> Toml.Schema.fromValue v
  fromValue v@Toml.Schema.List'{} = MultipleObjectVersionSpec <$> Toml.Schema.fromValue v
  fromValue v = Toml.Schema.failAt (Toml.valueAnn v) $ "invalid poetry dependency spec " <> Toml.valueType v

newtype ObjectVersion = ObjectVersion
  { unObjectVersion :: Text
  }
  deriving (Eq, Ord, Show)

instance Toml.Schema.FromValue ObjectVersion where
  fromValue =
    Toml.Schema.parseTableFromValue $
      ObjectVersion
        <$> Toml.Schema.reqKey "version"
