module Strategy.Dart.PubSpecLock (
  analyzePubLockFile,
  PackageName (..),
  PubLockContent (..),
  PubLockPackageMetadata (..),
  logIgnoredPackages,

  -- * for testing
  buildGraph,
  PubDepSource (..),
  PubLockPackageHostedSource (..),
  PubLockPackageGitSource (..),
  PubLockPackageSdkSource (..),
  PubLockPackagePathSource (..),
  toDependency,
  isSupported,
) where

import Control.Carrier.Simple (Has)
import Control.Effect.Diagnostics (Diagnostics, context)
import Data.Aeson.Types (
  FromJSONKey,
 )
import Data.Aeson.Types qualified as AesonTypes
import Data.Foldable (asum, for_)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Yaml (FromJSON (parseJSON), (.:), (.:?))
import Data.Yaml qualified as Yaml
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (GitType, PubType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.Logger (Logger, Pretty (pretty), logDebug)
import Effect.ReadFS (ReadFS, readContentsYaml)
import GHC.Generics (Generic)
import Graphing (Graphing, deeps, directs)
import Path
import Types (GraphBreadth (..))

newtype PackageName = PackageName {unPackageName :: Text} deriving (Show, Eq, Ord, FromJSONKey)
newtype PubLockContent = PubLockContent {packages :: Map PackageName PubLockPackageMetadata} deriving (Show, Eq, Ord)

-- | Represents Pub Dependency's metadata from lock file.
data PubLockPackageMetadata = PubLockPackageMetadata
  { pubLockPackageIsDirect :: Bool
  , pubLockPackageSource :: PubDepSource
  , pubLockPackageVersion :: Maybe Text
  , pubLockPackageEnvironment :: [DepEnvironment]
  }
  deriving (Generic, Show, Eq, Ord)

newtype PubLockPackageSdkSource = PubLockPackageSdkSource {sdkName :: Text} deriving (Show, Eq, Ord)
newtype PubLockPackagePathSource = PubLockPackagePathSource {hostPath :: Text} deriving (Show, Eq, Ord)
data PubLockPackageGitSource = PubLockPackageGitSource {gitUrl :: Text, ref :: Text} deriving (Show, Eq, Ord)
data PubLockPackageHostedSource = PubLockPackageHostedSource {hostPackageName :: Maybe Text, hostUrl :: Maybe Text} deriving (Show, Eq, Ord)

-- | Represents Pub Dependency's source.
data PubDepSource
  = SdkSource PubLockPackageSdkSource
  | GitSource PubLockPackageGitSource
  | HostedSource PubLockPackageHostedSource
  | PathSource PubLockPackagePathSource
  deriving (Show, Eq, Ord)

instance FromJSON PackageName where
  parseJSON (AesonTypes.String packageName) = pure $ PackageName packageName
  parseJSON _ = fail "failed to parse package's name"

instance FromJSON PubLockContent where
  parseJSON = Yaml.withObject "pubspec.lock content" $ \o -> do
    packages <- o .: "packages"
    pure $ PubLockContent packages

instance FromJSON PubLockPackageMetadata where
  parseJSON = Yaml.withObject "pubspec.lock content package" $ \o -> do
    pubLockPackageIsDirect <- isDirect <$> o .: "dependency"
    pubLockPackageSource <- o .: "description"
    pubLockPackageVersion <- o .:? "version"
    pubLockPackageEnvironment <- getEnvironment <$> o .: "dependency"
    pure $ PubLockPackageMetadata pubLockPackageIsDirect pubLockPackageSource pubLockPackageVersion pubLockPackageEnvironment
    where
      isDirect :: Text -> Bool
      isDirect = Text.isInfixOf "direct"

      getEnvironment :: Text -> [DepEnvironment]
      getEnvironment candidate
        | Text.isInfixOf "main" candidate = [EnvProduction]
        | Text.isInfixOf "dev" candidate = [EnvDevelopment]
        | otherwise = []

instance FromJSON PubDepSource where
  parseJSON (Yaml.String v) = pure $ SdkSource $ PubLockPackageSdkSource v
  parseJSON (Yaml.Object o) =
    asum
      [ parseGitSource o
      , parsePathSource o
      , parseHostedSource o
      ]
    where
      parseHostedSource :: Yaml.Object -> Yaml.Parser PubDepSource
      parseHostedSource ho = HostedSource <$> (PubLockPackageHostedSource <$> ho .:? "name" <*> ho .:? "url")

      parseGitSource :: Yaml.Object -> Yaml.Parser PubDepSource
      parseGitSource go = GitSource <$> (PubLockPackageGitSource <$> go .: "url" <*> go .: "ref")

      parsePathSource :: Yaml.Object -> Yaml.Parser PubDepSource
      parsePathSource po = PathSource . PubLockPackagePathSource <$> po .: "path"
  parseJSON _ = fail "could not parse pub dependency's metadata"

isSupported :: PubLockPackageMetadata -> Bool
isSupported meta =
  case pubLockPackageSource meta of
    SdkSource _ -> False
    PathSource _ -> False
    GitSource _ -> True
    HostedSource _ -> True

-- Transforms Package into Dependency.
toDependency :: PackageName -> PubLockPackageMetadata -> Dependency
toDependency pkg meta =
  Dependency
    { dependencyType = depType
    , dependencyName = depName
    , dependencyVersion = depVersion
    , dependencyLocations = depLocation
    , dependencyEnvironments = Set.fromList $ pubLockPackageEnvironment meta
    , dependencyTags = Map.empty
    }
  where
    depType :: DepType
    depType = case pubLockPackageSource meta of
      GitSource{} -> GitType
      _ -> PubType

    depName :: Text
    depName = case pubLockPackageSource meta of
      GitSource (PubLockPackageGitSource gitUrl _) -> gitUrl
      _ -> unPackageName pkg

    depVersion :: Maybe VerConstraint
    depVersion = case pubLockPackageSource meta of
      GitSource (PubLockPackageGitSource _ ref) -> Just $ CEq ref
      _ -> CEq <$> pubLockPackageVersion meta

    depLocation :: [Text]
    depLocation = case pubLockPackageSource meta of
      HostedSource (PubLockPackageHostedSource _ (Just hostUrl)) -> [hostUrl]
      _ -> []

-- | Builds the dependency graphing from pubspec.lock's content.
-- Edges are not reported.
buildGraph :: PubLockContent -> Graphing Dependency
buildGraph lockContent = graphOfDirects <> graphOfTransitives
  where
    supportedPackages :: Map PackageName PubLockPackageMetadata
    supportedPackages = Map.filter isSupported $ packages lockContent

    getDependencies :: (PubLockPackageMetadata -> Bool) -> [Dependency]
    getDependencies f = Map.elems $ Map.mapWithKey toDependency $ Map.filter f supportedPackages

    graphOfTransitives :: Graphing Dependency
    graphOfTransitives = deeps $ getDependencies $ not . pubLockPackageIsDirect

    graphOfDirects :: Graphing Dependency
    graphOfDirects = directs $ getDependencies pubLockPackageIsDirect

logIgnoredPackages :: Has Logger sig m => PubLockContent -> m ()
logIgnoredPackages lockContent = for_ notSupportedDependenciesMsgs (logDebug . pretty)
  where
    notSupportedDependenciesMsgs :: [Text]
    notSupportedDependenciesMsgs = map (<> " : ignored in analyses. Dependency's source is not supported!") notSupportedPackages

    notSupportedPackages :: [Text]
    notSupportedPackages = map (unPackageName . fst) (Map.toList $ Map.filter isSupported $ packages lockContent)

analyzePubLockFile ::
  (Has ReadFS sig m, Has Diagnostics sig m, Has Logger sig m) =>
  Path Abs File ->
  m (Graphing Dependency, GraphBreadth)
analyzePubLockFile lockFile = do
  lockContents <- context "Reading pubspec.lock" $ readContentsYaml lockFile
  _ <- logIgnoredPackages lockContents
  context "building graphing from pubspec.lock only" $ pure (buildGraph lockContents, Partial)
