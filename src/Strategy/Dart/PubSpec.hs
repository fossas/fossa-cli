module Strategy.Dart.PubSpec (
  analyzePubSpecFile,

  -- * for testing
  buildGraph,
  PubSpecContent (..),
  PubSpecDepSource (..),
  PubSpecDepHostedSource (..),
  PubSpecDepGitSource (..),
  PubSpecDepSdkSource (..),
  PubSpecDepPathSource (..),
) where

import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, context)
import Data.Foldable (asum, for_)
import Data.Map (Map, toList)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Yaml (FromJSON (parseJSON), (.:), (.:?))
import Data.Yaml qualified as Yaml
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (GitType, PubType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.Logger (Logger, Pretty (pretty), logDebug)
import Effect.ReadFS (Has, ReadFS, readContentsYaml)
import Graphing (Graphing, directs, induceJust)
import Path
import Strategy.Dart.PubSpecLock (PackageName (..))
import Types (GraphBreadth (..))

data PubSpecContent = PubSpecContent
  { pubSpecDependencies :: Maybe (Map PackageName PubSpecDepSource)
  , pubSpecDevDependencies :: Maybe (Map PackageName PubSpecDepSource)
  , pubSpecDependenciesOverrides :: Maybe (Map PackageName PubSpecDepSource)
  }
  deriving (Show, Eq, Ord)

newtype PubSpecDepSdkSource = PubSpecDepSdkSource {sdkName :: Text} deriving (Show, Eq, Ord)
newtype PubSpecDepPathSource = PubSpecDepPathSource {hostPath :: Text} deriving (Show, Eq, Ord)
data PubSpecDepHostedSource = PubSpecDepHostedSource
  { version :: Maybe Text
  , hostedName :: Maybe Text
  , hostedUrl :: Maybe Text
  }
  deriving (Show, Eq, Ord)
data PubSpecDepGitSource = PubSpecDepGitSource
  { gitRef :: Maybe Text
  , gitUrl :: Text
  }
  deriving (Show, Eq, Ord)

data PubSpecDepSource
  = HostedSource PubSpecDepHostedSource
  | GitSource PubSpecDepGitSource
  | SdkSource PubSpecDepSdkSource
  | PathSource PubSpecDepPathSource
  deriving (Show, Eq, Ord)

instance FromJSON PubSpecContent where
  parseJSON = Yaml.withObject "pubspec.yaml content" $ \o -> do
    dependencies <- o .:? "dependencies"
    devDependencies <- o .:? "dev_dependencies"
    depOverrides <- o .:? "dependency_overrides"
    pure $ PubSpecContent dependencies devDependencies depOverrides

instance FromJSON PubSpecDepSource where
  parseJSON (Yaml.String s) = pure $ HostedSource $ PubSpecDepHostedSource (Just s) Nothing Nothing
  parseJSON (Yaml.Object o) =
    asum
      [ parseHostedSource o
      , parseGitSource o
      , parseSdkSource o
      , parsePathSource o
      ]
    where
      parseHostedSource :: Yaml.Object -> Yaml.Parser PubSpecDepSource
      parseHostedSource ho =
        HostedSource
          <$> ( PubSpecDepHostedSource
                  <$> ho .: "version"
                  <*> ho .: "hosted" |> "name"
                  <*> ho .: "hosted" |> "url"
              )

      parseGitSource :: Yaml.Object -> Yaml.Parser PubSpecDepSource
      parseGitSource go =
        GitSource
          <$> ( PubSpecDepGitSource
                  <$> go .: "git" |> "ref"
                  <*> go .: "git" |> "url"
                  <|> PubSpecDepGitSource Nothing
                    <$> go .: "git"
              )
      parseSdkSource :: Yaml.Object -> Yaml.Parser PubSpecDepSource
      parseSdkSource so = SdkSource . PubSpecDepSdkSource <$> so .: "sdk"

      parsePathSource :: Yaml.Object -> Yaml.Parser PubSpecDepSource
      parsePathSource po = PathSource . PubSpecDepPathSource <$> po .: "path"

      (|>) :: FromJSON a => Yaml.Parser Yaml.Object -> Text -> Yaml.Parser a
      (|>) parser key = do
        obj <- parser
        obj .: key
  parseJSON _ = fail "failed parsing pub package's source!"

toDependency :: DepEnvironment -> PackageName -> PubSpecDepSource -> Maybe Dependency
toDependency environment name (HostedSource (PubSpecDepHostedSource version _ url)) =
  Just
    Dependency
      { dependencyType = PubType
      , dependencyName = unPackageName name
      , dependencyVersion = CEq <$> version
      , dependencyLocations = maybeToList url
      , dependencyEnvironments = Set.singleton environment
      , dependencyTags = Map.empty
      }
toDependency environment _ (GitSource (PubSpecDepGitSource gitRef gitUrl)) =
  Just
    Dependency
      { dependencyType = GitType
      , dependencyName = gitUrl
      , dependencyVersion = CEq <$> gitRef
      , dependencyLocations = []
      , dependencyEnvironments = Set.singleton environment
      , dependencyTags = Map.empty
      }
toDependency _ _ (SdkSource _) = Nothing
toDependency _ _ (PathSource _) = Nothing

-- | Updates all of 'A''s value, with 'B' value, when key of 'A' matches with key of 'B'.
update :: Map PackageName PubSpecDepSource -> Map PackageName PubSpecDepSource -> Map PackageName PubSpecDepSource
update a b = Map.union (Map.intersection b a) a

buildGraph :: PubSpecContent -> Graphing.Graphing Dependency
buildGraph specContent = induceJust allDependencies
  where
    -- In pub manifest, dependency can be overriden.
    -- Ref: https://dart.dev/tools/pub/dependencies#dependency-overrides
    supersededDependencies :: Map PackageName PubSpecDepSource
    supersededDependencies = fromMaybe Map.empty $ pubSpecDependenciesOverrides specContent

    dependencies :: Map PackageName PubSpecDepSource
    dependencies = update (fromMaybe Map.empty $ pubSpecDependencies specContent) supersededDependencies

    devDependencies :: Map PackageName PubSpecDepSource
    devDependencies = update (fromMaybe Map.empty $ pubSpecDevDependencies specContent) supersededDependencies

    allDependencies :: Graphing (Maybe Dependency)
    allDependencies =
      directs (map (uncurry $ toDependency EnvProduction) $ toList dependencies)
        <> directs (map (uncurry $ toDependency EnvDevelopment) $ toList devDependencies)

logIgnoredPackages :: Has Logger sig m => PubSpecContent -> m ()
logIgnoredPackages specContent = for_ notSupportedPackagesMsgs (logDebug . pretty)
  where
    notSupportedPackagesMsgs :: [Text]
    notSupportedPackagesMsgs = map (<> " : ignored in analyses. Dependency's source is not supported!") notSupportedPackages

    notSupportedPackages :: [Text]
    notSupportedPackages =
      notSupportedOf (pubSpecDevDependencies specContent)
        ++ notSupportedOf (pubSpecDependencies specContent)

    notSupportedOf :: Maybe (Map PackageName PubSpecDepSource) -> [Text]
    notSupportedOf dependencies =
      unPackageName . fst
        <$> (toList . notSupported)
          (update (fromMaybe Map.empty dependencies) supersededDependencies)

    supersededDependencies :: Map PackageName PubSpecDepSource
    supersededDependencies = fromMaybe Map.empty $ pubSpecDependenciesOverrides specContent

    notSupported :: Map k PubSpecDepSource -> Map k PubSpecDepSource
    notSupported = Map.filter $ not . isSupported

    isSupported :: PubSpecDepSource -> Bool
    isSupported (GitSource _) = True
    isSupported (HostedSource _) = True
    isSupported (SdkSource _) = False
    isSupported (PathSource _) = False

analyzePubSpecFile ::
  (Has ReadFS sig m, Has Diagnostics sig m, Has Logger sig m) =>
  Path Abs File ->
  m (Graphing Dependency, GraphBreadth)
analyzePubSpecFile specFile = do
  specContent <- context "Reading pubspec.yaml" $ readContentsYaml specFile
  _ <- logIgnoredPackages specContent
  context "building graphing from pubspec.yaml" $ pure (buildGraph specContent, Partial)
