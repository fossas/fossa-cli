module Strategy.Cocoapods.PodfileLock (
  analyze',
  buildGraph,
  PodLock (..),
  Dep (..),
  Pod (..),
  ExternalSource (..),
  ExternalGitSource (..),
) where

import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  context,
  run,
 )
import Data.Aeson (FromJSON (parseJSON))
import Data.Aeson.Types (FromJSONKey)
import Data.Char qualified as C
import Data.Foldable (asum, traverse_)
import Data.Functor (void)
import Data.HashMap.Lazy qualified as HashMap (toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text (Text)
import Data.Void (Void)
import Data.Yaml ((.:), (.:?))
import Data.Yaml qualified as Yaml
import DepTypes (DepType (GitType, PodType), Dependency (..), VerConstraint (CEq))
import Effect.Grapher (LabeledGrapher, direct, edge, label, withLabeling)
import Effect.ReadFS (ReadFS, readContentsYaml)
import Graphing (Graphing)
import Options.Applicative (Alternative ((<|>)))
import Path
import Text.Megaparsec (MonadParsec (takeWhileP), Parsec, between, empty, errorBundlePretty, parse, some, takeWhile1P)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze' file = do
  podfileLock <- readContentsYaml file
  context "Building dependency graph" $ pure (buildGraph podfileLock)

newtype PodfilePkg = PodfilePkg {pkgName :: Text}
  deriving (Eq, Ord, Show)

type PodfileGrapher = LabeledGrapher PodfilePkg PodfileLabel

newtype PodfileLabel
  = PodfileVersion (Maybe Text)
  deriving (Eq, Ord, Show)

toDependency :: Map Text ExternalSource -> PodfilePkg -> Set PodfileLabel -> Dependency
toDependency externalSrc pkg = foldr applyLabel start
  where
    start :: Dependency
    start =
      Dependency
        { dependencyType = depType
        , dependencyName = depName
        , dependencyVersion = Nothing
        , dependencyLocations = []
        , dependencyEnvironments = []
        , dependencyTags = Map.empty
        }

    depType :: DepType
    depType = case Map.lookup (pkgName pkg) externalSrc of
      Just (ExternalGitType _) -> GitType
      _ -> PodType

    depName :: Text
    depName = case Map.lookup (pkgName pkg) externalSrc of
      Just (ExternalGitType gitSrc) -> urlOf gitSrc
      _ -> pkgName pkg

    applyLabel :: PodfileLabel -> Dependency -> Dependency
    applyLabel (PodfileVersion ver) dep = dep{dependencyVersion = CEq <$> ver}

buildGraph :: PodLock -> Graphing Dependency
buildGraph lockContent =
  run . withLabeling (toDependency $ lockExternalSources lockContent) $
    traverse_ addSection sections
  where
    sections :: [Section]
    sections = toSections lockContent

    addSection :: Has PodfileGrapher sig m => Section -> m ()
    addSection (DependencySection deps) = traverse_ (direct . PodfilePkg . depName) deps
    addSection (PodSection pods) = traverse_ addSpec pods

    addSpec :: Has PodfileGrapher sig m => Pod -> m ()
    addSpec pod = do
      let pkg = PodfilePkg (podName pod)
      let pkgVersion = case Map.lookup (podName pod) (lockExternalSources lockContent) of
            Just (ExternalGitType src) ->
              asum
                [ tagOf src
                , commitOf src
                , branchOf src
                ]
            _ -> Just $ podVersion pod

      -- add edges between spec and specdeps
      traverse_ (edge pkg . PodfilePkg . depName) $ podSpecs pod
      -- add a label for version
      label pkg $ PodfileVersion pkgVersion

type Parser = Parsec Void Text

data Section
  = PodSection [Pod]
  | DependencySection [Dep]
  deriving (Eq, Ord, Show)

data ExternalSource
  = ExternalGitType ExternalGitSource
  | ExternalOtherType
  deriving (Show, Eq, Ord)

data ExternalGitSource = ExternalGitSource
  { urlOf :: Text
  , tagOf :: Maybe Text
  , commitOf :: Maybe Text
  , branchOf :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data PodLock = PodLock
  { lockPods :: [Pod]
  , lockDeps :: [Dep]
  , lockExternalSources :: Map Text ExternalSource
  }
  deriving (Show, Eq, Ord)

toSections :: PodLock -> [Section]
toSections lockContent =
  [ PodSection $ lockPods lockContent
  , DependencySection $ lockDeps lockContent
  ]

newtype Dep = Dep
  { depName :: Text
  }
  deriving (Eq, Ord, Show, FromJSONKey)

data Pod = Pod
  { podName :: Text
  , podVersion :: Text
  , podSpecs :: [Dep]
  }
  deriving (Eq, Ord, Show)

parseName :: Parser Text
parseName = lexeme $ takeWhile1P (Just "dep") (not . C.isSpace)

parseVersion :: Parser Text
parseVersion = between (char '(') (char ')') $ lexeme (takeWhileP (Just "version") (/= ')'))

parseNameAndVersion :: Parser (Text, Text)
parseNameAndVersion = (,) <$> parseName <*> parseVersion

instance FromJSON PodLock where
  parseJSON = Yaml.withObject "Podfile.lock content" $ \obj ->
    PodLock <$> obj .: "PODS"
      <*> obj .: "DEPENDENCIES"
      <*> obj .: "EXTERNAL SOURCES"

instance FromJSON Pod where
  parseJSON (Yaml.String p) = parserPod p Nothing
  parseJSON (Yaml.Object obj) = case HashMap.toList obj of
    [(podEntry, podDepsListing)] -> parserPod podEntry $ Just podDepsListing
    [] -> fail "Expected non empty list of dependencies, but received empty list"
    _ -> fail $ "Expected list of dependencies, but received: " <> show obj
  parseJSON notSupported = fail $ "Expected string, but received: " <> show notSupported

instance FromJSON Dep where
  parseJSON (Yaml.String d) = case parse parseName "" d of
    Left pErr -> fail $ show $ errorBundlePretty pErr
    Right name -> pure $ Dep name
  parseJSON notSupported = fail $ "Expected string, but received: " <> show notSupported

instance FromJSON ExternalSource where
  parseJSON = Yaml.withObject "External source" $ \obj ->
    do
      ExternalGitType
        <$> ( ExternalGitSource
                <$> obj .: ":git"
                <*> obj .:? ":tag"
                <*> obj .:? ":commit"
                <*> obj .:? ":branch"
            )
      <|> pure ExternalOtherType

parserPod :: Text -> Maybe Yaml.Value -> Yaml.Parser Pod
parserPod query deps = case parse parseNameAndVersion "" query of
  Left pErr -> fail $ show $ errorBundlePretty pErr
  Right (name, version) -> case deps of
    Nothing -> pure $ Pod name version []
    Just podDeps -> Pod name version <$> parseJSON @[Dep] podDeps

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space (void $ some (char ' ')) empty empty
