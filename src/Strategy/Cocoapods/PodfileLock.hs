module Strategy.Cocoapods.PodfileLock (
  analyze',
  buildGraph,
  PodLock (..),
  Dep (..),
  Pod (..),
  Section (..),
  toSections,
) where

import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  context,
  run,
 )
import Data.Aeson (FromJSON (parseJSON), (.:))
import Data.Char qualified as C
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.HashMap.Lazy qualified as HashMap (toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text (Text)
import Data.Void (Void)
import Data.Yaml qualified as Yaml
import DepTypes (DepType (PodType), Dependency (..), VerConstraint (CEq))
import Effect.Grapher (LabeledGrapher, direct, edge, label, withLabeling)
import Effect.ReadFS (ReadFS, readContentsYaml)
import Graphing (Graphing)
import Path
import Text.Megaparsec (MonadParsec (takeWhileP), Parsec, between, empty, errorBundlePretty, parse, some, takeWhile1P)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze' file = do
  podfileLock <- toSections <$> readContentsYaml file
  context "Building dependency graph" $ pure (buildGraph podfileLock)

newtype PodfilePkg = PodfilePkg {pkgName :: Text}
  deriving (Eq, Ord, Show)

type PodfileGrapher = LabeledGrapher PodfilePkg PodfileLabel

newtype PodfileLabel
  = PodfileVersion Text
  deriving (Eq, Ord, Show)

toDependency :: PodfilePkg -> Set PodfileLabel -> Dependency
toDependency pkg = foldr applyLabel start
  where
    start :: Dependency
    start =
      Dependency
        { dependencyType = PodType
        , dependencyName = pkgName pkg
        , dependencyVersion = Nothing
        , dependencyLocations = []
        , dependencyEnvironments = []
        , dependencyTags = Map.empty
        }

    applyLabel :: PodfileLabel -> Dependency -> Dependency
    applyLabel (PodfileVersion ver) dep = dep{dependencyVersion = Just (CEq ver)}

buildGraph :: [Section] -> Graphing Dependency
buildGraph sections =
  run . withLabeling toDependency $
    traverse_ addSection sections
  where
    addSection :: Has PodfileGrapher sig m => Section -> m ()
    addSection (DependencySection deps) = traverse_ (direct . PodfilePkg . depName) deps
    addSection (PodSection pods) = traverse_ addSpec pods

    addSpec :: Has PodfileGrapher sig m => Pod -> m ()
    addSpec pod = do
      let pkg = PodfilePkg (podName pod)
      -- add edges between spec and specdeps
      traverse_ (edge pkg . PodfilePkg . depName) (podSpecs pod)
      -- add a label for version
      label pkg (PodfileVersion (podVersion pod))

type Parser = Parsec Void Text

data Section
  = PodSection [Pod]
  | DependencySection [Dep]
  deriving (Eq, Ord, Show)

data PodLock = PodLock
  { lockPods :: [Pod]
  , lockDeps :: [Dep]
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
  deriving (Eq, Ord, Show)

data SourceDep = SourceDep
  { sDepName :: Text
  , tags :: Map Text Text
  }
  deriving (Eq, Ord, Show)

data Pod = Pod
  { podName :: Text
  , podVersion :: Text
  , podSpecs :: [Dep]
  }
  deriving (Eq, Ord, Show)

data Remote = Remote
  { remoteLocation :: Text
  , remoteDeps :: [Dep]
  }
  deriving (Eq, Ord, Show)

parseName :: Parser Text
parseName = lexeme $ takeWhile1P (Just "dep") (not . C.isSpace)

parseVersion :: Parser Text
parseVersion = between (char '(') (char ')') $ lexeme (takeWhileP (Just "version") (/= ')'))

parseNameAndVersion :: Parser (Text, Text)
parseNameAndVersion = (,) <$> parseName <*> parseVersion

instance FromJSON PodLock where
  parseJSON = Yaml.withObject "Podfile.lock content" $ \obj -> do
    PodLock <$> obj .: "PODS"
      <*> obj .: "DEPENDENCIES"

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
