module Strategy.Cocoapods.PodfileLock (
  analyze',
  buildGraph,
  buildGraphStatic,
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
  (<||>),
 )
import Data.Aeson (FromJSON (parseJSON))
import Data.Aeson qualified as JSON
import Data.Aeson.Types (FromJSONKey)
import Data.Char qualified as C
import Data.Foldable (asum, traverse_)
import Data.Functor (void)
import Data.HashMap.Lazy qualified as HashMap (toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.String.Conversion (toString, toText)
import Data.Text (Text)
import Data.Void (Void)
import Data.Yaml ((.:), (.:?))
import Data.Yaml qualified as Yaml
import DepTypes (DepType (GitType, PodType), Dependency (..), VerConstraint (CEq))
import Effect.Exec (AllowErr (..), Command (..), Exec, exec, execJson)
import Effect.Grapher (LabeledGrapher, direct, edge, label, withLabeling)
import Effect.ReadFS (ReadFS, readContentsYaml)
import Graphing (Graphing, gtraverse)
import Options.Applicative (Alternative ((<|>)))
import Path (Abs, Dir, File, Path, parent)
import System.FilePath ((<.>), (</>))
import Text.Megaparsec (Parsec, between, empty, errorBundlePretty, parse, some, takeWhile1P, takeWhileP)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Text.Extra (showT)

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m, Has Exec sig m) => Path Abs File -> m (Graphing Dependency)
analyze' file = do
  podfileLock <- readContentsYaml file
  context "Building dependency graph" $ buildGraph file podfileLock

type PodfileGrapher = LabeledGrapher PodfilePkg PodfileLabel

newtype PodfilePkg = PodfilePkg {pkgName :: Text}
  deriving (Eq, Ord, Show)

newtype PodfileLabel
  = PodfileVersion Text
  deriving (Eq, Ord, Show)

buildGraphStatic :: PodLock -> Graphing Dependency
buildGraphStatic PodLock{lockPods, lockDeps, lockExternalSources} = staticGraph
  where
    staticGraph :: Graphing Dependency
    staticGraph =
      run . withLabeling (toDependency lockExternalSources) $ do
        -- Add direct dependencies.
        traverse_ (direct . PodfilePkg . depName) lockDeps
        -- Add transitive graph.
        traverse_ addSpec lockPods

    toDependency :: Map Text ExternalSource -> PodfilePkg -> Set PodfileLabel -> Dependency
    toDependency externals PodfilePkg{pkgName} = foldr applyLabel start
      where
        start :: Dependency
        start =
          Dependency
            { dependencyType = depType
            , dependencyName = depName
            , dependencyVersion = Nothing
            , dependencyLocations = []
            , dependencyEnvironments = mempty
            , dependencyTags = Map.empty
            }

        depType :: DepType
        depType = case Map.lookup pkgName externals of
          Just (ExternalGitType _) -> GitType
          _ -> PodType

        depName :: Text
        depName = case Map.lookup pkgName externals of
          Just (ExternalGitType ExternalGitSource{urlOf}) -> urlOf
          _ -> pkgName

        applyLabel :: PodfileLabel -> Dependency -> Dependency
        applyLabel (PodfileVersion ver) dep =
          dep
            { dependencyVersion =
                CEq <$> case Map.lookup pkgName externals of
                  Just (ExternalGitType ExternalGitSource{tagOf, commitOf, branchOf}) -> asum [tagOf, commitOf, branchOf]
                  _ -> Just ver
            }

    addSpec :: Has PodfileGrapher sig m => Pod -> m ()
    addSpec Pod{podName, podVersion, podDeps} = do
      -- Add edges to transitive dependencies.
      traverse_ (edge pkg . PodfilePkg . depName) podDeps
      -- Label pods with versions.
      label pkg $ PodfileVersion podVersion
      where
        pkg = PodfilePkg podName

buildGraph ::
  (Has ReadFS sig m, Has Exec sig m, Has Diagnostics sig m) =>
  Path Abs File ->
  PodLock ->
  m (Graphing Dependency)
buildGraph lockFilePath lockFile@PodLock{lockExternalSources} = do
  -- If `pod` is not installed, then the static graph is the best we can do.
  podVersion <- exec lockFileDir Command{cmdName = "pod", cmdArgs = ["--version"], cmdAllowErr = Never}
  case podVersion of
    Left _ -> pure staticGraph
    Right _ -> do
      -- If `pod` _is_ installed, then we can shell out to `pod ipc spec` to
      -- convert locally vendored `.podspec` files to JSON and read them for
      -- their Git locators. We do this because locally vendored `.podspec`
      -- files are often vendored specifically because they are not available on
      -- the Cocoapods registry.
      gtraverse replaceVendoredPods staticGraph
  where
    staticGraph :: Graphing Dependency
    staticGraph = buildGraphStatic lockFile

    lockFileDir :: Path Abs Dir
    lockFileDir = parent lockFilePath

    replaceVendoredPods :: (Has Exec sig m, Has Diagnostics sig m) => Dependency -> m Dependency
    replaceVendoredPods d@Dependency{dependencyType, dependencyName} = case dependencyType of
      PodType -> case Map.lookup dependencyName lockExternalSources of
        Just es -> case es of
          ExternalPodSpec podSpecPath ->
            readPodSpecAt podSpecPath
              <||> pure d
          ExternalPath podPath ->
            readPodSpecAt (toText $ toString podPath </> (toString dependencyName <.> "podspec"))
              <||> pure d
          ExternalGitType _ -> pure d
          ExternalOtherType -> pure d
        Nothing -> pure d
      _ -> pure d

    readPodSpecAt :: (Has Exec sig m, Has Diagnostics sig m) => Text -> m Dependency
    readPodSpecAt podSpecPath = context ("Resolving vendored podspec at " <> showT podSpecPath) $ do
      (PodSpecJSON ExternalGitSource{urlOf, tagOf, commitOf, branchOf}) <-
        execJson
          lockFileDir
          Command
            { cmdName = "pod"
            , cmdArgs = ["ipc", "spec", podSpecPath]
            , cmdAllowErr = Never
            }
      pure
        Dependency
          { dependencyType = GitType
          , dependencyName = urlOf
          , dependencyVersion = CEq <$> asum [tagOf, commitOf, branchOf]
          , dependencyLocations = []
          , dependencyEnvironments = mempty
          , dependencyTags = Map.empty
          }

type Parser = Parsec Void Text

data Section
  = PodSection [Pod]
  | DependencySection [Dep]
  deriving (Eq, Ord, Show)

data ExternalSource
  = ExternalGitType ExternalGitSource
  | ExternalPodSpec Text -- This is always a `.podspec` filepath.
  | ExternalPath Text -- This is a directory containing a `.podspec` with the Pod's name.
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

newtype Dep = Dep
  { depName :: Text
  }
  deriving (Eq, Ord, Show, FromJSONKey)

data Pod = Pod
  { podName :: Text
  , podVersion :: Text
  , podDeps :: [Dep]
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
    ( ExternalGitType
        <$> ( ExternalGitSource
                <$> obj .: ":git"
                <*> obj .:? ":tag"
                <*> obj .:? ":commit"
                <*> obj .:? ":branch"
            )
    )
      -- We don't parse these as filepaths because `path`'s parsing functions do
      -- not accept `..`.
      <|> (ExternalPodSpec <$> obj .: ":podspec")
      <|> (ExternalPath <$> obj .: ":path")
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

newtype PodSpecJSON = PodSpecJSON ExternalGitSource

instance FromJSON PodSpecJSON where
  parseJSON = JSON.withObject "PodSpecJSON" $ \o ->
    PodSpecJSON <$> do
      source <- o .: "source"
      ExternalGitSource
        <$> source .: "git"
        <*> source .:? "tag"
        <*> source .:? "commit"
        <*> source .:? "branch"
