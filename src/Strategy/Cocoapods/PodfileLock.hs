module Strategy.Cocoapods.PodfileLock (
  analyze',
  analyzeStatic,
  buildGraph,
  buildGraphStatic,
  PodLock (..),
  Dep (..),
  Pod (..),
  ExternalSource (..),
  ExternalGitSource (..),

  -- * for testing
  PodsSpecJSONSubSpec (..),
  allSubspecs,
) where

import Control.Carrier.State.Strict (evalState)
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  context,
  fatalText,
  fromEither,
  run,
  (<||>),
 )
import Control.Effect.State (State, get, put)
import Control.Monad (when)
import Data.Aeson (FromJSON (parseJSON), (.!=))
import Data.Aeson qualified as JSON
import Data.Aeson.KeyMap qualified as Object
import Data.Aeson.Types (FromJSONKey)
import Data.Bifunctor (first)
import Data.Char qualified as C
import Data.Foldable (asum, find, traverse_)
import Data.Functor (void)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.SemVer qualified as SemVer
import Data.SemVer.Internal (Version (..))
import Data.Set (Set)
import Data.String.Conversion (decodeUtf8, toString, toText)
import Data.Text (Text, null, strip)
import Data.Text qualified as Text
import Data.Text.Extra (showT, splitOnceOn)
import Data.Void (Void)
import Data.Yaml ((.:), (.:?))
import Data.Yaml qualified as Yaml
import DepTypes (DepType (GitType, PodType), Dependency (..), VerConstraint (CEq))
import Effect.Exec (AllowErr (..), Command (..), Exec, exec, execJson)
import Effect.Grapher (LabeledGrapher, direct, edge, label, withLabeling)
import Effect.Logger (Logger, Pretty (pretty), logDebug)
import Effect.ReadFS (ReadFS, readContentsYaml)
import Graphing (Graphing, gtraverse)
import Options.Applicative (Alternative ((<|>)))
import Path (Abs, Dir, File, Path, parent)
import System.FilePath ((<.>), (</>))
import Text.Megaparsec (Parsec, between, empty, errorBundlePretty, parse, some, takeWhile1P, takeWhileP)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m, Has Exec sig m, Has Logger sig m) => Path Abs File -> m (Graphing Dependency)
analyze' podfileLockFilepath = do
  context "Building dependency graph" $
    analyze podfileLockFilepath
      <||> analyzeStatic podfileLockFilepath

analyzeStatic :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyzeStatic podfileLockFilepath = do
  podfileLock <- readContentsYaml podfileLockFilepath
  pure (buildGraphStatic podfileLock)

analyze :: (Has ReadFS sig m, Has Diagnostics sig m, Has Exec sig m, Has Logger sig m) => Path Abs File -> m (Graphing Dependency)
analyze podfileLockFilepath = do
  podfileLock <- readContentsYaml podfileLockFilepath
  buildGraph podfileLockFilepath podfileLock

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

-- We use this cache to avoid repeated invocations since gtraverse may revisit
-- nodes. In practice, this saves quite a bit of time (e.g. on example projects,
-- it has historically turned ~2 minute analyses into ~15 seconds).
type PodSpecCache = Map Text PodSpecJSON

-- `pod ipc spec` returns the JSON representation of a fully-evaluated Podspec.
-- The `--silent` flag suppresses stdout printed by a podspec that calls
-- Pod::UI.puts, which can break JSON parsing.
--
-- See also:
--
-- - Command documentation: https://guides.cocoapods.org/terminal/commands.html#pod_ipc_spec
-- - Implemented in `pod`: https://github.com/CocoaPods/CocoaPods/blob/master/CHANGELOG.md#0290-2013-12-25
podIpcSpecCmd :: Text -> Command
podIpcSpecCmd podSpecPath =
  Command
    { cmdName = "pod"
    , cmdArgs = ["ipc", "spec", "--silent", podSpecPath]
    , cmdAllowErr = Never
    }

-- `pod --version` returns a semantic version.
podVersionCmd :: Command
podVersionCmd = Command{cmdName = "pod", cmdArgs = ["--version"], cmdAllowErr = Never}

buildGraph ::
  (Has ReadFS sig m, Has Exec sig m, Has Diagnostics sig m, Has Logger sig m) =>
  Path Abs File ->
  PodLock ->
  m (Graphing Dependency)
buildGraph lockFilePath lockFile@PodLock{lockExternalSources} = do
  -- If `pod` is not installed, then the static graph is the best we can do.
  podVersionOutput <- exec lockFileDir podVersionCmd
  case podVersionOutput of
    Left _ -> fatalText "could not analyze vendored pods: `pod --version` was not at least `0.29.0`"
    Right podVersionStdoutRaw -> do
      let podVersionStdout = strip $ decodeUtf8 podVersionStdoutRaw

      -- `pod ipc spec` is supported for versions 0.29.0 and above. See also:
      -- https://github.com/CocoaPods/CocoaPods/blob/master/CHANGELOG.md#0290-2013-12-25
      Version{_versionMajor, _versionMinor} <-
        fromEither $
          first ("could not parse `pod --version` output: " <>) $
            SemVer.fromText podVersionStdout
      when (_versionMajor == 0 && _versionMinor < 29) $
        fatalText $
          "`pod` version " <> podVersionStdout <> " does not support `pod ipc spec`"

      -- If `pod` _is_ installed, then we can shell out to `pod ipc spec` to
      -- convert locally vendored `.podspec` files to JSON and read them for
      -- their Git locators. We do this because locally vendored `.podspec`
      -- files are often vendored specifically because they are not available on
      -- the Cocoapods registry.
      evalState @PodSpecCache mempty $ gtraverse replaceVendoredPods staticGraph
  where
    staticGraph :: Graphing Dependency
    staticGraph = buildGraphStatic lockFile

    lockFileDir :: Path Abs Dir
    lockFileDir = parent lockFilePath

    toPodSpecPath :: Text -> Text -> Text
    toPodSpecPath podPath dependencyName = toText $ toString podPath </> (toString dependencyName <.> "podspec")

    replaceVendoredPods ::
      ( Has Exec sig m
      , Has Diagnostics sig m
      , Has (State PodSpecCache) sig m
      , Has Logger sig m
      ) =>
      Dependency ->
      m Dependency
    replaceVendoredPods d@Dependency{dependencyType, dependencyName} = case dependencyType of
      PodType -> case Map.lookup dependencyName lockExternalSources of
        Just es -> case es of
          ExternalPodSpec podSpecPath -> readPodSpecAt podSpecPath <||> pure d
          ExternalPath podPath -> readPodSpecAt (toPodSpecPath podPath dependencyName) <||> pure d
          ExternalGitType _ -> pure d
          ExternalOtherType -> pure d
        Nothing -> do
          -- Pod dependency can be included as part of externally sourced dependency's sub spec.
          -- Refer to: https://guides.cocoapods.org/syntax/podspec.html#subspec
          --
          -- To aptly resolve such dependencies, we infer candidate parent,
          -- and candidate sub spec from dependency's name. If and only if:
          --  * Candidate parent spec exists within external sources
          --  * Parent's pod spec has sub spec listed
          --  * Parent has git source
          --
          -- We replace this dependency with parent's git sourced dependency.
          let (parentSpec, childSpec) = splitOnceOn "/" dependencyName

          if Data.Text.null childSpec
            then pure d
            else do
              revisedDep <- case Map.lookup parentSpec lockExternalSources of
                Just (ExternalPodSpec podSpecPath) -> (readPodSubSpecSourceAt podSpecPath childSpec) <||> pure d
                Just (ExternalPath podPath) -> (readPodSubSpecSourceAt (toPodSpecPath podPath parentSpec) childSpec) <||> pure d
                _ -> pure d

              when (revisedDep /= d) $
                logDebug . pretty $
                  "Replaced " <> dependencyName <> " with " <> parentSpec <> ", since " <> dependencyName <> " is subspec of " <> parentSpec

              pure revisedDep
      _ -> pure d

    readPodSpecRaw ::
      ( Has Exec sig m
      , Has Diagnostics sig m
      , Has (State PodSpecCache) sig m
      ) =>
      Text ->
      m PodSpecJSON
    readPodSpecRaw podSpecPath = context ("Resolving vendored podspec at " <> showT podSpecPath) $ do
      podSpecCache <- get
      case Map.lookup podSpecPath podSpecCache of
        Just cached -> pure cached
        Nothing -> do
          podSpec <- execJson lockFileDir $ podIpcSpecCmd podSpecPath
          put $ Map.insert podSpecPath podSpec podSpecCache
          pure podSpec

    readPodSpecAt ::
      ( Has Exec sig m
      , Has Diagnostics sig m
      , Has (State PodSpecCache) sig m
      ) =>
      Text ->
      m Dependency
    readPodSpecAt podSpecPath = context ("Resolving vendored podspec at " <> showT podSpecPath) $ do
      podSpecJson <- readPodSpecRaw podSpecPath
      pure $ podSpecJsonToDependency podSpecJson

    podSpecJsonToDependency :: PodSpecJSON -> Dependency
    podSpecJsonToDependency (PodSpecJSON ExternalGitSource{urlOf, tagOf, commitOf, branchOf} _) =
      Dependency
        { dependencyType = GitType
        , dependencyName = urlOf
        , dependencyVersion = CEq <$> asum [tagOf, commitOf, branchOf]
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }

    readPodSubSpecSourceAt ::
      ( Has Exec sig m
      , Has Diagnostics sig m
      , Has (State PodSpecCache) sig m
      ) =>
      Text ->
      Text ->
      m Dependency
    readPodSubSpecSourceAt podSpecPath candidateSubSpec = context
      ("Trying to resolve (" <> candidateSubSpec <> "), It is potentially a vendored subspec of podspec at: " <> showT podSpecPath)
      $ do
        podSpecJson <- readPodSpecRaw podSpecPath

        let allSubspecNames :: [Text]
            allSubspecNames = concatMap allSubspecs (podSpecSubSpecs podSpecJson)

        case find (== candidateSubSpec) allSubspecNames of
          Nothing -> fatalText $ "could not find candidate subspec (" <> candidateSubSpec <> ") in list of subspecs!"
          Just _ -> pure $ podSpecJsonToDependency podSpecJson

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
    PodLock
      <$> obj .: "PODS"
      <*> obj .: "DEPENDENCIES"
      <*> obj .:? "EXTERNAL SOURCES" .!= Map.empty

instance FromJSON Pod where
  parseJSON (Yaml.String p) = parserPod p Nothing
  parseJSON (Yaml.Object obj) = case Object.toList obj of
    [(podEntry, podDepsListing)] -> parserPod (toText podEntry) $ Just podDepsListing
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
      <|> ( do
              somePath <- obj .: ":path"
              if ".podspec" `Text.isSuffixOf` somePath
                then pure $ ExternalPodSpec somePath -- sometimes podspec are listed under :path
                else pure $ ExternalPath somePath
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

data PodSpecJSON = PodSpecJSON
  { podSpecSources :: ExternalGitSource
  , podSpecSubSpecs :: [PodsSpecJSONSubSpec]
  }
  deriving (Show, Eq, Ord)

data PodsSpecJSONSubSpec = PodsSpecJSONSubSpec
  { subSpecName :: Text
  , childSubSpecs :: [PodsSpecJSONSubSpec]
  }
  deriving (Show, Eq, Ord)

instance FromJSON PodsSpecJSONSubSpec where
  parseJSON = JSON.withObject "PodSpecJSON" $ \o ->
    PodsSpecJSONSubSpec
      <$> (o .: "name")
      <*> (o .:? "subspecs" .!= mempty)

instance FromJSON PodSpecJSON where
  parseJSON = JSON.withObject "PodSpecJSON" $ \o -> do
    source <- o .: "source"
    subSpecs <- (o .:? "subspecs" .!= mempty)
    externalGitSource <-
      ExternalGitSource
        <$> source .: "git"
        <*> source .:? "tag"
        <*> source .:? "commit"
        <*> source .:? "branch"
    pure $ PodSpecJSON externalGitSource subSpecs

-- | Returns all subspec names, including nested subspecs.
--
-- >>> allSubspecs (PodsSpecJSONSubSpec "a" [PodsSpecJSONSubSpec "b" mempty])
-- ["a", "a/b"]
allSubspecs :: PodsSpecJSONSubSpec -> [Text]
allSubspecs = allSpecs Nothing
  where
    allSpecs :: Maybe Text -> PodsSpecJSONSubSpec -> [Text]
    allSpecs prefix (PodsSpecJSONSubSpec name childs) =
      case prefix of
        Nothing -> [name] <> concatMap (allSpecs $ Just name) childs
        Just p -> [p <> "/" <> name] <> concatMap (allSpecs (Just $ p <> "/" <> name)) childs
