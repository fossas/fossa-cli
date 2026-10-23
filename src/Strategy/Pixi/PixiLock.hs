{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | Parser for @pixi.lock@.
--
-- @environments@ lists package locators by environment and platform;
-- @packages@ supplies their metadata. Locators join the two sections.
-- v5 uses @kind@ with @url@ or @path@; v6/v7 use @conda@ or @pypi@.
-- v6/v7 conda names and versions come from artifact filenames.
module Strategy.Pixi.PixiLock (
  analyze,
  buildGraph,
  knownLockVersions,
  outOfRangeVersionWarning,
  PixiLockFile (..),
  PixiEnvironment (..),
  PixiLocator (..),
  PixiPackage (..),
  PixiPackageEntry (..),
  PixiLockYieldedNoDeps (..),
  parseCondaArtifactUrl,
  CondaArtifact (..),
  classifyPypiSource,
  PypiSource (..),
  environmentToDepEnvironment,
) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Algebra.Graph.AdjacencyMap.Algorithm qualified as AMA
import Algebra.Graph.NonEmpty.AdjacencyMap qualified as NAM
import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, Has, ToDiagnostic (..), fatal, warn)
import Data.Aeson (
  FromJSON (parseJSON),
  Object,
  withObject,
  (.!=),
  (.:),
  (.:?),
 )
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseEither, parseMaybe)
import Data.Char (isAlphaNum, isSpace)
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.String.Conversion (toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvOther, EnvProduction, EnvTesting),
  DepType (CondaType, GitType, PipType, UnresolvedPathType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.ReadFS (ReadFS, readContentsYaml)
import Errata (Errata (..))
import Graphing (Graphing)
import Graphing qualified
import Path (Abs, File, Path)
import Strategy.Conda.Naming (condaDependencyName)
import Strategy.Pixi.Manifest (PixiManifestDeps (manifestCondaDeps, manifestPypiDeps))

-- | Known lock versions; other versions are parsed with a warning.
knownLockVersions :: (Int, Int)
knownLockVersions = (5, 7)

data PixiLockYieldedNoDeps = PixiLockYieldedNoDeps
  { noDepsVersion :: Int
  , noDepsEntries :: Int
  }

instance ToDiagnostic PixiLockYieldedNoDeps where
  renderDiagnostic (PixiLockYieldedNoDeps version entries) =
    Errata
      (Just $ "pixi.lock yielded no dependencies (package count: " <> toText (show entries) <> ")")
      []
      (Just $ "The lockfile is version " <> toText (show version) <> ". Check any warnings above for skipped packages. Upgrading fossa-cli may help.")

data PixiLocator
  = CondaLocator Text
  | PypiLocator Text
  deriving (Eq, Ord, Show)

data PixiPackage = PixiPackage
  { pixiPkgLocator :: PixiLocator
  , pixiPkgName :: Maybe Text
  -- ^ Absent for v6/v7 conda artifacts; read from the filename.
  , pixiPkgVersion :: Maybe Text
  , pixiPkgDepends :: [Text]
  -- ^ Conda match-specs, e.g. @"python >=3.9,<3.10"@.
  , pixiPkgRequiresDist :: [Text]
  -- ^ PEP 508 requirements for PyPI packages.
  }
  deriving (Eq, Ord, Show)

newtype PixiEnvironment = PixiEnvironment
  { pixiEnvPackages :: Map Text [PixiLocator]
  -- ^ Package locators by platform.
  }
  deriving (Eq, Ord, Show)

data PixiLockFile = PixiLockFile
  { pixiLockVersion :: Int
  , pixiLockEnvironments :: Map Text PixiEnvironment
  , pixiLockPackages :: [PixiPackageEntry]
  }
  deriving (Eq, Ord, Show)

-- | Preserve entry errors so other packages can still be analyzed.
data PixiPackageEntry
  = PackageEntry PixiPackage
  | UnreadableEntry Text
  deriving (Eq, Ord, Show)

instance FromJSON PixiPackageEntry where
  parseJSON value =
    pure . either (UnreadableEntry . toText) PackageEntry $ parseEither parseJSON value

instance FromJSON PixiLockFile where
  parseJSON = withObject "PixiLockFile" $ \obj ->
    PixiLockFile
      <$> obj .: "version"
      <*> obj .: "environments"
      <*> obj .: "packages"

-- Unknown environment locators, including @conda_source@, are skipped.
-- Their corresponding @packages@ entries produce warnings.
instance FromJSON PixiEnvironment where
  parseJSON = withObject "PixiEnvironment" $ \obj ->
    PixiEnvironment . fmap (mapMaybe $ parseMaybe parseJSON) <$> obj .: "packages"

parseLocatorKeys :: Object -> Parser PixiLocator
parseLocatorKeys obj =
  case (KeyMap.lookup "conda" obj, KeyMap.lookup "pypi" obj) of
    (Just _, _) -> CondaLocator <$> obj .: "conda"
    (_, Just _) -> PypiLocator <$> obj .: "pypi"
    _ -> fail $ "expected a 'conda' or 'pypi' key, got: " <> show (KeyMap.keys obj)

instance FromJSON PixiLocator where
  parseJSON = withObject "PixiLocator" parseLocatorKeys

instance FromJSON PixiPackage where
  parseJSON = withObject "PixiPackage" $ \obj ->
    case KeyMap.lookup "kind" obj of
      Just _ -> parseV5 obj
      Nothing -> parseV6 obj
    where
      -- v5 local sources use `path` instead of `url` as the locator.
      parseV5 :: Object -> Parser PixiPackage
      parseV5 obj = do
        kind <- obj .: "kind"
        url <- obj .:? "url"
        path <- obj .:? "path"
        source <- maybe (fail "entry has neither a 'url' nor a 'path'") pure (url <|> path)
        locator <- case (kind :: Text) of
          "conda" -> pure $ CondaLocator source
          "pypi" -> pure $ PypiLocator source
          other -> fail $ "unknown package kind: " <> toString other
        pkgFields obj locator

      parseV6 :: Object -> Parser PixiPackage
      parseV6 obj = do
        locator <- parseLocatorKeys obj
        pkgFields obj locator

      pkgFields obj locator =
        PixiPackage locator
          <$> obj .:? "name"
          <*> obj .:? "version"
          <*> obj .:? "depends" .!= []
          <*> obj .:? "requires_dist" .!= []

data CondaArtifact = CondaArtifact
  { condaArtifactChannel :: Text
  , condaArtifactPlatform :: Text
  , condaArtifactName :: Text
  , condaArtifactVersion :: Text
  }
  deriving (Eq, Ord, Show)

-- | The channel is every path segment between the host and subdir,
-- including labels such as @conda-forge/label/broken@.
-- The filename's last two hyphenated fields are version and build;
-- package names may contain hyphens.
parseCondaArtifactUrl :: Text -> Maybe CondaArtifact
parseCondaArtifactUrl url = do
  let afterScheme = case Text.breakOn "://" url of
        (before, rest) | Text.null rest -> before
        (_, rest) -> Text.drop 3 rest
      segments = filter (not . Text.null) . Text.splitOn "/" . Text.takeWhile (/= '?') $ afterScheme
  (initSegments, fileName) <- unsnoc segments
  (hostAndChannel, platform) <- unsnoc initSegments
  channel <- case hostAndChannel of
    [] -> Nothing
    (_host : channelSegments) | not (null channelSegments) -> Just (Text.intercalate "/" channelSegments)
    _ -> Nothing
  let stem = stripArchiveExtension fileName
  (nameAndVersion, _build) <- unsnocOn '-' stem
  (name, version) <- unsnocOn '-' nameAndVersion
  if Text.null name || Text.null version
    then Nothing
    else Just $ CondaArtifact channel platform name version
  where
    stripArchiveExtension t =
      case mapMaybe (`Text.stripSuffix` t) [".tar.bz2", ".conda"] of
        (stripped : _) -> stripped
        [] -> t

    unsnoc xs = case reverse xs of
      [] -> Nothing
      (x : rest) -> Just (reverse rest, x)

    unsnocOn c t = case Text.breakOnEnd (Text.singleton c) t of
      ("", _) -> Nothing
      (before, after) -> Just (Text.dropEnd 1 before, after)

data PypiSource
  = -- | A remote source without a @git+@ prefix.
    PypiRegistry
  | -- | A @git+@ URL with the prefix removed.
    PypiGit Text
  | -- | A local path with any @file://@ prefix removed.
    PypiLocalPath Text
  deriving (Eq, Ord, Show)

classifyPypiSource :: Text -> PypiSource
classifyPypiSource locator
  | "git+" `Text.isPrefixOf` locator = PypiGit (Text.drop 4 locator)
  | otherwise = maybe PypiRegistry PypiLocalPath (localSourcePath locator)

-- | Strip @file://@ so downstream path resolution can use the locator.
-- Other locators containing @://@ are remote, regardless of scheme.
localSourcePath :: Text -> Maybe Text
localSourcePath locator
  | Just path <- Text.stripPrefix "file://" locator = Just path
  | "://" `Text.isInfixOf` locator = Nothing
  | otherwise = Just locator

environmentToDepEnvironment :: Text -> DepEnvironment
environmentToDepEnvironment = \case
  "default" -> EnvProduction
  "dev" -> EnvDevelopment
  "development" -> EnvDevelopment
  "test" -> EnvTesting
  "testing" -> EnvTesting
  other -> EnvOther other

type DepKey = (DepType, Text, Maybe VerConstraint)

depKey :: Dependency -> DepKey
depKey Dependency{..} = (dependencyType, dependencyName, dependencyVersion)

buildGraph :: (Has Diagnostics sig m) => PixiManifestDeps -> PixiLockFile -> m (Graphing Dependency)
buildGraph declared PixiLockFile{..} = do
  let packageIndex = Map.fromList [(pixiPkgLocator p, p) | PackageEntry p <- pixiLockPackages]
      unreadable = ["pixi: skipping an unreadable package entry: " <> why | UnreadableEntry why <- pixiLockPackages]
      slots =
        [ (locator, environmentToDepEnvironment envName)
        | (envName, PixiEnvironment platforms) <- Map.toList pixiLockEnvironments
        , locators <- Map.elems platforms
        , locator <- locators
        ]
      (skipped, deps, keyOf) = foldr (collect packageIndex) ([], Map.empty, Map.empty) slots
      depOf locator = (`Map.lookup` deps) =<< Map.lookup locator keyOf

      condaKeys = Map.fromList [(l, k) | l <- Map.keys keyOf, Just k <- [condaMatchKey (Map.lookup l packageIndex) l]]
      pypiKeys = Map.fromList [(l, k) | l <- Map.keys keyOf, Just k <- [pypiMatchKey (Map.lookup l packageIndex) l]]

      condaByName = Map.fromListWith (<>) [(name, [(subdir, l)]) | (l, (name, subdir)) <- Map.toList condaKeys]
      pypiByName = Map.fromListWith (<>) [(name, [l]) | (l, name) <- Map.toList pypiKeys]

      -- `noarch` packages can depend on, and be used by, any subdir.
      subdirMatches (Just from) (Just to) = from == "noarch" || to == from || to == "noarch"
      subdirMatches _ _ = True

      targetsOf locator pkg = case locator of
        CondaLocator{} ->
          [ target
          | spec <- pixiPkgDepends pkg
          , (subdir, target) <- Map.findWithDefault [] (condaSpecName spec) condaByName
          , subdirMatches (snd =<< Map.lookup locator condaKeys) subdir
          ]
        PypiLocator{} ->
          [ target
          | spec <- pixiPkgRequiresDist pkg
          , target <- Map.findWithDefault [] (normalizePypiName $ pypiSpecName spec) pypiByName
          ]

      graphEdges =
        [ (parent, child)
        | (locator, pkg) <- Map.toList packageIndex
        , Just parent <- [depOf locator]
        , target <- targetsOf locator pkg
        , Just child <- [depOf target]
        , parent /= child
        ]

      graph = Graphing.deeps (Map.elems deps) <> Graphing.edges graphEdges

      -- pixi compares conda names case-insensitively.
      declaredConda = Set.map Text.toLower $ manifestCondaDeps declared
      declaredPypi = Set.map normalizePypiName $ manifestPypiDeps declared
      declaredKeys =
        Set.fromList . mapMaybe (`Map.lookup` keyOf) $
          [l | (l, (name, _)) <- Map.toList condaKeys, Text.toLower name `Set.member` declaredConda]
            <> [l | (l, name) <- Map.toList pypiKeys, name `Set.member` declaredPypi]
  for_ (Set.toList . Set.fromList $ unreadable <> skipped) warn
  -- Packages the manifest declares are direct. Root components are direct
  -- too, including every member of an unreached cycle: analysis drops
  -- packages that no direct dependency reaches.
  let components = AMA.scc $ Graphing.toAdjacencyMap graph
      reached = Set.fromList . map snd $ AM.edgeList components
      roots = Set.unions [NAM.vertexSet c | c <- AM.vertexList components, Set.notMember c reached]
      isDirect dep = Set.member dep roots || Set.member (depKey dep) declaredKeys
  -- Shrinking a direct workspace package promotes its dependencies.
  pure . Graphing.shrink (not . isWorkspacePackage) $ Graphing.promoteToDirect isDirect graph
  where
    collect index (locator, env) (skips, acc, keys) =
      case toDependency (Map.lookup locator index) locator env of
        Left skip -> (skip : skips, acc, keys)
        Right dep ->
          ( skips
          , Map.insertWith mergeEnvironments (depKey dep) dep acc
          , Map.insert locator (depKey dep) keys
          )

    mergeEnvironments new old =
      old{dependencyEnvironments = dependencyEnvironments old <> dependencyEnvironments new}

-- | pixi writes the lockfile's own directory as @.@ or @./@.
isWorkspacePackage :: Dependency -> Bool
isWorkspacePackage dep = dependencyType dep == UnresolvedPathType && dependencyName dep `elem` [".", "./"]

condaMatchKey :: Maybe PixiPackage -> PixiLocator -> Maybe (Text, Maybe Text)
condaMatchKey meta = \case
  PypiLocator{} -> Nothing
  CondaLocator url -> case localSourcePath url of
    -- Local packages have no artifact subdir and can match any platform.
    Just _ -> (,Nothing) <$> (pixiPkgName =<< meta)
    Nothing -> do
      CondaArtifact{..} <- parseCondaArtifactUrl url
      pure (fromMaybe condaArtifactName (pixiPkgName =<< meta), Just condaArtifactPlatform)

pypiMatchKey :: Maybe PixiPackage -> PixiLocator -> Maybe Text
pypiMatchKey meta = \case
  CondaLocator{} -> Nothing
  PypiLocator{} -> normalizePypiName <$> (pixiPkgName =<< meta)

condaSpecName :: Text -> Text
condaSpecName spec = Text.takeWhile (\c -> not (isSpace c) && c `notElem` ['=', '>', '<', '!', '~']) afterChannel
  where
    afterChannel = case Text.breakOn "::" (Text.strip spec) of
      (before, rest) | Text.null rest -> before
      (_, rest) -> Text.drop 2 rest

-- | Read the PEP 508 package name, ignoring extras, constraints, and markers.
pypiSpecName :: Text -> Text
pypiSpecName = Text.takeWhile (\c -> isAlphaNum c || c `elem` ['-', '_', '.']) . Text.stripStart

-- | PEP 503 normalization for matching; reported names stay unchanged.
normalizePypiName :: Text -> Text
normalizePypiName = Text.toLower . Text.intercalate "-" . filter (not . Text.null) . Text.split (`elem` ['-', '_', '.'])

toDependency :: Maybe PixiPackage -> PixiLocator -> DepEnvironment -> Either Text Dependency
toDependency metadata locator env = case locator of
  CondaLocator url -> condaDep url
  PypiLocator source -> pypiDep source
  where
    mkDep depType name version =
      Dependency
        { dependencyType = depType
        , dependencyName = name
        , dependencyVersion = CEq <$> version
        , dependencyLocations = []
        , dependencyEnvironments = Set.singleton env
        , dependencyTags = Map.empty
        }

    described fallback = maybe fallback (\n -> n <> " (" <> fallback <> ")") (pixiPkgName =<< metadata)

    -- Resolved by 'App.Fossa.PathDependency'.
    pathDep path = mkDep UnresolvedPathType path (pixiPkgVersion =<< metadata)

    -- Classify paths first: their segments can resemble artifact URLs.
    condaDep url = case localSourcePath url of
      Just path -> Right $ pathDep path
      Nothing -> case parseCondaArtifactUrl url of
        Nothing -> Left $ "pixi: could not read a package name from conda artifact URL, skipping: " <> described url
        Just CondaArtifact{..} ->
          let name = fromMaybe condaArtifactName (pixiPkgName =<< metadata)
              version = (pixiPkgVersion =<< metadata) <|> Just condaArtifactVersion
           in Right $ mkDep CondaType (condaDependencyName condaArtifactChannel condaArtifactPlatform name) version

    pypiDep source = case classifyPypiSource source of
      PypiRegistry -> case pixiPkgName =<< metadata of
        Nothing -> Left $ "pixi: pypi entry has no name, skipping: " <> source
        Just name -> Right $ mkDep PipType name (pixiPkgVersion =<< metadata)
      PypiGit url -> Right $ mkDep GitType (Text.takeWhile (/= '?') $ Text.takeWhile (/= '#') url) (gitRevision url)
      PypiLocalPath path -> Right $ pathDep path

    -- The fragment holds the resolved commit.
    gitRevision url = case Text.breakOnEnd "#" url of
      ("", _) -> Nothing
      (_, sha) | Text.null sha -> Nothing
      (_, sha) -> Just sha

analyze :: (Has ReadFS sig m, Has Diagnostics sig m) => PixiManifestDeps -> Path Abs File -> m (Graphing Dependency)
analyze declared lockPath = do
  lockFile <- readContentsYaml @PixiLockFile lockPath
  for_ (outOfRangeVersionWarning $ pixiLockVersion lockFile) warn
  graph <- buildGraph declared lockFile
  -- A workspace-only lockfile legitimately produces an empty graph.
  let isWorkspaceEntry (PackageEntry p) = either (const False) isWorkspacePackage $ toDependency (Just p) (pixiPkgLocator p) EnvProduction
      isWorkspaceEntry (UnreadableEntry _) = False
      entries = length . filter (not . isWorkspaceEntry) $ pixiLockPackages lockFile
  if entries > 0 && null (Graphing.vertexList graph)
    then fatal $ PixiLockYieldedNoDeps (pixiLockVersion lockFile) entries
    else pure graph

outOfRangeVersionWarning :: Int -> Maybe Text
outOfRangeVersionWarning version
  | version > newest =
      Just $
        "pixi: lock version " <> shown version <> " is newer than any version this fossa-cli knows about (" <> shown newest <> "). It was parsed anyway, so results may be incomplete; upgrade fossa-cli."
  | version < oldest =
      Just $
        "pixi: lock version " <> shown version <> " is older than " <> shown oldest <> ", which is not officially supported!"
  | otherwise = Nothing
  where
    (oldest, newest) = knownLockVersions
    shown :: Int -> Text
    shown = toText . show
