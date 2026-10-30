{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | Parser for @pixi.lock@, written against the lock versions 4 through 7
-- that rattler_lock 0.34.8 reads:
-- <https://github.com/conda/rattler/tree/rattler_lock-v0.34.8/crates/rattler_lock>
--
-- @environments@ lists package locators by environment and platform;
-- @packages@ supplies their metadata. Locators join the two sections.
-- v4/v5 use @kind@ with @url@ or @path@; v6/v7 use @conda@ or @pypi@, and v7
-- uses @conda_source@ for packages pixi builds from source.
-- v6/v7 conda names and versions come from artifact filenames.
module Strategy.Pixi.PixiLock (
  analyze,
  buildGraph,
  knownLockVersions,
  outOfRangeVersionWarning,
  PixiLockFile (..),
  PixiEnvironment (..),
  PixiEnvPackage (..),
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
import Control.Monad (mfilter)
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
import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.Either (fromRight, rights)
import Data.Foldable (fold, for_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Conversion (toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvOther, EnvProduction, EnvTesting),
  DepType (CondaType, GitType, PipType, URLType, UnresolvedPathType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.ReadFS (ReadFS, readContentsYaml)
import Errata (Errata (..))
import Graphing (Graphing)
import Graphing qualified
import Path (Abs, File, Path)
import Strategy.Conda.Naming (condaDependencyName)
import Strategy.Pixi.Manifest (PixiManifestDeps (manifestCondaDeps, manifestPypiDeps), environmentPypiDeps, reqParts)
import Strategy.Python.Util (Marker (..), MarkerOp (MarkerOperator), Operator (OpEq, OpNot), Req, requirementParser)
import System.FilePath (isAbsolute, (</>))
import Text.Megaparsec (Parsec, anySingle, chunk, eof, lookAhead, many, manyTill, option, optional, parse, some, takeRest, takeWhile1P, takeWhileP, try)
import Text.Megaparsec.Char (char, space)

-- | Known lock versions; other versions are parsed with a warning.
knownLockVersions :: (Int, Int)
knownLockVersions = (4, 7)

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
  | -- | @name[hash] \@ location@, where the location holds the package's manifest.
    CondaSourceLocator Text
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
  , pixiPkgBuildSource :: Maybe PixiBuildSource
  }
  deriving (Eq, Ord, Show)

-- | Where pixi builds a source package from, when it isn't the directory of
-- the package's manifest.
data PixiBuildSource
  = -- | The repository and resolved commit.
    BuildFromGit Text Text
  | BuildFromUrl Text
  | -- | Relative to the package's manifest, unless absolute.
    BuildFromPath Text
  deriving (Eq, Ord, Show)

instance FromJSON PixiBuildSource where
  parseJSON = withObject "PixiBuildSource" $ \obj ->
    case (KeyMap.lookup "git" obj, KeyMap.lookup "url" obj, KeyMap.lookup "path" obj) of
      (Just _, _, _) -> BuildFromGit <$> obj .: "git" <*> obj .: "rev"
      (_, Just _, _) -> BuildFromUrl <$> obj .: "url"
      (_, _, Just _) -> BuildFromPath <$> obj .: "path"
      _ -> fail $ "expected a 'git', 'url', or 'path' source, got: " <> show (KeyMap.keys obj)

newtype PixiEnvironment = PixiEnvironment
  { pixiEnvPackages :: Map Text [PixiEnvPackage]
  -- ^ Packages by platform.
  }
  deriving (Eq, Ord, Show)

-- | Lock versions before 7 record the extras a PyPI package is installed with.
data PixiEnvPackage = PixiEnvPackage
  { envPackageLocator :: PixiLocator
  , envPackageExtras :: [Text]
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

-- | Read before the rest, so a version this parser doesn't know gets its
-- warning even when the other keys don't decode.
newtype PixiLockVersion = PixiLockVersion Int

instance FromJSON PixiLockVersion where
  parseJSON = withObject "PixiLockVersion" $ \obj -> PixiLockVersion <$> obj .: "version"

instance FromJSON PixiLockFile where
  parseJSON = withObject "PixiLockFile" $ \obj ->
    PixiLockFile
      <$> obj .: "version"
      <*> obj .: "environments"
      <*> obj .: "packages"

-- Unknown environment locators are skipped. Their corresponding @packages@
-- entries produce warnings.
instance FromJSON PixiEnvironment where
  parseJSON = withObject "PixiEnvironment" $ \obj ->
    PixiEnvironment . fmap (mapMaybe $ parseMaybe parseJSON) <$> obj .: "packages"

parseLocatorKeys :: Object -> Parser PixiLocator
parseLocatorKeys obj =
  case (KeyMap.lookup "conda" obj, KeyMap.lookup "pypi" obj, KeyMap.lookup "conda_source" obj) of
    (Just _, _, _) -> CondaLocator <$> obj .: "conda"
    (_, Just _, _) -> PypiLocator <$> obj .: "pypi"
    (_, _, Just _) -> CondaSourceLocator <$> obj .: "conda_source"
    _ -> fail $ "expected a 'conda', 'pypi', or 'conda_source' key, got: " <> show (KeyMap.keys obj)

instance FromJSON PixiEnvPackage where
  parseJSON = withObject "PixiEnvPackage" $ \obj ->
    PixiEnvPackage <$> parseLocatorKeys obj <*> obj .:? "extras" .!= []

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
          <*> obj .:? "source"

data CondaArtifact = CondaArtifact
  { condaArtifactChannel :: Text
  , condaArtifactPlatform :: Text
  , condaArtifactName :: Text
  , condaArtifactVersion :: Text
  }
  deriving (Eq, Ord, Show)

-- | Reads @<channel>/<subdir>/<name>-<version>-<build>@ after the host. The
-- channel is every path segment between the host and subdir, including labels
-- such as @conda-forge/label/broken@, and package names can contain hyphens:
--
-- > https://conda.anaconda.org/conda-forge/linux-64/zlib-1.3.1-hb9d3cd8_2.conda
-- > https://conda.anaconda.org/conda-forge/linux-64/_openmp_mutex-4.5-2_gnu.tar.bz2
-- > https://conda.anaconda.org/conda-forge/noarch/tzdata-2025b-h78e105d_0.conda
-- > s3://my-s3-bucket/my-channel/linux-64/ld_impl_linux-64-2.40-hf3520f5_7.conda
parseCondaArtifactUrl :: Text -> Maybe CondaArtifact
parseCondaArtifactUrl = either (const Nothing) Just . parse artifactUrl ""
  where
    artifactUrl :: Parsec Void Text CondaArtifact
    artifactUrl = do
      _scheme <- optional . try $ manyTill anySingle (chunk "://")
      _host <- many (char '/') *> segment <* slashes
      -- A channel segment is followed by at least the subdir and filename.
      channel <- Text.intercalate "/" <$> some (try $ segment <* slashes <* lookAhead (segment *> slashes *> segment))
      platform <- segment <* slashes
      -- The last two hyphenated fields are the version and build.
      name <- Text.intercalate "-" <$> some (try $ field <* char '-' <* lookAhead (field *> char '-'))
      version <- field <* char '-'
      _build <- takeWhileP (Just "build") (`notElem` ['/', '?'])
      _ <- many (char '/') *> optional (char '?' *> takeRest) *> eof
      pure $ CondaArtifact channel platform name version

    segment = takeWhile1P (Just "path segment") (`notElem` ['/', '?'])
    slashes = some (char '/')
    field = takeWhile1P (Just "field") (`notElem` ['-', '/', '?'])

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
        [ (envPackageLocator entry, environmentToDepEnvironment envName)
        | (envName, PixiEnvironment platforms) <- Map.toList pixiLockEnvironments
        , entries <- Map.elems platforms
        , entry <- entries
        ]
      (skipped, deps, keyOf) = foldr (collect packageIndex) ([], Map.empty, Map.empty) slots
      depOf locator = (`Map.lookup` deps) =<< Map.lookup locator keyOf

      condaKeys = Map.fromList [(l, k) | l <- Map.keys keyOf, Just k <- [condaMatchKey (Map.lookup l packageIndex) l]]
      pypiKeys = Map.fromList [(l, k) | l <- Map.keys keyOf, Just k <- [pypiMatchKey (Map.lookup l packageIndex) l]]

      parsedRequirements = Map.map (map (\spec -> first (const spec) $ parse requirementParser "" spec) . pixiPkgRequiresDist) packageIndex
      unparsed = ["pixi: skipping a requirement that could not be read: " <> spec | Left spec <- fold parsedRequirements]
      requirementsOf locator = rights $ Map.findWithDefault [] locator parsedRequirements

      -- An environment installs one package per name on each platform, so
      -- names resolve within that set.
      edgesWithin env entries =
        let installed = map envPackageLocator entries
            condaByName = Map.fromList [(name, l) | l <- installed, Just name <- [Map.lookup l condaKeys]]
            pypiByName = Map.fromList [(name, l) | l <- installed, Just name <- [Map.lookup l pypiKeys]]
            requested =
              Map.fromListWith
                (<>)
                [ (name, Set.fromList (map normalizePypiName extras) <> Map.findWithDefault mempty name (declaredExtras env))
                | PixiEnvPackage l extras <- entries
                , Just name <- [Map.lookup l pypiKeys]
                ]
            extrasOf = installedExtras requested [(name, requirementsOf l) | (name, l) <- Map.toList pypiByName]
            targets locator = case locator of
              PypiLocator{} ->
                [ target
                | Just name <- [Map.lookup locator pypiKeys]
                , (dependency, _, marker) <- map reqParts $ requirementsOf locator
                , all (markerAllows $ Map.findWithDefault mempty name extrasOf) marker
                , Just target <- [Map.lookup (normalizePypiName dependency) pypiByName]
                ]
              _ -> mapMaybe ((`Map.lookup` condaByName) . condaSpecName) . foldMap pixiPkgDepends $ Map.lookup locator packageIndex
         in [(l, target) | l <- installed, target <- targets l]

      graphEdges =
        [ (parent, child)
        | (env, PixiEnvironment platforms) <- Map.toList pixiLockEnvironments
        , entries <- Map.elems platforms
        , (locator, target) <- edgesWithin env entries
        , Just parent <- [depOf locator]
        , Just child <- [depOf target]
        , parent /= child
        ]

      graph = Graphing.deeps (Map.elems deps) <> Graphing.edges graphEdges

      -- pixi compares conda names case-insensitively.
      declaredConda = Set.map Text.toLower $ manifestCondaDeps declared
      declaredPypi = Set.map normalizePypiName . foldMap Map.keysSet $ manifestPypiDeps declared
      declaredExtras env = Map.fromListWith (<>) [(normalizePypiName name, Set.map normalizePypiName extras) | (name, extras) <- Map.toList $ environmentPypiDeps declared env]
      declaredKeys =
        Set.fromList . mapMaybe (`Map.lookup` keyOf) $
          [l | (l, name) <- Map.toList condaKeys, Text.toLower name `Set.member` declaredConda]
            <> [l | (l, name) <- Map.toList pypiKeys, name `Set.member` declaredPypi]
  for_ (Set.toList . Set.fromList $ unreadable <> skipped <> unparsed) warn
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

condaMatchKey :: Maybe PixiPackage -> PixiLocator -> Maybe Text
condaMatchKey meta = \case
  PypiLocator{} -> Nothing
  CondaSourceLocator identifier -> fst <$> either (const Nothing) Just (parse sourceIdentifier "" identifier)
  CondaLocator url -> case localSourcePath url of
    Just _ -> pixiPkgName =<< meta
    Nothing -> (pixiPkgName =<< meta) <|> (condaArtifactName <$> parseCondaArtifactUrl url)

pypiMatchKey :: Maybe PixiPackage -> PixiLocator -> Maybe Text
pypiMatchKey meta = \case
  PypiLocator{} -> normalizePypiName <$> (pixiPkgName =<< meta)
  _ -> Nothing

-- | Reads a match spec's package name, after any @channel::@ prefix:
--
-- > python >=3.11,<3.12.0a0
-- > conda-forge::pytorch 2.8.0 cpu_mkl*102
condaSpecName :: Text -> Text
condaSpecName = fromRight "" . parse matchSpecName ""
  where
    matchSpecName :: Parsec Void Text Text
    matchSpecName = do
      _channel <- space *> optional (try $ manyTill anySingle (chunk "::"))
      takeWhileP (Just "package name") (\c -> not (isSpace c) && c `notElem` ['=', '>', '<', '!', '~'])

-- | Only @extra@ comparisons are evaluated; other markers are assumed to hold.
markerAllows :: Set Text -> Marker -> Bool
markerAllows extras = \case
  MarkerAnd a b -> markerAllows extras a && markerAllows extras b
  MarkerOr a b -> markerAllows extras a || markerAllows extras b
  MarkerExpr "extra" (MarkerOperator OpEq) extra -> Set.member (normalizePypiName extra) extras
  MarkerExpr "extra" (MarkerOperator OpNot) extra -> Set.notMember (normalizePypiName extra) extras
  MarkerExpr{} -> True

-- | The extras each PyPI package is installed with: those requested for it,
-- and those its active requirements request in turn, to a fixed point.
installedExtras :: Map Text (Set Text) -> [(Text, [Req])] -> Map Text (Set Text)
installedExtras requested packages
  | next == requested = requested
  | otherwise = installedExtras next packages
  where
    next =
      Map.unionWith (<>) requested $
        Map.fromListWith
          (<>)
          [ (normalizePypiName dependency, Set.fromList $ map normalizePypiName extras)
          | (name, reqs) <- packages
          , (dependency, extras, marker) <- map reqParts reqs
          , all (markerAllows $ Map.findWithDefault mempty name requested) marker
          ]

-- | PEP 503 normalization for matching; reported names stay unchanged.
normalizePypiName :: Text -> Text
normalizePypiName = Text.toLower . Text.intercalate "-" . filter (not . Text.null) . Text.split (`elem` ['-', '_', '.'])

toDependency :: Maybe PixiPackage -> PixiLocator -> DepEnvironment -> Either Text Dependency
toDependency metadata locator env = case locator of
  CondaLocator url -> condaDep url
  PypiLocator source -> pypiDep source
  CondaSourceLocator identifier -> sourceDep identifier
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

    -- Classify paths and git sources first: their segments can resemble artifact URLs.
    condaDep url = case classifyPypiSource url of
      PypiLocalPath path -> Right $ pathDep path
      -- A v6 lock lists a package built from git by its locked URL.
      PypiGit repository -> Right $ gitDep repository
      PypiRegistry -> case parseCondaArtifactUrl url of
        Nothing -> Left $ "pixi: could not read a package name from conda artifact URL, skipping: " <> described url
        Just CondaArtifact{..} ->
          let name = fromMaybe condaArtifactName (pixiPkgName =<< metadata)
              version = (pixiPkgVersion =<< metadata) <|> Just condaArtifactVersion
           in Right $ mkDep CondaType (condaDependencyName condaArtifactChannel condaArtifactPlatform name) version

    pypiDep source = case classifyPypiSource source of
      PypiRegistry -> case pixiPkgName =<< metadata of
        Nothing -> Left $ "pixi: pypi entry has no name, skipping: " <> source
        Just name -> Right $ mkDep PipType name (pixiPkgVersion =<< metadata)
      PypiGit url -> Right $ gitDep url
      PypiLocalPath path -> Right $ pathDep path

    gitDep url = uncurry (mkDep GitType) . fromRight (url, Nothing) $ parse gitSource "" url

    -- Without a build source, a package is built from its manifest's location.
    sourceDep identifier = case parse sourceIdentifier "" identifier of
      Left _ -> Left $ "pixi: could not read a source package, skipping: " <> identifier
      Right (_, location) -> Right $ case pixiPkgBuildSource =<< metadata of
        Just (BuildFromGit url commit) -> mkDep GitType url (Just commit)
        Just (BuildFromUrl url) -> mkDep URLType url (pixiPkgVersion =<< metadata)
        Just (BuildFromPath path) -> case classifyPypiSource location of
          PypiLocalPath manifestDir -> pathDep $ relativeToManifest manifestDir path
          -- A relative path names a directory within the remote source.
          _ -> if isAbsolute (toString path) then pathDep path else locationDep location
        Nothing -> locationDep location

    locationDep location = case classifyPypiSource location of
      PypiGit url -> gitDep url
      PypiRegistry -> mkDep URLType location (pixiPkgVersion =<< metadata)
      PypiLocalPath path -> pathDep path

    -- The fragment holds the resolved commit. Without one, a ref after the
    -- path is the version; an @ before the path belongs to the user:
    --
    -- > https://github.com/laurentS/slowapi.git?rev=a72bcc66597f620f04bf5be3676e40ed308d3a6a#a72bcc66597f620f04bf5be3676e40ed308d3a6a
    -- > https://github.com/NVIDIA/numba-cuda@main
    -- > ssh://git@github.com/example/tools.git@v0.1.0
    gitSource :: Parsec Void Text (Text, Maybe Text)
    gitSource = do
      scheme <- option "" . try $ (<>) <$> takeWhile1P (Just "scheme") (`notElem` [':', '/', '@', '?', '#']) <*> chunk "://"
      authority <- takeWhileP (Just "authority") (`notElem` ['/', '?', '#'])
      path <- takeWhileP (Just "path") (`notElem` ['@', '?', '#'])
      ref <- optional $ char '@' *> takeWhileP (Just "ref") (`notElem` ['?', '#'])
      _query <- optional $ char '?' *> takeWhileP (Just "query") (/= '#')
      commit <- optional $ char '#' *> takeRest
      pure (scheme <> authority <> path, mfilter (not . Text.null) commit <|> mfilter (not . Text.null) ref)

-- | Reads a source package's name and the location of its manifest:
--
-- > sdl_example[10db969c] @ .
sourceIdentifier :: Parsec Void Text (Text, Text)
sourceIdentifier = do
  name <- takeWhile1P (Just "package name") (/= '[')
  _hash <- char '[' *> takeWhile1P (Just "hash") (/= ']') <* char ']'
  location <- space *> char '@' *> space *> takeRest
  pure (name, location)

relativeToManifest :: Text -> Text -> Text
relativeToManifest location path
  | location `elem` [".", "./"] || isAbsolute (toString path) = path
  | otherwise = toText $ toString location </> toString path

analyze :: (Has ReadFS sig m, Has Diagnostics sig m) => PixiManifestDeps -> Path Abs File -> m (Graphing Dependency)
analyze declared lockPath = do
  PixiLockVersion version <- readContentsYaml lockPath
  for_ (outOfRangeVersionWarning version) warn
  lockFile <- readContentsYaml @PixiLockFile lockPath
  graph <- buildGraph declared lockFile
  -- A workspace-only lockfile legitimately produces an empty graph. Packages no
  -- environment uses, like the ones pixi builds source packages with, don't count.
  let metadata = foldMap (\case PackageEntry p -> Map.singleton (pixiPkgLocator p) p; UnreadableEntry _ -> mempty) $ pixiLockPackages lockFile
      isWorkspace locator = either (const False) isWorkspacePackage $ toDependency (Map.lookup locator metadata) locator EnvProduction
      used = foldMap (foldMap (Set.fromList . map envPackageLocator) . pixiEnvPackages) $ pixiLockEnvironments lockFile
      entries = Set.size $ Set.filter (not . isWorkspace) used
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
