{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | Parser for @pixi.lock@ (<https://pixi.prefix.dev>).
--
-- The lockfile is two joined halves. @environments@ maps an environment name
-- to a platform to a list of package /locators/; @packages@ carries the
-- metadata for each locator. The locator string is the join key, and the same
-- entry is referenced from every environment and platform that resolved it —
-- which is why a package has to be deduped across the join rather than emitted
-- per slot.
--
-- Three lock versions are in the wild and they differ only in the @packages@
-- half:
--
-- * v5 tags each entry @kind: conda@ \/ @kind: pypi@ and spells out @name@,
--   @version@, @build@ and @subdir@, with @url@ as the locator.
-- * v6 and v7 key the entry on @conda:@ \/ @pypi:@ whose value IS the locator,
--   and drop @name@ and @version@ for conda entries entirely. v7 adds a
--   top-level @platforms@ block that has no bearing on dependencies.
--
-- That dropped name is the one real fragility here: for v6/v7 conda packages
-- the only place the name and version exist is the artifact filename, so we
-- parse @\<name\>-\<version\>-\<build\>.conda@ out of the URL. Conda's filename
-- convention makes this unambiguous — the last two hyphen-separated fields are
-- version and build, and a name may itself contain hyphens — but it is a
-- convention rather than a guarantee.
module Strategy.Pixi.PixiLock (
  analyze,
  buildGraph,
  supportedLockVersions,
  PixiLockFile (..),
  PixiEnvironment (..),
  PixiLocator (..),
  PixiPackage (..),
  UnsupportedPixiLockVersion (..),
  -- exposed for testing
  parseCondaArtifactUrl,
  CondaArtifact (..),
  classifyPypiSource,
  PypiSource (..),
  environmentToDepEnvironment,
) where

import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, Has, ToDiagnostic (..), fatal, warn)
import Data.Aeson (
  FromJSON (parseJSON),
  Object,
  withObject,
  (.:),
  (.:?),
 )
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
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
  DepType (CondaType, GitType, PipType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.ReadFS (ReadFS, readContentsYaml)
import Errata (Errata (..))
import Graphing (Graphing)
import Graphing qualified
import Path (Abs, File, Path)
import Strategy.Conda.Naming (condaDependencyName)

-- | Lock versions this parser understands. An unrecognised version fails
-- loudly rather than producing an empty graph: reporting zero dependencies for
-- a project that plainly has them is the bug this strategy exists to fix, and
-- it is indistinguishable from success.
supportedLockVersions :: [Int]
supportedLockVersions = [5, 6, 7]

newtype UnsupportedPixiLockVersion = UnsupportedPixiLockVersion Int

instance ToDiagnostic UnsupportedPixiLockVersion where
  renderDiagnostic (UnsupportedPixiLockVersion v) =
    Errata
      (Just $ "Unsupported pixi.lock version: " <> toText (show v))
      []
      (Just $ "fossa-cli supports pixi lock versions " <> supported <> ". Re-run `pixi install` with a pixi release that writes one of those versions, or upgrade fossa-cli.")
    where
      supported = Text.intercalate ", " (map (toText . show) supportedLockVersions)

-- | The value of a @conda:@ or @pypi:@ key: a URL for a published artifact, or
-- a path\/VCS spec for a source dependency.
data PixiLocator
  = CondaLocator Text
  | PypiLocator Text
  deriving (Eq, Ord, Show)

data PixiPackage = PixiPackage
  { pixiPkgLocator :: PixiLocator
  , pixiPkgName :: Maybe Text
  -- ^ Explicit for every v5 entry and for v6\/v7 pypi entries; absent for
  -- v6\/v7 conda entries, where it comes from the artifact filename.
  , pixiPkgVersion :: Maybe Text
  }
  deriving (Eq, Ord, Show)

newtype PixiEnvironment = PixiEnvironment
  { pixiEnvPackages :: Map Text [PixiLocator]
  -- ^ Platform (@linux-64@, @osx-arm64@, …) to the locators resolved for it.
  }
  deriving (Eq, Ord, Show)

data PixiLockFile = PixiLockFile
  { pixiLockVersion :: Int
  , pixiLockEnvironments :: Map Text PixiEnvironment
  , pixiLockPackages :: [PixiPackage]
  }
  deriving (Eq, Ord, Show)

instance FromJSON PixiLockFile where
  parseJSON = withObject "PixiLockFile" $ \obj ->
    PixiLockFile
      <$> obj .: "version"
      <*> obj .: "environments"
      <*> obj .: "packages"

instance FromJSON PixiEnvironment where
  parseJSON = withObject "PixiEnvironment" $ \obj ->
    PixiEnvironment <$> obj .: "packages"

-- | Read the @conda:@ \/ @pypi:@ key that both the environments list and the
-- v6\/v7 package entries use to name an artifact.
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
      -- v5: `kind` names the ecosystem and `url` is the locator.
      parseV5 :: Object -> Parser PixiPackage
      parseV5 obj = do
        kind <- obj .: "kind"
        url <- obj .: "url"
        locator <- case (kind :: Text) of
          "conda" -> pure $ CondaLocator url
          "pypi" -> pure $ PypiLocator url
          other -> fail $ "unknown package kind: " <> toString other
        PixiPackage locator <$> obj .:? "name" <*> obj .:? "version"

      -- v6/v7: the ecosystem key holds the locator.
      parseV6 :: Object -> Parser PixiPackage
      parseV6 obj = do
        locator <- parseLocatorKeys obj
        PixiPackage locator <$> obj .:? "name" <*> obj .:? "version"

-- | Name, version and build recovered from a conda artifact URL, plus the
-- channel and subdir the URL path encodes.
data CondaArtifact = CondaArtifact
  { condaArtifactChannel :: Text
  , condaArtifactPlatform :: Text
  , condaArtifactName :: Text
  , condaArtifactVersion :: Text
  }
  deriving (Eq, Ord, Show)

-- | Split @https:\/\/conda.anaconda.org\/conda-forge\/linux-64\/zlib-1.3.1-hb9d3cd8_2.conda@.
--
-- The last two path segments are the subdir and the filename; the segment
-- before the subdir is the channel, which also covers self-hosted mirrors such
-- as @https:\/\/prefix.dev\/conda-forge\/…@. Within the filename the final two
-- hyphen-separated fields are version and build, so a hyphenated package name
-- like @ld_impl_linux-64@ survives the split.
parseCondaArtifactUrl :: Text -> Maybe CondaArtifact
parseCondaArtifactUrl url = do
  let segments = filter (not . Text.null) . Text.splitOn "/" . Text.takeWhile (/= '?') $ url
  (initSegments, fileName) <- unsnoc segments
  (channelSegments, platform) <- unsnoc initSegments
  channel <- lastMaybe channelSegments
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

    lastMaybe xs = case reverse xs of
      [] -> Nothing
      (x : _) -> Just x

    unsnocOn c t = case Text.breakOnEnd (Text.singleton c) t of
      ("", _) -> Nothing
      (before, after) -> Just (Text.dropEnd 1 before, after)

-- | Where a pypi entry actually comes from. pixi resolves local directories and
-- git checkouts alongside published wheels, and the locator is the only thing
-- that distinguishes them.
data PypiSource
  = -- | A published wheel or sdist on an index.
    PypiRegistry
  | -- | @git+https:\/\/…@, optionally with @?rev=@ and a @#@ commit.
    PypiGit Text
  | -- | A path relative to the workspace, or a @file:\/\/@ URL.
    PypiLocalPath
  deriving (Eq, Ord, Show)

classifyPypiSource :: Text -> PypiSource
classifyPypiSource locator
  | "git+" `Text.isPrefixOf` locator = PypiGit (Text.drop 4 locator)
  | "file://" `Text.isPrefixOf` locator = PypiLocalPath
  | any (`Text.isPrefixOf` locator) ["http://", "https://"] = PypiRegistry
  | otherwise = PypiLocalPath

-- | pixi environment names are arbitrary. @default@ is the one with fixed
-- meaning; the two conventional names map to the constructors a user would
-- expect to filter on, and anything else is preserved verbatim so it can still
-- be filtered by name.
environmentToDepEnvironment :: Text -> DepEnvironment
environmentToDepEnvironment = \case
  "default" -> EnvProduction
  "dev" -> EnvDevelopment
  "development" -> EnvDevelopment
  "test" -> EnvTesting
  "testing" -> EnvTesting
  other -> EnvOther other

-- | Key a dependency is deduped on. Environments are unioned across every
-- (environment, platform) slot a package appears in, so a package resolved for
-- five platforms is one dependency rather than five.
type DepKey = (DepType, Text, Maybe VerConstraint)

depKey :: Dependency -> DepKey
depKey Dependency{..} = (dependencyType, dependencyName, dependencyVersion)

buildGraph :: (Has Diagnostics sig m) => PixiLockFile -> m (Graphing Dependency)
buildGraph PixiLockFile{..} = do
  let packageIndex = Map.fromList [(pixiPkgLocator p, p) | p <- pixiLockPackages]
      slots =
        [ (locator, environmentToDepEnvironment envName)
        | (envName, PixiEnvironment platforms) <- Map.toList pixiLockEnvironments
        , locators <- Map.elems platforms
        , locator <- locators
        ]
      (skipped, deps) = foldr (collect packageIndex) ([], Map.empty) slots
  for_ (Set.toList $ Set.fromList skipped) warn
  pure . Graphing.fromList . Map.elems $ deps
  where
    collect index (locator, env) (skips, acc) =
      case toDependency (Map.lookup locator index) locator env of
        Left skip -> (skip : skips, acc)
        Right dep -> (skips, Map.insertWith mergeEnvironments (depKey dep) dep acc)

    mergeEnvironments new old =
      old{dependencyEnvironments = dependencyEnvironments old <> dependencyEnvironments new}

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

    -- v5 spells name and version out; v6/v7 leave the filename as the only
    -- source for them. Prefer the explicit fields when the lockfile has them.
    condaDep url = case parseCondaArtifactUrl url of
      Nothing -> Left $ "pixi: could not read a package name from conda artifact URL, skipping: " <> url
      Just CondaArtifact{..} ->
        let name = fromMaybe condaArtifactName (pixiPkgName =<< metadata)
            version = (pixiPkgVersion =<< metadata) <|> Just condaArtifactVersion
         in Right $ mkDep CondaType (condaDependencyName condaArtifactChannel condaArtifactPlatform name) version

    pypiDep source = case classifyPypiSource source of
      PypiRegistry -> case pixiPkgName =<< metadata of
        Nothing -> Left $ "pixi: pypi entry has no name, skipping: " <> source
        Just name -> Right $ mkDep PipType name (pixiPkgVersion =<< metadata)
      PypiGit url -> Right $ mkDep GitType (Text.takeWhile (/= '?') $ Text.takeWhile (/= '#') url) (gitRevision url)
      PypiLocalPath ->
        Left $
          "pixi: skipping pypi dependency from a local source, which has no registry coordinates: " <> source

    -- The fragment is the resolved commit; pixi also writes a `?rev=` query
    -- that names the same commit less precisely.
    gitRevision url = case Text.breakOnEnd "#" url of
      ("", _) -> Nothing
      (_, sha) | Text.null sha -> Nothing
      (_, sha) -> Just sha

analyze :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze lockPath = do
  lockFile <- readContentsYaml @PixiLockFile lockPath
  if pixiLockVersion lockFile `elem` supportedLockVersions
    then buildGraph lockFile
    else fatal . UnsupportedPixiLockVersion $ pixiLockVersion lockFile
