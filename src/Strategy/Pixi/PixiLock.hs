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
-- Three lock versions are in the wild and they differ mostly in the @packages@
-- half:
--
-- * v5 tags each entry @kind: conda@ \/ @kind: pypi@ and spells out @name@,
--   @version@, @build@ and @subdir@, with @url@ as the locator.
-- * v6 and v7 key the entry on @conda:@ \/ @pypi:@ whose value IS the locator,
--   and drop @name@ and @version@ for conda entries entirely. v7 adds a
--   top-level @platforms@ block that has no bearing on dependencies, and keys
--   pixi-build source packages on @conda_source:@ in both halves; those are
--   skipped.
--
-- That dropped name is the one real fragility here: for v6/v7 conda packages
-- the only place the name and version exist is the artifact filename, so we
-- parse @\<name\>-\<version\>-\<build\>.conda@ out of the URL. Conda's filename
-- convention makes this unambiguous — the last two hyphen-separated fields are
-- version and build, and a name may itself contain hyphens — but it is a
-- convention rather than a guarantee.
--
-- NO SINGLE ENTRY MAY FAIL THE FILE. Every @packages@ entry is read
-- independently and an unreadable one degrades to a warning, because the bug
-- this strategy exists to fix is a pixi project reporting zero dependencies.
-- An aeson field decoder that fails mid-list takes the whole document with it,
-- which turns one odd package into exactly that bug — a v5 lockfile with one
-- @path:@-sourced package used to fail with @key \"url\" not found@ and report
-- nothing at all.
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
  -- exposed for testing
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

-- | Oldest and newest lock versions this parser was written against.
--
-- Not a gate. The format has been additive -- v7 is v6's @packages@ shape plus
-- an inert top-level @platforms@ block -- so an unseen version is far likelier
-- to parse than not, and refusing one outright reports nothing for a project
-- that plainly has dependencies. Out-of-range versions are parsed anyway and
-- only warn, matching pnpm.
knownLockVersions :: (Int, Int)
knownLockVersions = (5, 7)

-- | The lockfile listed packages and not one of them produced a dependency.
--
-- Fatal, where a lockfile with no entries at all is not: an empty graph is
-- indistinguishable from a clean scan of a workspace that has no
-- dependencies, so whether there was anything to read is the only thing that
-- separates the two. Every entry failing at once is what a restructured
-- @packages@ shape looks like from here.
data PixiLockYieldedNoDeps = PixiLockYieldedNoDeps
  { noDepsVersion :: Int
  , noDepsEntries :: Int
  }

instance ToDiagnostic PixiLockYieldedNoDeps where
  renderDiagnostic (PixiLockYieldedNoDeps version entries) =
    Errata
      (Just $ "pixi.lock lists " <> toText (show entries) <> " packages, but none of them could be read")
      []
      (Just $ "The lockfile is version " <> toText (show version) <> ". Its `packages` entries are shaped in a way this fossa-cli does not recognise; the warnings above name each one. Upgrading fossa-cli may help.")

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
  , pixiPkgDepends :: [Text]
  -- ^ Conda match-specs (@"python >=3.9,<3.10"@), naming a package rather
  -- than resolving one. Empty for pypi entries.
  , pixiPkgRequiresDist :: [Text]
  -- ^ PEP 508 requirements. Empty for conda entries.
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
  , pixiLockPackages :: [PixiPackageEntry]
  }
  deriving (Eq, Ord, Show)

-- | One @packages@ entry, or the reason it could not be read.
--
-- The whole point of the indirection: 'parseJSON' here always succeeds, so a
-- malformed or unrecognised entry can only ever cost that one package.
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

-- | An entry of a kind this parser doesn't know, such as a v7 @conda_source:@,
-- is dropped rather than failing the file; its @packages@ entry warns.
instance FromJSON PixiEnvironment where
  parseJSON = withObject "PixiEnvironment" $ \obj ->
    PixiEnvironment . fmap (mapMaybe $ parseMaybe parseJSON) <$> obj .: "packages"

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
      -- v5: `kind` names the ecosystem, and the locator is `url` for a
      -- published artifact or `path` for one resolved from disk. `path` is
      -- what the environments list references for those, so it is the join
      -- key as much as `url` is.
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

      -- v6/v7: the ecosystem key holds the locator.
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
-- The last two path segments are the subdir and the filename. Everything
-- between the host and the subdir is the channel, joined back with @\/@ —
-- not just the segment nearest the subdir. Labeled channels are ordinary
-- conda usage and put extra segments there:
-- @conda-forge\/label\/broken\/linux-64\/…@ is the @conda-forge\/label\/broken@
-- channel, not the @broken@ channel. This matches
-- 'Strategy.Conda.CondaEnvCreate.parseCondaEnvDep', which joins every channel
-- component the same way; the two strategies have to agree or one package
-- lands in FOSSA under two names.
--
-- Dropping the host also covers self-hosted mirrors such as
-- @https:\/\/prefix.dev\/conda-forge\/…@. Within the filename the final two
-- hyphen-separated fields are version and build, so a hyphenated package name
-- like @ld_impl_linux-64@ survives the split.
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

-- | Where a pypi entry actually comes from. pixi resolves local directories and
-- git checkouts alongside published wheels, and the locator is the only thing
-- that distinguishes them.
data PypiSource
  = -- | A published wheel or sdist on an index.
    PypiRegistry
  | -- | @git+https:\/\/…@, optionally with @?rev=@ and a @#@ commit.
    PypiGit Text
  | -- | Something on disk, carrying the path with any @file:\/\/@ removed.
    PypiLocalPath Text
  deriving (Eq, Ord, Show)

classifyPypiSource :: Text -> PypiSource
classifyPypiSource locator
  | "git+" `Text.isPrefixOf` locator = PypiGit (Text.drop 4 locator)
  | otherwise = maybe PypiRegistry PypiLocalPath (localSourcePath locator)

-- | The path a locator names on disk, if it names one at all rather than an
-- artifact to fetch.
--
-- @file:\/\/@ is stripped so both spellings of one package produce one
-- dependency. That is not cosmetic: the name is resolved against the project
-- directory downstream (@App.Fossa.PathDependency.absPathOf@), which fails
-- outright on a URL. pdm and cargo strip it for the same reason. Any other
-- URL scheme, such as an @s3:\/\/@ channel, is remote, as rattler reads it.
localSourcePath :: Text -> Maybe Text
localSourcePath locator
  | Just path <- Text.stripPrefix "file://" locator = Just path
  | "://" `Text.isInfixOf` locator = Nothing
  | otherwise = Just locator

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

      -- How each locator is addressed from someone else's dependency list.
      -- Only locators that became dependencies are addressable, which is why
      -- a skipped entry can never be an edge target.
      condaKeys = Map.fromList [(l, k) | l <- Map.keys keyOf, Just k <- [condaMatchKey (Map.lookup l packageIndex) l]]
      pypiKeys = Map.fromList [(l, k) | l <- Map.keys keyOf, Just k <- [pypiMatchKey (Map.lookup l packageIndex) l]]

      condaByName = Map.fromListWith (<>) [(name, [(subdir, l)]) | (l, (name, subdir)) <- Map.toList condaKeys]
      pypiByName = Map.fromListWith (<>) [(name, [l]) | (l, name) <- Map.toList pypiKeys]

      -- A conda package resolves within its own subdir plus `noarch`, and a
      -- `noarch` package, which every platform shares, in every subdir. Without
      -- that, a linux-64 package would edge to the osx-arm64 build of the same
      -- dependency, because every platform is unioned into one graph.
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
  for_ (Set.toList . Set.fromList $ unreadable <> skipped) warn
  -- The roots ARE the direct set. pixi's environments list is the full
  -- resolved closure, so nothing in the file says which packages were asked
  -- for -- but a package nothing else depends on is one that was. No second
  -- file to read, and nothing to do when there is no pixi.toml. A cycle that
  -- nothing outside it depends on is a root too, so every member is direct;
  -- otherwise nothing would reach it and it would be pruned from the results.
  let components = AMA.scc $ Graphing.toAdjacencyMap graph
      reached = Set.fromList . map snd $ AM.edgeList components
      roots = Set.unions [NAM.vertexSet c | c <- AM.vertexList components, Set.notMember c reached]
  pure $ Graphing.promoteToDirect (`Set.member` roots) graph
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

-- | The bare name and subdir a conda locator is addressed by. @depends@
-- entries are never subdir-qualified, so the subdir is matched separately
-- rather than folded into the name the way the dependency name is.
condaMatchKey :: Maybe PixiPackage -> PixiLocator -> Maybe (Text, Maybe Text)
condaMatchKey meta = \case
  PypiLocator{} -> Nothing
  CondaLocator url -> case localSourcePath url of
    -- A local package has no artifact filename to read a subdir out of, so it
    -- answers to any subdir that names it.
    Just _ -> (,Nothing) <$> (pixiPkgName =<< meta)
    Nothing -> do
      CondaArtifact{..} <- parseCondaArtifactUrl url
      pure (fromMaybe condaArtifactName (pixiPkgName =<< meta), Just condaArtifactPlatform)

pypiMatchKey :: Maybe PixiPackage -> PixiLocator -> Maybe Text
pypiMatchKey meta = \case
  CondaLocator{} -> Nothing
  PypiLocator{} -> normalizePypiName <$> (pixiPkgName =<< meta)

-- | The package named at the head of a conda match-spec:
-- @"conda-forge::python >=3.9,<3.10"@ is @python@. Channel prefix, version
-- and build glob are all separated from the name by a character that cannot
-- appear in one.
condaSpecName :: Text -> Text
condaSpecName spec = Text.takeWhile (\c -> not (isSpace c) && c `notElem` ['=', '>', '<', '!', '~']) afterChannel
  where
    afterChannel = case Text.breakOn "::" (Text.strip spec) of
      (before, rest) | Text.null rest -> before
      (_, rest) -> Text.drop 2 rest

-- | The distribution named at the head of a PEP 508 requirement:
-- @"requests[socks]>=2 ; python_version<'3.9'"@ is @requests@.
--
-- Markers are deliberately ignored rather than evaluated. We have no
-- interpreter to evaluate them against, and an edge reported on a platform
-- that would not install it is a far smaller error than dropping a real one.
pypiSpecName :: Text -> Text
pypiSpecName = Text.takeWhile (\c -> isAlphaNum c || c `elem` ['-', '_', '.']) . Text.stripStart

-- | PEP 503 normalization, so @zope.interface@ in a @requires_dist@ finds the
-- package the lockfile spells @zope-interface@. Used only for matching; the
-- reported dependency name is whatever the lockfile said.
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

    -- Names the package when the lockfile gave us one, because "skipping
    -- ./links/requests-2.31.0-py3-none-any.whl" is a worse thing to read in
    -- a scan log than "skipping requests".
    described fallback = maybe fallback (\n -> n <> " (" <> fallback <> ")") (pixiPkgName =<< metadata)

    -- Resolved later by the path-dependency scanner, which license-scans the
    -- directory the name points at. See 'App.Fossa.PathDependency'.
    pathDep path = mkDep UnresolvedPathType path (pixiPkgVersion =<< metadata)

    -- Classify before parsing. parseCondaArtifactUrl reads channel, subdir and
    -- filename out of URL segments, and a path has none of those: a shallow one
    -- yielded Nothing and the package vanished, while a deeper one had enough
    -- segments to produce a name built from directories ('shared':pkgs:zlib).
    --
    -- v5 spells name and version out; v6/v7 leave the filename as the only
    -- source for them. Prefer the explicit fields when the lockfile has them.
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

    -- The fragment is the resolved commit; pixi also writes a `?rev=` query
    -- that names the same commit less precisely.
    gitRevision url = case Text.breakOnEnd "#" url of
      ("", _) -> Nothing
      (_, sha) | Text.null sha -> Nothing
      (_, sha) -> Just sha

analyze :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze lockPath = do
  lockFile <- readContentsYaml @PixiLockFile lockPath
  for_ (outOfRangeVersionWarning $ pixiLockVersion lockFile) warn
  graph <- buildGraph lockFile
  -- Checked for every version, not just unknown ones: entries go unreadable
  -- because the `packages` shape moved, which a pixi release can do without
  -- touching `version:`. Keying this off the version number would reinstate
  -- the brittleness it replaces.
  let entries = length $ pixiLockPackages lockFile
  if entries > 0 && null (Graphing.vertexList graph)
    then fatal $ PixiLockYieldedNoDeps (pixiLockVersion lockFile) entries
    else pure graph

-- | What to say about a version outside 'knownLockVersions', if anything.
-- Pure so the wording is testable without standing up the diagnostics stack.
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
