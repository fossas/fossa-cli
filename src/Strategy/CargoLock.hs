{-# LANGUAGE TemplateHaskell #-}

module Strategy.CargoLock (
  CargoLock (..),
  CargoPackage (..),
  CargoDependency (..),
  CargoLockError (..),
  parseCargoLock,
  parseDependencyString,

  -- * Workspace manifest analysis
  WorkspaceMember (..),
  DepKind (..),
  ManifestAnalysis (..),
  enumerateWorkspaceMembers,
  analyzeManifests,

  -- * Source-selection decision (pure, for the analyze-time fallback)
  LockfileSource (..),
  analysisLockfileSource,
  decideLockfileSource,
  StaticLockfileOutcome (..),
  staticLockfileOutcome,
) where

import Control.Effect.Diagnostics (Diagnostics, Has)
import Data.Bifunctor (second)
import Data.Function (on)
import Data.Glob qualified
import Data.List (foldl', intercalate, isPrefixOf, nub, nubBy, sortBy, unsnoc)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.String.Conversion (toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Effect.ReadFS (ReadFS, listDir, readContentsToml)
import Path (
  Abs,
  Dir,
  Path,
  Rel,
  dirname,
  filename,
  mkRelFile,
  parseAbsDir,
  parseRelFile,
  stripProperPrefix,
  toFilePath,
  (</>),
 )
import Toml (Result (Failure, Success), Table' (MkTable), Value, Value' (..), decode)
import Toml.Schema qualified

-- | A parsed Cargo.lock file.
--
-- @lockVersion@ is the top-level "version" key; @lockPackages@ are the
-- [[package]] entries in file order.
data CargoLock = CargoLock
  { lockVersion :: Int
  -- ^ Warnings raised during parsing, e.g. when the lockfile version
  -- is newer than the latest one this parser was written against.
  , lockWarnings :: [Text]
  , lockPackages :: [CargoPackage]
  }
  deriving (Eq, Show)

-- | A single [[package]] entry in a Cargo.lock file.
data CargoPackage = CargoPackage
  { packageName :: Text
  -- ^ v4 omits this for some git-sourced packages, so it is stored
  -- as 'Maybe' rather than coerced.
  , packageVersion :: Maybe Text
  -- ^ 'Nothing' identifies local / path dependencies.
  , packageSource :: Maybe Text
  , packageChecksum :: Maybe Text
  , packageDependencies :: [CargoDependency]
  }
  deriving (Eq, Show)

-- | A single entry of a [[package]] "dependencies" array.
--
-- Entries are either "name" or "name version" (crate names contain no
-- spaces), so the version part is optional.
data CargoDependency = CargoDependency
  { depName :: Text
  , depVersion :: Maybe Text
  }
  deriving (Eq, Show)

-- | Errors raised by 'parseCargoLock'.
data CargoLockError
  = -- | The input is not valid TOML (or does not fit the expected
    -- Cargo.lock structure). Carries the parser's message.
    TomlParseError Text
  | -- | The lockfile version is missing or predates v3. A missing
    -- version field (very early Cargo.lock files) is reported as 0.
    UnsupportedVersion Int
  deriving (Eq, Show)

-- | Parse the contents of a Cargo.lock file.
--
-- Versions 3 and 4 are parsed normally. A missing version or a version
-- below 3 uses the older v1/v2 layout ([root]/[metadata] tables) and is
-- rejected. Versions above 4 are parsed on a best-effort basis and, if
-- the rest of the file validates, surface a warning in
-- 'lockWarnings'.
parseCargoLock :: Text -> Either CargoLockError CargoLock
parseCargoLock contents =
  case decode contents of
    Failure errs -> Left $ TomlParseError $ Text.unlines (map toText errs)
    Success _ (RawLock mVersion rawPackages) ->
      checkVersion mVersion (map toPackage rawPackages)

checkVersion :: Maybe Int -> [CargoPackage] -> Either CargoLockError CargoLock
checkVersion mVersion packages =
  case mVersion of
    Nothing ->
      -- No version field at all: pre-v1 layout, reject as unsupported.
      Left (UnsupportedVersion 0)
    Just version ->
      if version < 3
        then Left (UnsupportedVersion version)
        else
          let warnings =
                if version > 4
                  then
                    [ "Cargo.lock version "
                        <> toText version
                        <> " is newer than the supported range (3-4); parsed on a best-effort basis."
                    ]
                  else []
           in Right (CargoLock version warnings packages)

-- | Split a "dependencies" array entry into name and optional version,
-- splitting on the last space (crate names contain no spaces).
--
-- "ansi_term" -> ("ansi_term", Nothing)
-- "ansi_term 0.12.1" -> ("ansi_term", Just "0.12.1")
--
-- Uses the text >=2.0 'Text.breakOnEnd', which returns (the text before the
-- last space, *including* that space, and the text after it); when there is
-- no space it returns ("", whole). A non-empty prefix therefore means a
-- version is present.
parseDependencyString :: Text -> CargoDependency
parseDependencyString s =
  let (prefix, version) = Text.breakOnEnd " " s
   in if Text.null prefix
        then CargoDependency s Nothing
        else CargoDependency (Text.stripEnd prefix) (Just version)

-- ===========================================================================
-- Workspace manifest analysis
--
-- Given the directory that 'findProjects' discovered as a Cargo project root,
-- these helpers figure out (1) which directories are the workspace members and
-- (2) the kinds (production / dev / build) with which each member declares its
-- dependencies. Together with a parsed 'CargoLock' they let 'Strategy.Cargo'
-- rebuild a 'CargoMetadata' without running 'cargo metadata'.
--
-- # Accepted gap
--
-- Dependency kinds are derived only from member manifests. Edges whose parent
-- is *not* a workspace member (e.g. a non-member path dependency or a git
-- package) therefore cannot be classified and default to a single Production
-- ('NodeDepKind' with a null kind) in
-- "Strategy.Cargo".lockfileToMetadata. This is pinned by a golden test.
--
-- Nested non-member path dependencies (a path dependency of a path
-- dependency) get a @file://<crate-name>@ source rather than an absolute
-- path in the dependency name (same 'UnresolvedPathType', different name)
-- because only the path declared by a workspace member is resolvable.

-- | A single workspace member: the @Cargo.toml@ directory plus the crate name
-- declared in its @\[package\]@ table.
data WorkspaceMember = WorkspaceMember
  { memberName :: Text
  -- ^ The @\[package\].name@ of the member's manifest.
  , memberDir :: Path Abs Dir
  -- ^ Absolute directory containing the member's @Cargo.toml@.
  }
  deriving (Eq, Show)

-- | The kind of a dependency as declared in a member's manifest.
data DepKind
  = -- | Declared in @\[dependencies\]@ (or a @target.@ variant of it).
    DepProd
  | -- | Declared in @\[dev-dependencies\]@.
    DepDev
  | -- | Declared in @\[build-dependencies\]@.
    DepBuild
  deriving (Eq, Ord, Show)

-- | The result of analyzing every member manifest of a workspace.
data ManifestAnalysis = ManifestAnalysis
  { depKindMap :: Map.Map (Text, Text) (Set.Set DepKind)
  -- ^ Map from @(parent crate name, dependency crate name)@ to the set of
  -- kinds under which that member declares the dependency. A dependency can
  -- appear in several sections (e.g. both @\[dependencies\]@ and
  -- @\[build-dependencies\]@), so the set can hold several entries.
  , manifestPathDeps :: Map.Map Text (Path Abs Dir)
  -- ^ Best-effort absolute directories of non-member path dependencies, keyed
  -- by crate name, resolved from each member's @path = "..."@ declaration.
  , -- \^ Warnings raised while deriving dependency kinds (e.g. an unresolvable
    -- @workspace = true@ inheritance).
    manifestWarnings :: [Text]
  }
  deriving (Eq, Show)

-- | Enumerate the workspace members of the project rooted at @rootDir@.
--
-- A project without a @\[workspace\]@ table is treated as a single-crate
-- project whose own directory is the sole member. A workspace (virtual or
-- not) is expanded from the union of its @members@ and @default-members@
-- globs; only directories that actually contain a @Cargo.toml@ are kept.
enumerateWorkspaceMembers ::
  (Has ReadFS sig m, Has Diagnostics sig m) =>
  Path Abs Dir ->
  m [WorkspaceMember]
enumerateWorkspaceMembers rootDir = do
  rootM <- readContentsToml @(Map.Map Text Value) (rootDir </> $(mkRelFile "Cargo.toml"))
  case topTable rootM "workspace" of
    Nothing ->
      -- Non-workspace single crate: the root itself is the only member.
      case rootPackage rootM of
        Just name -> pure [WorkspaceMember name rootDir]
        Nothing -> pure []
    Just ws -> do
      let globs = wsGlobs ws
      allDirs <- findManifestDirs rootDir
      candidates <- traverse (memberFor globs) allDirs
      let members = concat candidates
          -- A root manifest with a [package] table always contributes the
          -- root package as a workspace member, even when no glob matches
          -- the root directory; a glob that does match it ("." for example)
          -- must not add a duplicate.
          rootMember =
            case rootPackage rootM of
              Just name -> [WorkspaceMember name rootDir]
              Nothing -> []
      -- Sort for determinism (manifest analysis is order-independent, but a
      -- stable member ordering keeps downstream list output reproducible).
      pure (sortBy (compare `on` memberDir) (nubBy ((==) `on` memberDir) (rootMember ++ members)))
  where
    memberFor globs d = do
      let rel = stripProperPrefix rootDir d
      case rel of
        Nothing -> pure []
        Just relDir ->
          if any (globMatches relDir) globs
            then do
              m <- readContentsToml @(Map.Map Text Value) (d </> $(mkRelFile "Cargo.toml"))
              case rootPackage m of
                Just name -> pure [WorkspaceMember name d]
                Nothing -> pure []
            else pure []

    -- Glob matching works on the path without a trailing slash: a Dir carries
    -- one, and FilePattern's '*' would not match it. Stripping it lets
    -- "crates/*" match "crates/alpha" exactly as Cargo does.
    globMatches :: Path Rel Dir -> Text -> Bool
    globMatches relDir glob =
      case parseRelFile (toString (stripTrailingSlash (toText relDir))) of
        Just f -> Data.Glob.matches f (Data.Glob.unsafeGlobRel (toString glob))
        Nothing -> False
      where
        stripTrailingSlash :: Text -> Text
        stripTrailingSlash s = fromMaybe s (Text.stripSuffix "/" s)

-- | Analyze every member manifest (plus the root's @\[workspace.dependencies\]@)
-- to derive the dependency-kind map and the non-member path-dependency map.
analyzeManifests ::
  (Has ReadFS sig m, Has Diagnostics sig m) =>
  Path Abs Dir ->
  [WorkspaceMember] ->
  m ManifestAnalysis
analyzeManifests rootDir members = do
  rootM <- readContentsToml @(Map.Map Text Value) (rootDir </> $(mkRelFile "Cargo.toml"))
  let wsDeps = topTable rootM "workspace" >>= (\ws -> ws .!? "dependencies" >>= vMap)
  perMember <-
    traverse
      ( \mem -> do
          m <- readContentsToml @(Map.Map Text Value) (memberDir mem </> $(mkRelFile "Cargo.toml"))
          pure (memberName mem, memberDir mem, m)
      )
      members
  pure (combineMembers wsDeps perMember)

-- | Pure core of 'analyzeManifests': fold the per-member manifests into the
-- combined 'ManifestAnalysis'.
combineMembers :: Maybe (Map.Map Text Value) -> [(Text, Path Abs Dir, Map.Map Text Value)] -> ManifestAnalysis
combineMembers wsDeps perMember =
  let triples = [collectMemberDeps name dir m wsDeps | (name, dir, m) <- perMember]
      (ks, paths, warns) = unzip3 triples
      kindMap =
        Map.unionsWith
          Set.union
          [ Map.fromListWith Set.union (map (second Set.singleton) klist)
          | klist <- ks
          ]
      pathDeps = Map.unions [Map.fromList plist | plist <- paths]
      warnings = nub (concat warns)
   in ManifestAnalysis kindMap pathDeps warnings

-- | Pure core of member-dep collection for a single member manifest.
collectMemberDeps ::
  Text ->
  Path Abs Dir ->
  Map.Map Text Value ->
  Maybe (Map.Map Text Value) ->
  ([((Text, Text), DepKind)], [(Text, Path Abs Dir)], [Text])
collectMemberDeps memberName memberDir m wsDeps =
  let perTable (knd, mTbl) =
        case mTbl of
          Nothing -> ([], [], [])
          Just tbl ->
            let entries = fmap (depEntry memberName memberDir knd wsDeps) (Map.toList tbl)
                (entryKs, entryPaths, entryWarns) = unzip3 entries
             in (concat entryKs, concat entryPaths, concat entryWarns)
      (ks, paths, warns) = unzip3 (map perTable (allDepTables m))
   in (concat ks, concat paths, concat warns)

-- | The @\[dependencies\]@ / @\[dev-dependencies\]@ / @\[build-dependencies\]@
-- tables declared directly on the package, plus every @\[target.'...'.*\]@
-- variant, each tagged with the kind it implies.
allDepTables :: Map.Map Text Value -> [(DepKind, Maybe (Map.Map Text Value))]
allDepTables m =
  [ (DepProd, m .!? "dependencies" >>= vMap)
  , (DepDev, m .!? "dev-dependencies" >>= vMap)
  , (DepBuild, m .!? "build-dependencies" >>= vMap)
  ]
    ++ concatMap fromTarget (Map.toList (fromMaybe Map.empty (topTable m "target")))
  where
    fromTarget (_, targetVal) =
      let t = fromMaybe Map.empty (vMap targetVal)
       in [ (DepProd, t .!? "dependencies" >>= vMap)
          , (DepDev, t .!? "dev-dependencies" >>= vMap)
          , (DepBuild, t .!? "build-dependencies" >>= vMap)
          ]

-- | Walk a single dependency entry, resolving renames and workspace
-- inheritance, and report the crate name it refers to (together with the
-- declaring kind), any @path@ it carries, and any warning.
depEntry ::
  Text ->
  Path Abs Dir ->
  DepKind ->
  Maybe (Map.Map Text Value) ->
  (Text, Value) ->
  ([((Text, Text), DepKind)], [(Text, Path Abs Dir)], [Text])
depEntry memberName memberDir knd wsDeps (key, v) =
  let (crateName, warns) = resolveCrateName wsDeps key v
      pathStr = vMap v >>= (\t -> t .!? "path" >>= vText)
      pathAbs = pathStr >>= resolvePathDep memberDir
   in ( [((memberName, crateName), knd)]
      , case pathAbs of
          Just d -> [(crateName, d)]
          Nothing -> []
      , warns
      )

-- | Resolve the crate name a dependency entry refers to. A bare string value
-- (@foo = "1.0"@) names the crate after the key; a table may rename it via
-- @package = "..."@ or inherit it from the workspace via @workspace = true@.
resolveCrateName :: Maybe (Map.Map Text Value) -> Text -> Value -> (Text, [Text])
resolveCrateName wsDeps key v =
  case vText v of
    Just _ -> (key, [])
    Nothing ->
      let t = fromMaybe Map.empty (vMap v)
          pkg = t .!? "package" >>= vText
          ws = t .!? "workspace" >>= vBool
       in case ws of
            Just True ->
              case wsDeps >>= (.!? key) of
                Nothing -> (key, [warnMsg])
                Just entry -> (fromMaybe key (vMap entry >>= (\et -> et .!? "package" >>= vText)), [])
            _ -> (fromMaybe key pkg, [])
  where
    warnMsg =
      "Cargo manifest dependency '"
        <> key
        <> "' uses workspace inheritance but no matching [workspace.dependencies] entry was found; classified as Production."

-- ===========================================================================
-- Pure TOML value navigation helpers (private)

infixl 8 .!?
(.!?) :: Ord k => Map.Map k v -> k -> Maybe v
(.!?) = flip Map.lookup

topTable :: Map.Map Text Value -> Text -> Maybe (Map.Map Text Value)
topTable m key = m .!? key >>= vMap

rootPackage :: Map.Map Text Value -> Maybe Text
rootPackage m = topTable m "package" >>= (\p -> p .!? "name" >>= vText)

wsGlobs :: Map.Map Text Value -> [Text]
wsGlobs ws = nub (concat (catMaybes [ws .!? "members" >>= vStringList, ws .!? "default-members" >>= vStringList]))

vMap :: Value -> Maybe (Map.Map Text Value)
vMap (Table' () (MkTable mp)) = Just (Map.map snd mp)
vMap _ = Nothing

vText :: Value -> Maybe Text
vText (Text' () t) = Just t
vText _ = Nothing

vBool :: Value -> Maybe Bool
vBool (Bool' () b) = Just b
vBool _ = Nothing

vStringList :: Value -> Maybe [Text]
vStringList (List' () xs) = Just (mapMaybe vText xs)
vStringList _ = Nothing

-- | Resolve a member's relative @path = "..."@ declaration (which may contain
-- ".." components, which path-pkg's parsers reject) into an absolute directory.
-- Absolute path declarations are used as-is.
resolvePathDep :: Path Abs Dir -> Text -> Maybe (Path Abs Dir)
resolvePathDep base raw =
  if Text.isPrefixOf "/" raw
    then parseAbsDir (toString raw)
    else parseAbsDir (normalizePathStr (toFilePath base <> toString raw))

-- | Resolve "." and ".." segments in a slash-separated path string.
normalizePathStr :: String -> String
normalizePathStr s =
  let isAbs = case s of
        ('/' : _) -> True
        _ -> False
      acc =
        foldl'
          ( \a seg ->
              case seg of
                "." -> a
                "" -> a
                ".." -> case unsnoc a of
                  Nothing -> a
                  Just (rest, _) -> rest
                _ -> a ++ [seg]
          )
          []
          (splitSlash s)
   in (if isAbs then "/" else "") ++ intercalate "/" acc

-- | Split a string on a character (the path separator here is always '/').
splitSlash :: String -> [String]
splitSlash xs
  | '/' `notElem` xs = [xs]
  | otherwise =
      let (pre, rest) = break (== '/') xs
       in pre : splitSlash (drop 1 rest)

-- | Recursively collect every directory under @root@ that contains a
-- @Cargo.toml@, skipping @target/@, hidden directories, and @node_modules/@.
findManifestDirs ::
  (Has ReadFS sig m, Has Diagnostics sig m) =>
  Path Abs Dir ->
  m [Path Abs Dir]
findManifestDirs = go
  where
    go d = do
      (subdirs, files) <- listDir d
      let hasToml = any ((== "Cargo.toml") . toString . filename) files
          descend = filter (shouldDescend . toString . dirname) subdirs
      ms <- traverse go descend
      pure $ if hasToml then d : concat ms else concat ms

    shouldDescend :: String -> Bool
    shouldDescend name =
      not ("." `isPrefixOf` name)
        && name /= "target"
        && name /= "node_modules"

-- ===========================================================================
-- Source-selection decision (pure, for the analyze-time fallback)

-- | Which data source 'Strategy.Cargo' should build its graph from: the local
-- @Cargo.lock@ (already parsed) or the output of @cargo metadata@. Exposed as a
-- pure function so the analyze-time fallback logic is testable without 'Exec'.
data LockfileSource
  = UseLockfile
  | UseMetadata
  deriving (Eq, Show)

decideLockfileSource :: Either CargoLockError CargoLock -> LockfileSource
decideLockfileSource =
  \case
    Right _ -> UseLockfile
    Left _ -> UseMetadata

-- | The source decision for a full analysis of a Cargo project, given the
-- parse result of its @Cargo.lock@, or 'Nothing' when no @Cargo.lock@ exists
-- in the manifest directory (the @cargo metadata@ path generates one).
analysisLockfileSource :: Maybe (Either CargoLockError CargoLock) -> LockfileSource
analysisLockfileSource =
  \case
    Nothing -> UseMetadata
    Just parsed -> decideLockfileSource parsed

-- | The outcome of resolving the @Cargo.lock@ for a *static* analysis of a
-- Cargo project. Unlike a full analysis, static analysis has no fallback to
-- @cargo metadata@: a missing or unparseable lockfile means the analysis
-- must fail.
data StaticLockfileOutcome
  = -- | A @Cargo.lock@ exists and parsed successfully.
    StaticUseLockfile CargoLock
  | -- | No @Cargo.lock@ exists in the manifest directory.
    StaticMissingLockfile
  | -- | A @Cargo.lock@ exists but could not be parsed.
    StaticUnparseableLockfile CargoLockError
  deriving (Eq, Show)

staticLockfileOutcome :: Maybe (Either CargoLockError CargoLock) -> StaticLockfileOutcome
staticLockfileOutcome =
  \case
    Nothing -> StaticMissingLockfile
    Just (Left err) -> StaticUnparseableLockfile err
    Just (Right lock) -> StaticUseLockfile lock

-- | Pre-validation shape used by the TOML schema: the version is not yet
-- checked and dependency entries are still raw strings.
data RawLock = RawLock
  { rawVersion :: Maybe Int
  , rawPackages :: [RawPackage]
  }
  deriving (Eq, Show)

data RawPackage = RawPackage
  { rawPackageName :: Text
  , rawPackageVersion :: Maybe Text
  , rawPackageSource :: Maybe Text
  , rawPackageChecksum :: Maybe Text
  -- ^ Absent for packages without dependencies, so defaults to [].
  , rawPackageDepStrings :: Maybe [Text]
  }
  deriving (Eq, Show)

instance Toml.Schema.FromValue RawLock where
  fromValue =
    Toml.Schema.parseTableFromValue $
      RawLock
        <$> Toml.Schema.optKey "version"
        -- Cargo.lock stores its packages as a [[package]] array of
        -- tables, so the top-level key is the singular "package".
        <*> Toml.Schema.reqKey "package"

instance Toml.Schema.FromValue RawPackage where
  fromValue =
    Toml.Schema.parseTableFromValue $
      RawPackage
        <$> Toml.Schema.reqKey "name"
        <*> Toml.Schema.optKey "version"
        <*> Toml.Schema.optKey "source"
        <*> Toml.Schema.optKey "checksum"
        <*> Toml.Schema.optKey "dependencies"

toPackage :: RawPackage -> CargoPackage
toPackage raw =
  CargoPackage
    { packageName = rawPackageName raw
    , packageVersion = rawPackageVersion raw
    , packageSource = rawPackageSource raw
    , packageChecksum = rawPackageChecksum raw
    , packageDependencies = maybe [] (map parseDependencyString) (rawPackageDepStrings raw)
    }
