{-# LANGUAGE TemplateHaskell #-}

module Strategy.Cargo.Lock (
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
  reqSatisfies,

  -- * Source-selection decision (pure, for the analyze-time fallback)
  LockfileSource (..),
  analysisLockfileSource,
  decideLockfileSource,
  StaticLockfileOutcome (..),
  staticLockfileOutcome,

  -- * Lockfile reading + static-analysis diagnostics (from Strategy.Cargo)
  readLockfile,
  lockfileToDeps,
  lockfileToMetadata,
  MissingCargoLockFile (..),
  UnparseableCargoLockFile (..),
  FailedToReadCargoLockFile (..),
  FailedToParseCargoLockFile (..),
) where

import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, Has, context, warn)
import Data.Bifunctor (second)
import Data.Char qualified as Char
import Data.Foldable (for_)
import Data.Function (on)
import Data.Glob qualified
import Data.List (foldl', intercalate, isPrefixOf, nub, nubBy, sortBy, unsnoc)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, mapMaybe)
import Data.SemVer (fromText)
import Data.SemVer.Internal (Version (..))
import Data.Set qualified as Set
import Data.String.Conversion (toString, toText)
import Data.Text (Text, breakOn, splitOn)
import Data.Text qualified as Text
import Diag.Diagnostic (ToDiagnostic (..), renderDiagnostic)
import Discovery.Walk (WalkStep (WalkSkipSome), walk')
import Effect.ReadFS (ReadFS, ReadFSErr (..), doesFileExist, readContentsText', readContentsToml)
import Errata (Errata (..))
import Graphing (Graphing)
import Path (Abs, Dir, File, Path, Rel, dirname, filename, mkRelFile, parseAbsDir, parseRelFile, stripProperPrefix, toFilePath, (</>))
import Strategy.Cargo.MetadataCmd (buildGraph)
import Strategy.Cargo.Types (CargoMetadata (..), NodeDepKind (..), NodeDependency (..), Package (..), PackageId (..), Resolve (..), ResolveNode (..))
import Toml (Result (Failure, Success), Table' (MkTable), Value, Value' (..), decode)
import Toml.Schema qualified
import Types (Dependency (..), GraphBreadth (Complete))

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

instance ToDiagnostic CargoLockError where
  renderDiagnostic (TomlParseError reason) = renderDiagnostic reason
  renderDiagnostic (UnsupportedVersion version) =
    Errata (Just ("unsupported Cargo.lock version " <> toText version)) [] Nothing

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
checkVersion mVersion packages
  | Nothing <- mVersion =
      -- No version field at all: pre-v1 layout, reject as unsupported.
      Left (UnsupportedVersion 0)
  | Just version <- mVersion, version < 3 = Left (UnsupportedVersion version)
  | Just version <- mVersion =
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
  , depKindReqs :: Map.Map (Text, Text) [(Text, DepKind)]
  -- ^ The same pairs, but keyed per declared version requirement:
  -- @(parent crate name, dependency crate name)@ to the list of
  -- @(requirement text, kind)@ pairs as declared in the member's manifest.
  -- A member may declare the same crate under several semver-incompatible
  -- requirements with different kinds (legal for 0.x crates), so each lockfile
  -- version must be labeled from the requirement that matches it. Declarations
  -- without a version requirement (path / git-only deps) are absent here and
  -- covered by 'depKindMap' instead.
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
          [ Map.fromListWith Set.union [(k, Set.singleton knd) | (k, _, knd) <- klist]
          | klist <- ks
          ]
      reqKinds =
        let byReq :: Map.Map (Text, Text) (Map.Map Text (Set.Set DepKind))
            byReq =
              Map.unionsWith
                (Map.unionWith Set.union)
                [ Map.fromListWith (Map.unionWith Set.union) [(k, Map.singleton r (Set.singleton knd)) | (k, Just r, knd) <- klist]
                | klist <- ks
                ]
         in Map.map (\(m :: Map.Map Text (Set.Set DepKind)) -> [(r, kd) | (r, set) <- Map.toList m, kd <- Set.toList set]) byReq
      pathDeps = Map.unions [Map.fromList plist | plist <- paths]
      warnings = nub (concat warns)
   in ManifestAnalysis kindMap reqKinds pathDeps warnings

-- | Pure core of member-dep collection for a single member manifest.
collectMemberDeps ::
  Text ->
  Path Abs Dir ->
  Map.Map Text Value ->
  Maybe (Map.Map Text Value) ->
  ([((Text, Text), Maybe Text, DepKind)], [(Text, Path Abs Dir)], [Text])
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
-- declaring kind and declared version requirement, if any), any @path@ it
-- carries, and any warning.
depEntry ::
  Text ->
  Path Abs Dir ->
  DepKind ->
  Maybe (Map.Map Text Value) ->
  (Text, Value) ->
  ([((Text, Text), Maybe Text, DepKind)], [(Text, Path Abs Dir)], [Text])
depEntry memberName memberDir knd wsDeps (key, v) =
  let (crateName, warns) = resolveCrateName wsDeps key v
      req = depVersionReq wsDeps key v
      pathStr = vMap v >>= (\t -> t .!? "path" >>= vText)
      pathAbs = pathStr >>= resolvePathDep memberDir
   in ( [((memberName, crateName), req, knd)]
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

-- | The version requirement a dependency entry declares, if any. A plain
-- string value (@foo = "1.2"@) is the requirement itself; an inline table
-- carries it under its @version@ key; a @workspace = true@ entry inherits it
-- from the corresponding @[workspace.dependencies]@ entry (which may be a
-- plain string or a table with its own @version@). Path-only and git-only
-- dependencies declare no requirement.
depVersionReq :: Maybe (Map.Map Text Value) -> Text -> Value -> Maybe Text
depVersionReq wsDeps key v =
  case vMap v >>= (.!? "workspace") >>= vBool of
    Just True -> wsDeps >>= (.!? key) >>= versionReqOf
    _ -> versionReqOf v
  where
    versionReqOf :: Value -> Maybe Text
    versionReqOf x = vText x <|> (vMap x >>= (.!? "version") >>= vText)

-- ===========================================================================
-- Cargo version-requirement matching (pure)

-- | The comparator a manifest version requirement was declared with. A bare
-- requirement carries no prefix and uses Cargo caret semantics ('CaretReq').
data CargoReqOp
  = CaretReq
  | TildeReq
  | ReqEq
  | ReqGte
  | ReqLte
  | ReqGt
  | ReqLt
  deriving (Eq, Show)

-- | Check whether a resolved crate version satisfies the version requirement
-- a member's manifest declared for a dependency. Semantics implemented (per
-- Cargo's documented caret rules, mirroring the @semver@ crate):
--
--   * bare / "^": >=M.m.p with the upper bound at the next nonzero-component
--     increment -- ^1.2.3 := >=1.2.3 <2.0.0, ^0.2 := >=0.2.0 <0.3.0,
--     ^0.0.5 := =0.0.5; an all-zero version (^0, ^0.0) has no upper bound;
--   * "~" (tilde): bumps the minor when one is given (~1.2 := >=1.2.0 <1.3.0),
--     otherwise the major (~1 := >=1.0.0 <2.0.0);
--   * "=", ">=", "<=", ">", "<": applied to the requirement with its missing
--     components padded with zero (=1.2 means =1.2.0, not a 1.2.x range).
--
-- An unparseable requirement or resolved version returns @False@, so callers
-- conservatively fall back to the union of all declared kinds.
reqSatisfies :: Text -> Text -> Bool
reqSatisfies reqText resolvedText =
  case parseCargoReq (Text.strip reqText) of
    Nothing -> False
    Just (op, parts) ->
      case fromText resolvedText of
        Left _ -> False
        Right v -> satisfiesCargoOp op parts (_versionMajor v, _versionMinor v, _versionPatch v)

parseCargoReq :: Text -> Maybe (CargoReqOp, [Int])
parseCargoReq t =
  case parseVersionParts rest of
    Nothing -> Nothing
    Just parts -> Just (op, parts)
  where
    (op, rest) = pickReqOp t

pickReqOp :: Text -> (CargoReqOp, Text)
pickReqOp t
  | Just r <- Text.stripPrefix ">=" t = (ReqGte, r)
  | Just r <- Text.stripPrefix "<=" t = (ReqLte, r)
  | Just r <- Text.stripPrefix "^" t = (CaretReq, r)
  | Just r <- Text.stripPrefix "~" t = (TildeReq, r)
  | Just r <- Text.stripPrefix "=" t = (ReqEq, r)
  | Just r <- Text.stripPrefix ">" t = (ReqGt, r)
  | Just r <- Text.stripPrefix "<" t = (ReqLt, r)
  | otherwise = (CaretReq, t)

-- | Split into 1 to 3 all-digit components; anything else (empty input,
-- pre-release/build suffixes, more than three parts) is rejected.
parseVersionParts :: Text -> Maybe [Int]
parseVersionParts t
  | Text.null t = Nothing
  | otherwise =
      let parts = splitOn "." t
          parsed = traverse parseIntPart parts
       in if length parts > 3 then Nothing else parsed
  where
    parseIntPart :: Text -> Maybe Int
    parseIntPart p
      | Text.null p || not (Text.all Char.isDigit p) = Nothing
      | otherwise = Just (read (toString p))

-- | Apply one parsed requirement to a resolved version. Missing components
-- are padded with zero (>=1.2 means >=1.2.0; =1.2 means exactly 1.2.0).
satisfiesCargoOp :: CargoReqOp -> [Int] -> (Int, Int, Int) -> Bool
satisfiesCargoOp op parts v =
  case op of
    CaretReq -> v >= lower && maybe True (v <) caretUpper
    TildeReq -> v >= lower && v < upper
    ReqEq -> v == lower
    ReqGte -> v >= lower
    ReqLte -> v <= lower
    ReqGt -> v > lower
    ReqLt -> v < lower
  where
    (maj, min, pat) = padParts parts
    lower = (maj, min, pat)

    -- Bump the minor when declared, otherwise the major.
    upper =
      case parts of
        (_ : _ : _) -> (maj, min + 1, 0)
        _ -> (maj + 1, 0, 0)

    -- Cargo caret: bump the leftmost nonzero component; all-zero has no upper bound.
    caretUpper
      | maj > 0 = Just (maj + 1, 0, 0)
      | min > 0 = Just (0, min + 1, 0)
      | pat > 0 = Just (0, 0, pat + 1)
      | otherwise = Nothing

padParts :: [Int] -> (Int, Int, Int)
padParts =
  \case
    [a] -> (a, 0, 0)
    [a, b] -> (a, b, 0)
    [a, b, c] -> (a, b, c)
    _ -> (0, 0, 0)

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
--
-- Implemented with 'Discovery.Walk.walk'': the handler collects a directory
-- when it holds a @Cargo.toml@ and prunes @target@/@node_modules@/hidden
-- subdirectories via 'WalkStep.WalkSkipSome'. As @walk'@ visits the root before
-- descending and prunes only subdirectories, this matches a plain recursive
-- descent that always enters the root. The pre-order traversal of @walk'@
-- (parent before children) reproduces the previous list ordering.
findManifestDirs ::
  (Has ReadFS sig m, Has Diagnostics sig m) =>
  Path Abs Dir ->
  m [Path Abs Dir]
findManifestDirs = walk' visit
  where
    visit dir subdirs files =
      let hasToml = any ((== "Cargo.toml") . toString . filename) files
          -- 'WalkSkipSome' with an empty list is equivalent to 'WalkContinue',
          -- so this always prunes the unwanted subdirs when there are any.
          prune = map (toText . toFilePath . dirname) $ filter (not . shouldDescendDir) subdirs
       in pure (if hasToml then [dir] else [], WalkSkipSome prune)

    shouldDescendDir :: Path Abs Dir -> Bool
    shouldDescendDir d =
      let name = toString (dirname d)
       in not ("." `isPrefixOf` name)
            && name /= "target"
            && name /= "node_modules"

-- ===========================================================================
-- Source-selection decision (pure, for the analyze-time fallback)

-- | Which data source 'Strategy.Cargo' should build its graph from: the local
-- @Cargo.lock@ (already parsed) or the output of @cargo metadata@. Exposed as a
-- pure function so the analyze-time fallback logic is testable without 'Exec'.
--
-- The local @Cargo.lock@ is preferred; @cargo metadata@ is used only when the
-- lockfile is absent or fails to parse, so a committed lockfile always wins.
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

-- ===========================================================================
-- Lockfile reading + static-analysis diagnostics
--
-- These moved from "Strategy.Cargo". 'readLockfile', 'lockfileToDeps' and
-- 'lockfileToMetadata' form the lockfile-first analysis path; the four
-- diagnostic newtypes surface the lockfile errors that the static and dynamic
-- paths turn into warnings or fatals.

-- | Read and parse the @Cargo.lock@ in @manifestDir@, if one exists.
-- 'Nothing' means no lockfile is present; @Just (Left err)@ means a lockfile
-- is present but did not parse; @Just (Right lock)@ is the parsed lockfile.
-- A lockfile that cannot be read (an IO error) surfaces as @Left err@ instead
-- of aborting, so callers that cannot recover (the static path) can turn it
-- into a fatal while callers with a fallback (the 'analyze' path) can warn
-- and use @cargo metadata@ instead.
readLockfile :: Has ReadFS sig m => Path Abs Dir -> m (Either ReadFSErr (Maybe (Either CargoLockError CargoLock)))
readLockfile manifestDir = do
  let lockPath = manifestDir </> $(mkRelFile "Cargo.lock")
  exists <- doesFileExist lockPath
  if exists
    then do
      contents <- readContentsText' lockPath
      pure (contents >>= \text -> Right (Just (parseCargoLock text)))
    else pure (Right Nothing)

-- | Build the dependency graph from a parsed lockfile and workspace manifest
-- analysis, without running @cargo@. Shared by the lockfile paths of
-- 'analyze' and 'AnalyzeProject.analyzeProjectStaticOnly'.
lockfileToDeps ::
  ( Has Diagnostics sig m
  , Has ReadFS sig m
  ) =>
  Bool ->
  Path Abs Dir ->
  CargoLock ->
  m (Graphing Dependency, GraphBreadth)
lockfileToDeps emitGitBackedLocators manifestDir lock =
  context "Building dependency graph" $ do
    members <- enumerateWorkspaceMembers manifestDir
    analysis <- analyzeManifests manifestDir members
    for_ (lockWarnings lock) warn
    for_ (manifestWarnings analysis) warn
    let graph = buildGraph emitGitBackedLocators (lockfileToMetadata lock members analysis)
    pure (graph, Complete)

-- | Rebuild a 'CargoMetadata' from a parsed 'CargoLock' plus workspace manifest
-- analysis, so 'buildGraph' can be driven without running @cargo metadata@.
--
-- The 'PackageId's are constructed so that 'toDependency' classifies each
-- dependency exactly as it would for the equivalent @cargo metadata@ output:
--
--   * registry / sparse packages use their source string verbatim, yielding
--     'CargoType' with the crate name and version;
--   * git packages keep their source verbatim in the package list (so the
--     commit hash is available to 'toDependency' via the source map) while the
--     'PackageId' itself drops the @#commit@ fragment, so untagged git deps
--     surface the commit hash as their version and tagged deps keep the crate
--     version, matching the metadata path;
--   * workspace members and other path packages use an absolute @file://@
--     source (with a @path+@ prefix for members), yielding 'UnresolvedPathType'
--     with the absolute path as the dependency name.
--
-- Dependency edge kinds come from the manifest analysis: a dependency declared
-- by a workspace member carries one 'NodeDepKind' per kind whose declared
-- version requirement matches the lockfile entry's resolved version (so a
-- crate declared as both a @\[dependencies\]@ and a @\[build-dependencies\]@
-- dep at incompatible versions is labeled per version, not unioned across
-- them). When no requirement matches -- an undeclared or versionless
-- declaration, or a lockfile that drifted ahead of the manifest -- the union
-- of all declared kinds is used instead. Edges whose parent is not a member
-- default to a single null kind -- the documented, accepted gap (see the
-- module header of "Strategy.Cargo.Lock").
lockfileToMetadata :: CargoLock -> [WorkspaceMember] -> ManifestAnalysis -> CargoMetadata
lockfileToMetadata lock members analysis =
  CargoMetadata
    { metadataPackages = packages
    , metadataWorkspaceMembers = memberIds
    , metadataResolve = Resolve resolveNodes
    }
  where
    memberDirs = Map.fromList [(memberName m, memberDir m) | m <- members]

    isMemberPath p = isNothing (packageSource p) && isJust (Map.lookup (packageName p) memberDirs)

    pkgIdentity p =
      let name = packageName p
          version = fromMaybe "*" (packageVersion p)
       in case packageSource p of
            Nothing ->
              case Map.lookup name memberDirs of
                Just dir -> (PackageId name version ("path+file://" <> toText (toFilePath dir)), Nothing)
                Nothing ->
                  case Map.lookup name (manifestPathDeps analysis) of
                    Just dir -> (PackageId name version ("file://" <> toText (toFilePath dir)), Nothing)
                    -- No member declared this path; fall back to a bare file
                    -- source so the dependency still resolves to
                    -- 'UnresolvedPathType'.
                    Nothing -> (PackageId name version ("file://" <> name), Nothing)
            Just src
              | "git+" `Text.isPrefixOf` src ->
                  -- Keep the query (?tag=/?branch=) but drop the trailing
                  -- #commit fragment from the id; the full source (with the
                  -- hash) is kept in the source map so 'toDependency' can
                  -- recover it.
                  let base = fst (breakOn "#" src)
                   in (PackageId name version base, Just src)
              | otherwise -> (PackageId name version src, Just src)

    labeled = [(p, pkgIdentity p) | p <- lockPackages lock]

    -- (name, version) -> PackageId, for resolving "name version" dep strings.
    pidByNameVer = Map.fromList [((packageName p, fromMaybe "*" (packageVersion p)), pid) | (p, (pid, _)) <- labeled]

    -- name -> [PackageId], a fallback for references that don't pin a version.
    pidByName = Map.fromListWith (++) [(packageName p, [pid]) | (p, (pid, _)) <- labeled]

    packages =
      [ Package (pkgIdName pid) (pkgIdVersion pid) pid Nothing Nothing [] srcUrl
      | (_, (pid, srcUrl)) <- labeled
      ]

    memberIds = [pid | (p, (pid, _)) <- labeled, isMemberPath p]

    resolveNodes =
      [ ResolveNode pid [NodeDependency (resolveTarget dep) (nodeDepKinds p dep) | dep <- packageDependencies p]
      | (p, (pid, _)) <- labeled
      ]

    resolveTarget dep =
      case Map.lookup (depName dep, fromMaybe "*" (depVersion dep)) pidByNameVer of
        Just pid -> pid
        Nothing ->
          case Map.lookup (depName dep) pidByName of
            Just (pid : _) -> pid
            _ -> PackageId (depName dep) (fromMaybe "*" (depVersion dep)) ""

    nodeDepKinds p dep =
      let parentName = packageName p
          -- The version the lockfile entry was resolved to; "*" for entries
          -- without a version (path / git deps), which matches no requirement.
          resolvedVer = fromMaybe "*" (depVersion dep)
          -- Fallback: the union of every kind this member declares for the
          -- crate, regardless of version. Used when the declaration has no
          -- version at all or none of the declared requirements match the
          -- resolved version (e.g. the lockfile drifted ahead of the manifest).
          unionKinds =
            case Map.lookup (parentName, depName dep) (depKindMap analysis) of
              Just kinds -> map depKindToNdk (Set.toList kinds)
              Nothing -> [prodNdk]
       in if not (isMemberPath p)
            then [prodNdk]
            else
              let perVersionKinds =
                    Set.fromList [knd | (req, knd) <- fromMaybe [] (Map.lookup (parentName, depName dep) (depKindReqs analysis)), reqSatisfies req resolvedVer]
               in if Set.null perVersionKinds
                    then unionKinds
                    else map depKindToNdk (Set.toList perVersionKinds)

    depKindToNdk DepProd = prodNdk
    depKindToNdk DepDev = NodeDepKind (Just "dev") Nothing
    depKindToNdk DepBuild = NodeDepKind (Just "build") Nothing

    prodNdk = NodeDepKind Nothing Nothing

newtype MissingCargoLockFile = MissingCargoLockFile (Path Abs File)
instance ToDiagnostic MissingCargoLockFile where
  renderDiagnostic (MissingCargoLockFile path) = do
    let header =
          "Cannot analyze Cargo project statically: no Cargo.lock was found for cargo manifest: "
            <> toText path
            <> ". Static analysis reads Cargo.lock directly and cannot run `cargo metadata` to generate one."
    Errata (Just header) [] Nothing

newtype UnparseableCargoLockFile = UnparseableCargoLockFile (Path Abs File, Errata)
instance ToDiagnostic UnparseableCargoLockFile where
  renderDiagnostic (UnparseableCargoLockFile (path, reason)) = do
    let header =
          "Cannot analyze Cargo project statically: could not parse Cargo.lock for cargo manifest: "
            <> toText path
            <> ": "
            <> renderErrataReason reason
            <> "."
    Errata (Just header) [] Nothing

newtype FailedToReadCargoLockFile = FailedToReadCargoLockFile (Path Abs File, Errata)
instance ToDiagnostic FailedToReadCargoLockFile where
  renderDiagnostic (FailedToReadCargoLockFile (path, reason)) = do
    let header =
          "Could not read Cargo.lock for cargo manifest: "
            <> toText path
            <> ": "
            <> renderErrataReason reason
            <> ". Falling back to `cargo metadata`."
    Errata (Just header) [] Nothing

newtype FailedToParseCargoLockFile = FailedToParseCargoLockFile (Path Abs File, Errata)
instance ToDiagnostic FailedToParseCargoLockFile where
  renderDiagnostic (FailedToParseCargoLockFile (path, reason)) = do
    let header =
          "Could not parse Cargo.lock for cargo manifest: "
            <> toText path
            <> ": "
            <> renderErrataReason reason
            <> ". Falling back to `cargo metadata`."
    Errata (Just header) [] Nothing

-- | Render the human-readable text of an 'Errata' (its body, falling back to
-- its header) so it can be embedded in a single-line diagnostic header. The
-- 'Strategy.Cargo.Lock' and 'Effect.ReadFS' diagnostic instances only ever
-- populate the header and body, never source blocks.
renderErrataReason :: Errata -> Text
renderErrataReason (Errata{errataBody = Just body}) = body
renderErrataReason (Errata{errataHeader = Just header}) = header
renderErrataReason _ = ""
