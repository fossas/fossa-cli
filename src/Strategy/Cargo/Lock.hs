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
  unresolvableDeps,
  MissingCargoLockFile (..),
  UnparseableCargoLockFile (..),
  FailedToReadCargoLockFile (..),
  FailedToParseCargoLockFile (..),
) where

import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, Has, context, warn)
import Control.Monad (guard, unless)
import Data.Char qualified as Char
import Data.Foldable (for_)
import Data.Function (on)
import Data.List (dropWhileEnd, foldl', intercalate, isPrefixOf, nub, nubBy, sortBy, unsnoc)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, mapMaybe, maybeToList)
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
import Path (Abs, Dir, File, Path, dirname, filename, mkRelFile, parseAbsDir, stripProperPrefix, toFilePath, (</>))
import Strategy.Cargo.MetadataCmd (buildGraph)
import Strategy.Cargo.Types (CargoMetadata (..), NodeDepKind (..), NodeDependency (..), Package (..), PackageId (..), Resolve (..), ResolveNode (..))
import System.FilePattern qualified as FilePattern
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
-- splitting on the last space (crate names contain no spaces). A trailing
-- parenthesized source suffix -- which Cargo uses to disambiguate the same
-- name+version resolved from different sources -- is stripped first.
--
-- "ansi_term" -> ("ansi_term", Nothing)
-- "ansi_term 0.12.1" -> ("ansi_term", Just "0.12.1")
-- "foo 1.2.3 (git+https://example.com/repo#abc1234)" -> ("foo", Just "1.2.3")
--
-- Uses the text >=2.0 'Text.breakOnEnd', which returns (the text before the
-- last space, *including* that space, and the text after it); when there is
-- no space it returns ("", whole). A non-empty prefix therefore means a
-- version is present.
parseDependencyString :: Text -> CargoDependency
parseDependencyString s =
  let plain = stripSourceSuffix s
      (prefix, version) = Text.breakOnEnd " " plain
   in if Text.null prefix
        then CargoDependency plain Nothing
        else CargoDependency (Text.stripEnd prefix) (Just version)

-- | Strip a trailing parenthesized source suffix from a "dependencies" entry:
-- "foo 1.2.3 (git+https://…#abc)" -> "foo 1.2.3", including the space
-- between the version and the parenthesis. An entry whose stripped form holds
-- no "(" (no well-formed "(…)" group) is returned unchanged.
stripSourceSuffix :: Text -> Text
stripSourceSuffix s =
  case Text.stripSuffix ")" s of
    Nothing -> s
    Just t ->
      let (beforeParen, _) = Text.breakOnEnd "(" t
          -- 'breakOnEnd' keeps the needle in its first component: drop the
          -- " (" that opens the source suffix.
          nameVer = fromMaybe beforeParen (Text.stripSuffix " (" beforeParen)
       in if Text.null nameVer then s else nameVer

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
-- Dependency kinds are derived only from member manifests. A member-declared
-- edge carries the kind(s) its member declares; a non-member edge (a
-- non-member path dependency or a git package) inherits the child's
-- propagated kind set -- the union of the kind sets of every member edge that
-- reaches that child -- in
-- "Strategy.Cargo".lockfileToMetadata. A package that no member edge reaches
-- (stale lockfile residue) therefore defaults to Production.
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
-- entries; directories matching an @exclude@ entry are not searched, and an
-- explicit (non-glob) entry is resolved directly from the root so that
-- out-of-tree members such as @"../shared-lib"@ are reachable. Only
-- directories that actually contain a @[Cargo.toml]@ are kept.
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
      -- Cargo member/exclude entries may use brace alternation
      -- (@crates/{a,b}@); expand each entry lexically before matching so the
      -- resulting paths are treated the same way Cargo would (@crates/a@ and
      -- @crates/b@). Entries without braces pass through unchanged.
      let globs = concatMap expandBraces (wsGlobs ws)
          excludes = concatMap expandBraces (wsExcludes ws)
      allDirs <- findManifestDirs rootDir excludes
      walkedMembers <- concat <$> traverse (memberFor globs) allDirs
      explicitMembers <- concat <$> traverse (explicitMember rootDir) (filter (not . hasGlobChars) globs)
      let members = walkedMembers ++ explicitMembers
          -- A root manifest with a [package] table always contributes the
          -- root package as a workspace member, even when no entry names the
          -- root directory; an entry that does match it ("." for example)
          -- must not add a duplicate.
          rootMember =
            case rootPackage rootM of
              Just name -> [WorkspaceMember name rootDir]
              Nothing -> []
      -- Sort for determinism (manifest analysis is order-independent, but a
      -- stable member ordering keeps downstream list output reproducible).
      pure (sortBy (compare `on` memberDir) (nubBy ((==) `on` memberDir) (rootMember ++ members)))
  where
    memberFor globs d =
      case relPathStr rootDir d of
        Nothing -> pure []
        Just relStr ->
          -- Membership is decided by the member entries alone. Deliberate
          -- divergence from 'cargo metadata', which also drops a directory a
          -- member entry matches when its path is named in 'exclude': per
          -- house policy, exclusions prune only the search above (so such a
          -- directory is not discovered there), and a validly discovered
          -- manifest is always a member. An explicitly listed member is
          -- therefore included even when also excluded.
          if any (globMatches relStr) globs
            then do
              m <- readContentsToml @(Map.Map Text Value) (d </> $(mkRelFile "Cargo.toml"))
              case rootPackage m of
                Just name -> pure [WorkspaceMember name d]
                Nothing -> pure []
            else pure []

    -- An explicit (non-glob) entry names one specific directory; it is
    -- resolved from the workspace root directly rather than relying on the
    -- manifest walk to reach it, so out-of-tree entries ("../sibling") work
    -- even though the walk never leaves 'rootDir'. A missing directory, an
    -- unparseable entry, or a manifest without a [package] table is reported
    -- as a warning (rather than skipped silently) so a drifted members list
    -- stays visible.
    explicitMember dir entry =
      case resolveMemberEntry dir entry of
        Nothing -> do
          warn (unresolvableMemberMsg entry)
          pure []
        Just d -> do
          let manifest = d </> $(mkRelFile "Cargo.toml")
          exists <- doesFileExist manifest
          if exists
            then do
              m <- readContentsToml @(Map.Map Text Value) manifest
              case rootPackage m of
                Just name -> pure [WorkspaceMember name d]
                Nothing -> do
                  warn (unresolvableMemberMsg entry)
                  pure []
            else do
              warn (unresolvableMemberMsg entry)
              pure []

-- | Warning text for a workspace member entry that failed to resolve to a
-- manifest carrying a @\[package\]@ table (a missing/unparseable directory, or
-- a manifest without a package name). Surfaced via 'warn' so a drifted
-- @members@ list stays visible instead of dropping the member silently.
unresolvableMemberMsg :: Text -> Text
unresolvableMemberMsg entry =
  "Cargo workspace member entry '" <> entry <> "' did not resolve to a manifest with a [package] table; omitted from the dependency graph."

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
  let triples = map (\(name, dir, m) -> collectMemberDeps name dir m wsDeps) perMember
      (ks, paths, warns) = unzip3 triples
      kindMap =
        Map.unionsWith Set.union $
          map
            ( \klist ->
                Map.fromListWith Set.union $ do
                  (k, _, knd) <- klist
                  pure (k, Set.singleton knd)
            )
            ks
      reqKinds =
        let byReq :: Map.Map (Text, Text) (Map.Map Text (Set.Set DepKind))
            byReq =
              Map.unionsWith
                (Map.unionWith Set.union)
                ( map
                    ( \klist ->
                        Map.fromListWith (Map.unionWith Set.union) $ do
                          (k, Just r, knd) <- klist
                          pure (k, Map.singleton r (Set.singleton knd))
                    )
                    ks
                )
         in Map.map
              ( \(m :: Map.Map Text (Set.Set DepKind)) -> do
                  (r, set) <- Map.toList m
                  kd <- Set.toList set
                  pure (r, kd)
              )
              byReq
      pathDeps = Map.unions (map Map.fromList paths)
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
--     components padded with zero (=1.2 means =1.2.0, not a 1.2.x range);
--   * a comma-joined list ("1.2, <1.2.5") is the conjunction of its parts;
--   * a requirement without a prerelease component never matches a
--     prerelease resolution (SemVer rule).
--
-- A requirement part that fails to parse, or an unparseable resolved version,
-- returns @False@, so callers conservatively fall back to the union of all
-- declared kinds.
reqSatisfies :: Text -> Text -> Bool
reqSatisfies reqText resolvedText =
  case fromText resolvedText of
    Left _ -> False
    Right v ->
      let ver = (_versionMajor v, _versionMinor v, _versionPatch v)
          -- A requirement without a prerelease never matches a prerelease resolution.
          isPreRelease = not (null (_versionRelease v))
          matchesPart part =
            case parseCargoReq (Text.strip part) of
              Nothing -> False
              Just (op, parts) -> satisfiesCargoOp op parts ver && (not isPreRelease || "-" `Text.isInfixOf` part)
       in all matchesPart (splitOn "," reqText)

-- | Parse one comma-separated requirement part into its operator and 1 to 3
-- numeric version components; 'reqSatisfies' splits the conjunction.
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
    (maj, mnr, pat) = padParts parts
    lower = (maj, mnr, pat)

    -- Bump the minor when declared, otherwise the major.
    upper =
      case parts of
        (_ : _ : _) -> (maj, mnr + 1, 0)
        _ -> (maj + 1, 0, 0)

    -- Cargo caret: bump the leftmost nonzero component; all-zero has no upper bound.
    caretUpper
      | maj > 0 = Just (maj + 1, 0, 0)
      | mnr > 0 = Just (0, mnr + 1, 0)
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

-- | The @[workspace]@ 'exclude' entries, if any. House policy: exclusions
-- prune the *search* for member manifests ('findManifestDirs') but never
-- remove a validly discovered manifest from membership.
wsExcludes :: Map.Map Text Value -> [Text]
wsExcludes ws = fromMaybe [] (ws .!? "exclude" >>= vStringList)

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

-- | Resolve an explicit (non-glob) workspace-member entry to an absolute
-- directory. The entry is relative to 'root'; lexical '.'/'..' segments are
-- normalized (via 'normalizePathStr'), so an out-of-tree member such as
-- "../shared-lib" resolves outside the walked tree -- which the manifest walk
-- never reaches -- while "./x" and "x" resolve inside it.
resolveMemberEntry :: Path Abs Dir -> Text -> Maybe (Path Abs Dir)
resolveMemberEntry root raw = parseAbsDir (normalizePathStr (toFilePath root <> "/" <> toString raw))

-- | Resolve "." and ".." segments in a path string. Backslashes (from
-- 'toFilePath' on Windows) are folded onto forward slashes first, since Cargo
-- member/exclude and path entries use forward slashes and POSIX never emits
-- them (so this folding is a no-op there); the "."/".." resolution then
-- operates on a single, slash-separated form.
normalizePathStr :: String -> String
normalizePathStr s =
  let fs = map (\c -> if c == '\\' then '/' else c) s
      isAbs = case fs of
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
          (splitSlash fs)
   in (if isAbs then "/" else "") ++ intercalate "/" acc

-- | Split a string on '/' (the separator after backslashes have been folded
-- onto '/' by 'normalizePathStr').
splitSlash :: String -> [String]
splitSlash xs
  | '/' `notElem` xs = [xs]
  | otherwise =
      let (pre, rest) = break (== '/') xs
       in pre : splitSlash (drop 1 rest)

-- | Match a relative directory path (as a string without trailing slash)
-- against a workspace @members@/@exclude@ entry with 'System.FilePattern'
-- semantics: '*' and '?' match within one segment, '**' across segments,
-- and literal '.'/'..' components are allowed. String-based matching --
-- rather than matching a parsed 'Path Rel', which rejects '..' -- keeps
-- out-of-tree entries such as "../sibling" expressible, the same primitives
-- the filter machinery in "Discovery.Filters" matches with.
globMatches :: Text -> Text -> Bool
globMatches relStr glob = toString glob FilePattern.?== toString relStr

-- | Whether a workspace entry is a glob (contains a wildcard character)
-- rather than a plain path. Brace alternation (@{a,b}@) is not a wildcard
-- here: it is expanded by 'expandBraces' before this predicate is consulted,
-- so a brace entry resolves to explicit paths rather than being matched as a
-- single literal pattern.
hasGlobChars :: Text -> Bool
hasGlobChars e = any (`elem` toString e) ['*', '?', '[']

-- | Lexically expand one level of brace alternation in a workspace
-- member/exclude entry: @"crates/{a,b}"@ -> @["crates/a", "crates/b"]@.
-- Entries without braces are returned unchanged, and an entry whose braces are
-- unbalanced or nested (so it cannot be expanded) is returned as the single
-- original string, where it is then treated as an explicit path -- which lets
-- its resolution failure surface as a warning rather than being dropped
-- silently.
expandBraces :: Text -> [Text]
expandBraces e =
  let (pre, rest) = Text.span (/= '{') e
   in if Text.null rest
        then [e]
        else
          let (content, suf) = Text.span (/= '}') (Text.drop 1 rest)
           in if Text.null suf || Text.elem '{' content
                then [e]
                else map (\alt -> pre <> alt <> Text.drop 1 suf) (Text.splitOn "," content)

-- | The relative path of 'dir' below 'root', as a slash-separated string
-- without the trailing slash a Dir carries (a FilePattern '*' would not match
-- it; stripping it lets "crates/*" match "crates/alpha" exactly as Cargo
-- does), or Nothing if 'dir' is not properly contained in 'root' -- in
-- particular, for 'root' itself. The result always uses forward slashes: a
-- Dir's 'toFilePath' carries OS-native separators (backslashes on Windows),
-- but Cargo member/exclude entries use forward slashes and 'globMatches'
-- matches forward-slash paths only, so backslashes are normalised away.
relPathStr :: Path Abs Dir -> Path Abs Dir -> Maybe Text
relPathStr root dir =
  case stripProperPrefix root dir of
    Nothing -> Nothing
    Just d ->
      let rel = fromMaybe (toText d) (Text.stripSuffix "/" (toText d))
       in Just (Text.replace "\\" "/" rel)

-- | Recursively collect every directory under @root@ that contains a
-- @Cargo.toml@, skipping @target/@, hidden directories, and @node_modules/@,
-- plus any directory whose path relative to @root@ matches one of the
-- workspace's @exclude@ entries. Excluded directories are not searched, hence
-- not discovered; membership itself never consults them (house policy -- see
-- 'enumerateWorkspaceMembers').
--
-- Implemented with 'Discovery.Walk.walk'': the handler collects a directory
-- when it holds a @Cargo.toml@ and prunes @target@/@node_modules@/hidden/excluded
-- subdirectories via 'WalkStep.WalkSkipSome'. As @walk'@ visits the root before
-- descending and prunes only subdirectories, this matches a plain recursive
-- descent that always enters the root. The pre-order traversal of @walk'@
-- (parent before children) reproduces the previous list ordering.
findManifestDirs ::
  (Has ReadFS sig m, Has Diagnostics sig m) =>
  Path Abs Dir ->
  [Text] ->
  m [Path Abs Dir]
findManifestDirs rootDir excludes = walk' visit rootDir
  where
    visit dir subdirs files =
      let hasToml = any ((== "Cargo.toml") . toString . filename) files
          -- 'WalkSkipSome' with an empty list is equivalent to 'WalkContinue',
          -- so this always prunes the unwanted subdirs when there are any.
          prune = map (toText . toFilePath . dirname) $ filter (\sd -> not (shouldDescendDir sd) || isExcluded excludes sd) subdirs
       in pure (if hasToml then [dir] else [], WalkSkipSome prune)

    -- A subdir matching a workspace @exclude@ entry is pruned from the search.
    isExcluded :: [Text] -> Path Abs Dir -> Bool
    isExcluded excludes' sd =
      case relPathStr rootDir sd of
        Nothing -> False
        Just relStr -> any (globMatches relStr) excludes'

    shouldDescendDir :: Path Abs Dir -> Bool
    shouldDescendDir d =
      -- 'dirname' yields a Dir whose string form carries the trailing
      -- separator (e.g. "target/"); drop it so the literal comparisons below
      -- match "target" and "node_modules" as the haddock above promises.
      let name = dropWhileEnd (== '/') (toString (dirname d))
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
    -- Unresolvable references are omitted from the graph by
    -- 'lockfileToMetadata'; surface them so a drifted/corrupt lockfile is
    -- visible rather than silently producing fewer edges.
    unless (null unresolvable) $ warn warning
    let graph = buildGraph emitGitBackedLocators (lockfileToMetadata lock members analysis)
    pure (graph, Complete)
  where
    unresolvable = unresolvableDeps lock
    warning :: Text
    warning =
      "Cargo.lock for project at "
        <> toText (toFilePath manifestDir)
        <> " references package(s) that match no [[package]] entry; omitted from the dependency graph: "
        <> Text.intercalate ", " unresolvable

-- | Names of @Cargo.lock@ "dependencies" entries that match no [[package]]
-- entry (neither by name and version nor by name alone). Unresolvable
-- references are omitted from the graph built by 'lockfileToMetadata' rather
-- than rendered as a phantom vertex with an empty source; this exposes their
-- names so the monadic callers can warn. Deduplicated, in first-occurrence
-- order.
unresolvableDeps :: CargoLock -> [Text]
unresolvableDeps lock =
  let packages = lockPackages lock
      nameVers = Set.fromList (map (\p -> (packageName p, fromMaybe "*" (packageVersion p))) packages)
      names = Set.fromList (map packageName packages)
      resolves dep = Set.member (depName dep, fromMaybe "*" (depVersion dep)) nameVers || Set.member (depName dep) names
   in nub (map depName (concatMap (filter (not . resolves) . packageDependencies) packages))

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
-- inherit the child's propagated kind set (Production when the child is
-- unreachable from any member) -- the documented, accepted gap (see the
-- module header of "Strategy.Cargo.Lock"). Edges whose dependency reference
-- matches no [[package]] entry in the lockfile are omitted entirely rather
-- than pointing at a phantom vertex; their names surface via 'unresolvableDeps'.
lockfileToMetadata :: CargoLock -> [WorkspaceMember] -> ManifestAnalysis -> CargoMetadata
lockfileToMetadata lock members analysis =
  CargoMetadata
    { metadataPackages = packages
    , metadataWorkspaceMembers = memberIds
    , metadataResolve = Resolve resolveNodes
    }
  where
    memberDirs = Map.fromList (map (\m -> (memberName m, memberDir m)) members)

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

    labeled = map (\p -> (p, pkgIdentity p)) (lockPackages lock)

    -- (name, version) -> PackageId, for resolving "name version" dep strings.
    pidByNameVer =
      Map.fromList
        (map (\(p, (pid, _)) -> ((packageName p, fromMaybe "*" (packageVersion p)), pid)) labeled)

    -- name -> [PackageId], a fallback for references that don't pin a version.
    pidByName = Map.fromListWith (++) (map (\(p, (pid, _)) -> (packageName p, [pid])) labeled)

    packages =
      map
        (\(_, (pid, srcUrl)) -> Package (pkgIdName pid) (pkgIdVersion pid) pid Nothing Nothing [] srcUrl)
        labeled

    memberIds = map (fst . snd) (filter (isMemberPath . fst) labeled)

    -- Adjacency: each lockfile package id -> the package ids it depends on,
    -- restricted to references that resolve to a real [[package]] entry.
    childMap =
      Map.fromListWith (++) $
        map
          (\(p, (pid, _)) -> (pid, mapMaybe resolveTarget (packageDependencies p)))
          labeled

    -- Seed the propagation: each member (root) edge pushes its declared kinds
    -- onto its child. A package is reachable from a root under a given kind
    -- iff some member declares it under that kind.
    seedEdges = do
      (p, _) <- labeled
      guard (isMemberPath p)
      dep <- packageDependencies p
      child <- maybeToList (resolveTarget dep)
      pure (child, nodeDepKindsSet p dep)

    -- Per-package propagation of dependency kinds over the lockfile graph:
    -- every package reachable from a member edge acquires that edge's kind
    -- set, so a package's set is the union of the kind sets under which it
    -- is reachable. Each seed edge runs one plain worklist reachability walk
    -- (same idiom as "Strategy.Cargo.MetadataCmd"; the seen set guards
    -- against the cyclic lockfiles cargo permits via dev-dependency
    -- cycles). A package left with no set is unreachable from any member
    -- (stale lockfile residue) and is relabelled Production by
    -- 'nodeDepFor'.
    propagated =
      Map.unionsWith Set.union $
        map (\(child, kinds) -> Map.fromSet (const kinds) (reachFrom child)) seedEdges

    reachFrom :: PackageId -> Set.Set PackageId
    reachFrom root = go Set.empty [root]
      where
        go seen [] = seen
        go seen (pid : todo)
          | Set.member pid seen = go seen todo
          | otherwise = go (Set.insert pid seen) (Map.findWithDefault [] pid childMap ++ todo)

    resolveNodes =
      map
        (\(p, (pid, _)) -> ResolveNode pid (mapMaybe (nodeDepFor p) (packageDependencies p)))
        labeled

    -- 'Nothing' for a reference matching no [[package]] entry: it is omitted
    -- from the graph rather than rendered as a phantom vertex with an empty
    -- source; its name surfaces via 'unresolvableDeps'.
    --
    -- Edge kinds: a member edge keeps its declared kinds; a non-member edge
    -- inherits the child's propagated kind set (Production when the child is
    -- unreachable from any member).
    nodeDepFor p dep = fmap (\target -> NodeDependency target (kindsFor p target dep)) (resolveTarget dep)

    kindsFor p target dep
      | isMemberPath p = map depKindToNdk (Set.toList (nodeDepKindsSet p dep))
      | otherwise =
          case Map.lookup target propagated of
            Just set | not (Set.null set) -> map depKindToNdk (Set.toList set)
            _ -> [prodNdk]

    resolveTarget :: CargoDependency -> Maybe PackageId
    resolveTarget dep =
      case Map.lookup (depName dep, fromMaybe "*" (depVersion dep)) pidByNameVer of
        Just pid -> Just pid
        Nothing ->
          case Map.lookup (depName dep) pidByName of
            Just (pid : _) -> Just pid
            _ -> Nothing

    -- Declared dependency kinds for a member edge, as a set of 'DepKind's.
    -- Empty for a non-member parent (its edges inherit the child's propagated
    -- set instead). The member path is unchanged from the historical logic: a
    -- per-version match with a union fallback.
    nodeDepKindsSet p dep =
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
              Just kinds -> kinds
              Nothing -> Set.singleton DepProd
       in if isMemberPath p
            then
              let perVersionKinds =
                    Set.fromList $ do
                      (req, knd) <- fromMaybe [] (Map.lookup (parentName, depName dep) (depKindReqs analysis))
                      guard (reqSatisfies req resolvedVer)
                      pure knd
               in if Set.null perVersionKinds
                    then unionKinds
                    else perVersionKinds
            else Set.empty

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
