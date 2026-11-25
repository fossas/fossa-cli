{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Tests for the Cargo.lock -> CargoMetadata conversion pipeline.
--
-- Covers:
--   * workspace member enumeration (single crate, virtual workspace,
--     members + default-members);
--   * per-member dependency-kind derivation (prod / dev / build, renames,
--     workspace inheritance, target tables, non-member path deps);
--   * the pure lockfile-vs-metadata source decision;
--   * golden lockfile -> CargoMetadata conversions (2-member workspace with a
--     prod+build member path dep, a non-member build-dep subtree, git deps
--     tagged/untagged, name-version disambiguation with per-version dep-kind
--     labeling, union fallback for drifted or versionless declarations);
--   * propagation of dependency kinds through the lockfile graph (dev/build
--     transitive chains, mixed prod+dev reachability, orphan packages).
module Cargo.LockfileSpec (
  spec,
) where

import Control.Effect.Diagnostics (errorBoundary)
import Control.Effect.Lift (sendIO)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes
import Diag.Result (Result (..), renderSuccess)
import Effect.Logger (renderIt)
import GraphUtil
import Graphing qualified
import Path (mkRelDir, toFilePath, (</>))
import Path.IO (makeAbsolute)
import Strategy.Cargo
import Strategy.Cargo.Lock (reqSatisfies)
import Test.Effect (expectationFailure', it', shouldBe', shouldContain', shouldNotContain', shouldSatisfy')
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Test.Hspec qualified as Test

-- | A registry source string, matching real 'cargo metadata' output.
registrySource :: Text
registrySource = "registry+https://github.com/rust-lang/crates.io-index"

nullKind :: NodeDepKind
nullKind = NodeDepKind Nothing Nothing

devKind :: Text -> NodeDepKind
devKind k = NodeDepKind (Just k) Nothing

mkPkgId :: Text -> Text -> PackageId
mkPkgId name ver = PackageId name ver registrySource

mkDep :: Text -> Text -> DepType -> [DepEnvironment] -> Dependency
mkDep name version depType envs = Dependency depType name (Just $ CEq version) [] (Set.fromList envs) Map.empty

spec :: Spec
spec = do
  memberEnumerationSpecs
  kindAnalysisSpecs
  sourceDecisionSpecs
  reqSatisfiesSpecs
  unresolvableDepsSpecs
  sourceSuffixSpecs
  goldenConversionSpecs
  propagationSpecs

-- ===========================================================================
-- Workspace member enumeration

memberEnumerationSpecs :: Spec
memberEnumerationSpecs =
  describe "workspace member enumeration" $ do
    it' "a single crate (no [workspace]) is its own sole member" $ do
      root <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/single_crate")
      ms <- enumerateWorkspaceMembers root
      ms `shouldBe'` [WorkspaceMember "solo" root]

    it' "a virtual workspace expands members = [\"crates/*\"]" $ do
      root <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/virtual_ws")
      ms <- enumerateWorkspaceMembers root
      let alphaDir = root </> $(mkRelDir "crates/alpha")
          betaDir = root </> $(mkRelDir "crates/beta")
      ms `shouldBe'` [WorkspaceMember "alpha" alphaDir, WorkspaceMember "beta" betaDir]

    -- Cargo requires default-members to be a subset of members; we take the
    -- union of both lists, so this also verifies that parsing a
    -- default-members key does not disturb member enumeration.
    it' "a workspace with members and default-members enumerates all members" $ do
      root <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/default_ws")
      ms <- enumerateWorkspaceMembers root
      let alphaDir = root </> $(mkRelDir "crates/alpha")
          betaDir = root </> $(mkRelDir "crates/beta")
          gammaDir = root </> $(mkRelDir "crates/gamma")
      ms
        `shouldBe'` [ WorkspaceMember "alpha" alphaDir
                    , WorkspaceMember "beta" betaDir
                    , WorkspaceMember "gamma" gammaDir
                    ]

    -- The root package is always a member of a non-virtual workspace; a
    -- members glob that also matches the root (".") must not add it twice.
    it' "a workspace whose members glob matches the root does not duplicate the root member" $ do
      root <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/rootmember_ws")
      ms <- enumerateWorkspaceMembers root
      ms `shouldBe'` [WorkspaceMember "rootee" root]

    -- An exclude entry prunes the manifest search, so the excluded directory
    -- is never discovered (even though its manifest exists and a member glob
    -- would match it); the non-excluded sibling is found normally.
    it' "a workspace exclude entry prunes the manifest search" $ do
      root <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/exclude_ws")
      ms <- enumerateWorkspaceMembers root
      let appDir = root </> $(mkRelDir "crates/app")
      -- "crates/legacy" has a valid manifest but is excluded from the search,
      -- so it must not appear as a member.
      ms `shouldBe'` [WorkspaceMember "app" appDir]

    -- House policy: an explicitly listed member (no glob) is always included,
    -- even when the same path is also named in exclude.
    it' "an explicitly listed member beats an exclude entry naming the same path" $ do
      root <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/excluded_member_ws")
      ms <- enumerateWorkspaceMembers root
      let legacyDir = root </> $(mkRelDir "crates/legacy")
      ms `shouldBe'` [WorkspaceMember "legacy" legacyDir]

    -- The manifest walk never leaves the workspace root, so an out-of-tree
    -- member ("../sibling") is resolved directly from the root instead.
    it' "an out-of-tree member entry is resolved from the workspace root" $ do
      root <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/out_of_tree/ws")
      siblingRoot <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/out_of_tree/sibling")
      ms <- enumerateWorkspaceMembers root
      ms `shouldBe'` [WorkspaceMember "sibling" siblingRoot]

    it' "a '**' member glob matches nested directories" $ do
      root <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/starstar_ws")
      ms <- enumerateWorkspaceMembers root
      let alphaDir = root </> $(mkRelDir "crates/alpha")
          betaDir = root </> $(mkRelDir "crates/nested/beta")
      ms `shouldBe'` [WorkspaceMember "alpha" alphaDir, WorkspaceMember "beta" betaDir]

    -- 'shouldDescendDir' must prune "target" (after dropping the trailing
    -- separator 'dirname' carries), so a stray vendored crate with a
    -- [package] table under target/ is never discovered -- even though the
    -- '**' member glob would otherwise match it.
    it' "the manifest walk does not descend into a target/ directory" $ do
      root <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/target_skip_ws")
      ms <- enumerateWorkspaceMembers root
      let appDir = root </> $(mkRelDir "crates/app")
      -- "target/Cargo.toml" names "vendored_stray"; pruning keeps it out.
      ms `shouldBe'` [WorkspaceMember "app" appDir]
      -- The stray crate is also absent from the graph built from a lockfile:
      -- the only dependency is the registry crate the member declares.
      analysis <- analyzeManifests root ms
      case parseCargoLock targetSkipLock of
        Left err -> expectationFailure' ("parseCargoLock failed: " ++ show err)
        Right lock -> do
          let meta = lockfileToMetadata lock ms analysis
              graph = buildGraph False meta
              serdeDep = mkDep "serde" "1.0.100" CargoType [EnvProduction]
          expectDeps' [serdeDep] graph
          map dependencyName (Graphing.directList graph) `shouldNotContain'` ["vendored_stray"]

    -- 'expandBraces' expands "crates/{a,b}" one level, so both crates are
    -- members; the explicit entry naming a nonexistent directory ("ghost_dir")
    -- resolves to nothing and must surface as a 'warn' rather than being
    -- dropped silently.
    it' "a brace member entry expands, and an unresolvable entry warns" $ do
      root <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/brace_ws")
      result <- errorBoundary $ enumerateWorkspaceMembers root
      case result of
        Failure _ _ -> expectationFailure' "expected member enumeration to succeed with a warning"
        Success ws ms -> do
          let aDir = root </> $(mkRelDir "crates/a")
              bDir = root </> $(mkRelDir "crates/b")
          ms `shouldBe'` [WorkspaceMember "a" aDir, WorkspaceMember "b" bDir]
          case renderSuccess ws mempty of
            Nothing -> expectationFailure' "expected an unresolvable-member warning"
            Just rendered ->
              renderIt rendered
                `shouldSatisfy'` Text.isInfixOf "member entry 'ghost_dir' did not resolve to a manifest with a [package] table"

-- ===========================================================================
-- Per-member dependency-kind derivation

kindAnalysisSpecs :: Spec
kindAnalysisSpecs =
  describe "manifest dependency-kind analysis" $ do
    it' "derives prod/dev/build kinds, renames, workspace inheritance, target tables, and path deps" $ do
      root <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/kind_ws")
      ms <- enumerateWorkspaceMembers root
      analysis <- analyzeManifests root ms
      let km = depKindMap analysis
          pd = manifestPathDeps analysis
          myLocalExpected = root </> $(mkRelDir "crates/libs/my_local")
          kinds k = Map.lookup k km
      kinds ("app", "serde") `shouldBe'` Just (Set.singleton DepProd)
      kinds ("app", "dual") `shouldBe'` Just (Set.fromList [DepProd, DepBuild, DepDev])
      kinds ("app", "cc") `shouldBe'` Just (Set.singleton DepBuild)
      kinds ("app", "approx") `shouldBe'` Just (Set.singleton DepDev)
      -- a dep under [target.'cfg(unix)'.dev-dependencies].
      kinds ("app", "unixtool") `shouldBe'` Just (Set.singleton DepDev)
      kinds ("app", "shared") `shouldBe'` Just (Set.singleton DepProd)
      kinds ("app", "mystery") `shouldBe'` Just (Set.singleton DepProd)
      kinds ("app", "foo") `shouldBe'` Just (Set.singleton DepProd)
      -- a non-member path dep is recorded with a resolved absolute directory.
      kinds ("app", "my_local") `shouldBe'` Just (Set.singleton DepProd)
      Map.lookup "my_local" pd `shouldBe'` Just myLocalExpected
      manifestWarnings analysis `shouldContain'` ["Cargo manifest dependency 'mystery' uses workspace inheritance but no matching [workspace.dependencies] entry was found; classified as Production."]

-- ===========================================================================
-- Pure lockfile-vs-metadata source decision

sourceDecisionSpecs :: Spec
sourceDecisionSpecs =
  describe "decideLockfileSource" $ do
    let dummyLock = CargoLock 3 [] []
    it "uses the lockfile when it parses" $
      decideLockfileSource (Right dummyLock) `Test.shouldBe` UseLockfile
    it "falls back to metadata when the lockfile fails to parse" $
      decideLockfileSource (Left (TomlParseError "boom")) `Test.shouldBe` UseMetadata
    it "falls back to metadata on an unsupported version" $
      decideLockfileSource (Left (UnsupportedVersion 1)) `Test.shouldBe` UseMetadata

-- | A lockfile for the 'target_skip_ws' workspace: the member's only
-- dependency is a registry crate; nothing in "target/" appears.
targetSkipLock :: Text
targetSkipLock =
  "version = 3\n"
    <> "\n[[package]]\nname = \"app\"\nversion = \"0.1.0\"\ndependencies = [\"serde 1.0.100\"]\n"
    <> "\n[[package]]\nname = \"serde\"\nversion = \"1.0.100\"\nsource = \"registry+https://github.com/rust-lang/crates.io-index\"\nchecksum = \"abcd\"\n"

-- | A lockfile whose member references "ghost", a crate with no [[package]]
-- entry in the lockfile.
phantomDepLock :: Text
phantomDepLock =
  "version = 3\n"
    <> "\n[[package]]\nname = \"solo\"\nversion = \"0.1.0\"\ndependencies = [\"serde 1.0.100\", \"ghost 9.9.9\"]\n"
    <> "\n[[package]]\nname = \"serde\"\nversion = \"1.0.100\"\nsource = \"registry+https://github.com/rust-lang/crates.io-index\"\nchecksum = \"abcd\"\n"

-- ===========================================================================
-- Cargo version-requirement matching (reqSatisfies)

reqSatisfiesSpecs :: Spec
reqSatisfiesSpecs =
  describe "reqSatisfies" $ do
    it "a bare (caret) requirement uses Cargo caret semantics" $ do
      reqSatisfies "0.12" "0.12.1" `shouldBe` True
      reqSatisfies "0.12" "0.13.0" `shouldBe` False
      -- Cargo's ^0.0.5 is an exact match (=0.0.5), not a range up to 0.0.6.
      reqSatisfies "0.0.5" "0.0.5" `shouldBe` True
      reqSatisfies "0.0.5" "0.0.6" `shouldBe` False

    it "a tilde requirement allows patch updates only" $ do
      reqSatisfies "~1.2" "1.2.9" `shouldBe` True
      reqSatisfies "~1.2" "1.3.0" `shouldBe` False

    it "= is an exact match with missing components zero-padded" $ do
      reqSatisfies "=1.2" "1.2.0" `shouldBe` True
      reqSatisfies "=1.2" "1.2.1" `shouldBe` False

    it ">= and < compare against the zero-padded requirement version" $ do
      reqSatisfies ">=1.2" "1.9.9" `shouldBe` True
      reqSatisfies "<2" "1.9.9" `shouldBe` True
      reqSatisfies "<2" "2.0.0" `shouldBe` False

    it "a comma-joined requirement is an AND of its parts" $ do
      reqSatisfies "1.2, <1.2.5" "1.2.3" `shouldBe` True
      reqSatisfies "1.2, <1.2.5" "1.2.9" `shouldBe` False

    it "an unparseable requirement or resolved version is a non-match" $ do
      reqSatisfies "bogus" "1.0.0" `shouldBe` False
      -- One bad part of an AND list rejects the whole requirement.
      reqSatisfies "1.2, nonsense" "1.2.3" `shouldBe` False
      reqSatisfies "1.0" "not-a-version" `shouldBe` False

    it "a requirement without a prerelease never matches a prerelease resolution" $ do
      reqSatisfies "=1.2.3" "1.2.3-beta" `shouldBe` False
      -- Requirement-side prereleases are not parsed, so the conservative
      -- fallback (False) applies here as well.
      reqSatisfies "1.2.3-beta" "1.2.3-beta" `shouldBe` False

-- ===========================================================================
-- Unresolvable dependency references (phantom vertices)

unresolvableDepsSpecs :: Spec
unresolvableDepsSpecs =
  describe "unresolvable lockfile references" $ do
    it "a member dep on a crate absent from the lock is reported, others are not" $ do
      let lockText =
            "version = 3\n"
              <> "\n[[package]]\nname = \"solo\"\nversion = \"0.1.0\"\ndependencies = [\"present 1.0.0\", \"missing_crate 2.5.0\"]\n"
              <> "\n[[package]]\nname = \"present\"\nversion = \"1.0.0\"\nsource = \"registry+https://github.com/rust-lang/crates.io-index\"\nchecksum = \"cc\"\n"
      case parseCargoLock lockText of
        Left err -> expectationFailure ("parseCargoLock failed: " ++ show err)
        Right lock -> unresolvableDeps lock `shouldBe` ["missing_crate"]
    it' "a dep matching no [[package]] entry is omitted, not turned into a phantom vertex" $ do
      root <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/single_crate")
      members <- enumerateWorkspaceMembers root
      analysis <- analyzeManifests root members
      case parseCargoLock phantomDepLock of
        Left err -> expectationFailure' ("parseCargoLock failed: " ++ show err)
        Right lock -> do
          unresolvableDeps lock `shouldBe'` ["ghost"]
          let soloPid = PackageId "solo" "0.1.0" ("path+file://" <> toText (toFilePath root))
              serdePid = mkPkgId "serde" "1.0.100"
              meta = lockfileToMetadata lock members analysis
          -- The rest of the metadata is well-formed: the resolvable edge and
          -- every locked package are present unchanged.
          meta
            `shouldBe'` CargoMetadata
              [ Package "solo" "0.1.0" soloPid Nothing Nothing [] Nothing
              , Package "serde" "1.0.100" serdePid Nothing Nothing [] (Just registrySource)
              ]
              [soloPid]
              ( Resolve
                  [ ResolveNode soloPid [NodeDependency serdePid [nullKind]]
                  , ResolveNode serdePid []
                  ]
              )
          -- ...and no node references a PackageId with an empty source.
          let allDeps = concatMap resolveNodeDeps (resolvedNodes $ metadataResolve meta)
          filter ((== "") . pkgIdSource . nodePkg) allDeps `shouldBe'` []

-- ===========================================================================
-- Source-suffixed dependency entries

-- | A lockfile whose member references a git package via the parenthesized
-- source form Cargo uses to disambiguate same name+version from different
-- sources.
sourceSuffixLock :: Text
sourceSuffixLock =
  "version = 3\n"
    <> "\n[[package]]\nname = \"app\"\nversion = \"0.1.0\"\ndependencies = [\"foo 1.2.3 (git+https://example.com/repo#abc1234)\"]\n"
    <> "\n[[package]]\nname = \"foo\"\nversion = \"1.2.3\"\nsource = \"git+https://example.com/repo#abc1234\"\n"

sourceSuffixSpecs :: Spec
sourceSuffixSpecs =
  describe "source-suffixed dependencies entries" $ do
    it "strips a trailing parenthesized source suffix before splitting" $ do
      parseDependencyString "foo 1.2.3 (git+https://example.com/repo#abc1234)"
        `shouldBe` CargoDependency "foo" (Just "1.2.3")

    it' "a source-suffixed entry resolves to its [[package]] without omission or phantom" $ do
      root <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/source_suffix")
      members <- enumerateWorkspaceMembers root
      analysis <- analyzeManifests root members
      case parseCargoLock sourceSuffixLock of
        Left err -> expectationFailure' ("parseCargoLock failed: " ++ show err)
        Right lock -> do
          unresolvableDeps lock `shouldBe'` []
          let appPid = PackageId "app" "0.1.0" ("path+file://" <> toText (toFilePath root))
              -- The PackageId drops the #commit fragment, matching the git-
              -- source handling in 'lockfileToMetadata'.
              fooPid = PackageId "foo" "1.2.3" "git+https://example.com/repo"
              meta = lockfileToMetadata lock members analysis
              expected =
                CargoMetadata
                  [ Package "app" "0.1.0" appPid Nothing Nothing [] Nothing
                  , Package "foo" "1.2.3" fooPid Nothing Nothing [] (Just "git+https://example.com/repo#abc1234")
                  ]
                  [appPid]
                  (Resolve [ResolveNode appPid [NodeDependency fooPid [nullKind]], ResolveNode fooPid []])
          meta `shouldBe'` expected

-- ===========================================================================
-- Golden lockfile -> CargoMetadata conversions

goldenWsLock :: Text
goldenWsLock =
  "version = 3\n"
    <> "\n[[package]]\nname = \"app\"\nversion = \"0.1.0\"\ndependencies = [\"mylib\", \"dual 1.0.0\", \"serde 1.0.100\"]\n"
    <> "\n[[package]]\nname = \"mylib\"\nversion = \"0.1.0\"\ndependencies = [\"serde 1.0.100\"]\n"
    <> "\n[[package]]\nname = \"dual\"\nversion = \"1.0.0\"\nsource = \"registry+https://github.com/rust-lang/crates.io-index\"\nchecksum = \"dd\"\n"
    <> "\n[[package]]\nname = \"serde\"\nversion = \"1.0.100\"\nsource = \"registry+https://github.com/rust-lang/crates.io-index\"\nchecksum = \"abcd\"\n"

rootpkgWsLock :: Text
rootpkgWsLock =
  "version = 3\n"
    <> "\n[[package]]\nname = \"alpha\"\nversion = \"0.1.0\"\ndependencies = [\"shared_reg 1.0.0\"]\n"
    <> "\n[[package]]\nname = \"buildd\"\nversion = \"1.0.0\"\nsource = \"registry+https://github.com/rust-lang/crates.io-index\"\nchecksum = \"bb\"\n"
    <> "\n[[package]]\nname = \"devdep\"\nversion = \"1.0.0\"\nsource = \"registry+https://github.com/rust-lang/crates.io-index\"\nchecksum = \"dd\"\n"
    <> "\n[[package]]\nname = \"rootpkg\"\nversion = \"0.1.0\"\ndependencies = [\"alpha\", \"buildd 1.0.0\", \"devdep 1.0.0\", \"shared_reg 1.0.0\"]\n"
    <> "\n[[package]]\nname = \"shared_reg\"\nversion = \"1.0.0\"\nsource = \"registry+https://github.com/rust-lang/crates.io-index\"\nchecksum = \"ss\"\n"

gitWsLock :: Text
gitWsLock =
  "version = 3\n"
    <> "\n[[package]]\nname = \"app\"\nversion = \"0.1.0\"\ndependencies = [\"tagged\", \"untagged\"]\n"
    <> "\n[[package]]\nname = \"tagged\"\nversion = \"1.2.3\"\nsource = \"git+https://github.com/x/tagged?tag=v1.2.3#feedface\"\n"
    <> "\n[[package]]\nname = \"untagged\"\nversion = \"0.5.0\"\nsource = \"git+https://github.com/x/untagged?rev=deadbeef#0abc123def456\"\n"

nonmemberWsLock :: Text
nonmemberWsLock =
  "version = 3\n"
    <> "\n[[package]]\nname = \"app\"\nversion = \"0.1.0\"\ndependencies = [\"ext\"]\n"
    <> "\n[[package]]\nname = \"ext\"\nversion = \"1.0.0\"\nsource = \"git+https://github.com/x/ext?tag=v1.0#abc123\"\ndependencies = [\"ext_build 0.2.0\"]\n"
    <> "\n[[package]]\nname = \"ext_build\"\nversion = \"0.2.0\"\nsource = \"registry+https://github.com/rust-lang/crates.io-index\"\nchecksum = \"zz\"\n"

disambigLock :: Text
disambigLock =
  "version = 3\n"
    <> "\n[[package]]\nname = \"disambig\"\nversion = \"0.1.0\"\ndependencies = [\"ansi_term 0.12.1\", \"ansi_term 0.11.0\"]\n"
    <> "\n[[package]]\nname = \"ansi_term\"\nversion = \"0.11.0\"\nsource = \"registry+https://github.com/rust-lang/crates.io-index\"\nchecksum = \"aa\"\n"
    <> "\n[[package]]\nname = \"ansi_term\"\nversion = \"0.12.1\"\nsource = \"registry+https://github.com/rust-lang/crates.io-index\"\nchecksum = \"bb\"\n"

compatUnifyLock :: Text
compatUnifyLock =
  "version = 3\n"
    <> "\n[[package]]\nname = \"compat_unify\"\nversion = \"0.1.0\"\ndependencies = [\"ansi_term 0.12.1\"]\n"
    <> "\n[[package]]\nname = \"ansi_term\"\nversion = \"0.12.1\"\nsource = \"registry+https://github.com/rust-lang/crates.io-index\"\nchecksum = \"cc\"\n"

driftedLock :: Text
driftedLock =
  "version = 3\n"
    <> "\n[[package]]\nname = \"drifted\"\nversion = \"0.1.0\"\ndependencies = [\"ansi_term 0.13.0\"]\n"
    <> "\n[[package]]\nname = \"ansi_term\"\nversion = \"0.13.0\"\nsource = \"registry+https://github.com/rust-lang/crates.io-index\"\nchecksum = \"dd\"\n"

versionlessLock :: Text
versionlessLock =
  "version = 3\n"
    <> "\n[[package]]\nname = \"versionless\"\nversion = \"0.1.0\"\ndependencies = [\"shared 0.1.0\", \"shared 1.2.0\"]\n"
    <> "\n[[package]]\nname = \"shared\"\nversion = \"0.1.0\"\n"
    <> "\n[[package]]\nname = \"shared\"\nversion = \"1.2.0\"\nsource = \"registry+https://github.com/rust-lang/crates.io-index\"\nchecksum = \"ee\"\n"

goldenConversionSpecs :: Spec
goldenConversionSpecs =
  describe "lockfile -> CargoMetadata conversion" $ do
    -- 'buildGraph' applies 'shrinkRoots', which removes the direct
    -- workspace-member roots and promotes their non-member children. Members
    -- therefore never appear in the final graph; the graph assertions below
    -- check the surviving (non-member) dependencies and their environment
    -- labels. The member PackageIds/edges are pinned by the structural
    -- 'CargoMetadata' equality asserted above each graph check.
    it' "2-member workspace: prod+build registry dep gets both environments" $ do
      root <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/golden_ws")
      members <- enumerateWorkspaceMembers root
      analysis <- analyzeManifests root members
      case parseCargoLock goldenWsLock of
        Left err -> expectationFailure' ("parseCargoLock failed: " ++ show err)
        Right lock -> do
          let appDir = root </> $(mkRelDir "crates/app")
              libDir = root </> $(mkRelDir "crates/lib")
              appPid = PackageId "app" "0.1.0" ("path+file://" <> toText (toFilePath appDir))
              libPid = PackageId "mylib" "0.1.0" ("path+file://" <> toText (toFilePath libDir))
          members `shouldBe'` [WorkspaceMember "app" appDir, WorkspaceMember "mylib" libDir]
          let dualPid = mkPkgId "dual" "1.0.0"
              serdePid = mkPkgId "serde" "1.0.100"
              meta = lockfileToMetadata lock members analysis
              expected =
                CargoMetadata
                  [ Package "app" "0.1.0" appPid Nothing Nothing [] Nothing
                  , Package "mylib" "0.1.0" libPid Nothing Nothing [] Nothing
                  , Package "dual" "1.0.0" dualPid Nothing Nothing [] (Just registrySource)
                  , Package "serde" "1.0.100" serdePid Nothing Nothing [] (Just registrySource)
                  ]
                  [appPid, libPid]
                  ( Resolve
                      [ ResolveNode appPid [NodeDependency libPid [nullKind], NodeDependency dualPid [nullKind, devKind "build"], NodeDependency serdePid [nullKind]]
                      , ResolveNode libPid [NodeDependency serdePid [nullKind]]
                      , ResolveNode dualPid []
                      , ResolveNode serdePid []
                      ]
                  )
          meta `shouldBe'` expected

          -- "dual" is declared in both [dependencies] and [build-dependencies]
          -- of the member, so it carries both environments; "serde" (a normal
          -- dep of two members) is Production only. Both survive as
          -- non-member roots.
          let graph = buildGraph False meta
              dualDep = mkDep "dual" "1.0.0" CargoType [EnvProduction, EnvDevelopment]
              serdeDep = mkDep "serde" "1.0.100" CargoType [EnvProduction]
          expectDeps' [dualDep, serdeDep] graph
          expectDirect' [dualDep, serdeDep] graph

    it' "a non-virtual workspace's root package is a member with its own dep kinds" $ do
      -- The root manifest has both a [package] and a [workspace] table:
      -- Cargo always includes the root package in workspace_members, so it
      -- must be enumerated as a member and its dev/build dep edges must be
      -- classified from its manifest.
      root <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/rootpkg_ws")
      members <- enumerateWorkspaceMembers root
      analysis <- analyzeManifests root members
      case parseCargoLock rootpkgWsLock of
        Left err -> expectationFailure' ("parseCargoLock failed: " ++ show err)
        Right lock -> do
          let alphaDir = root </> $(mkRelDir "crates/alpha")
              rootPid = PackageId "rootpkg" "0.1.0" ("path+file://" <> toText (toFilePath root))
              alphaPid = PackageId "alpha" "0.1.0" ("path+file://" <> toText (toFilePath alphaDir))
          members `shouldBe'` [WorkspaceMember "rootpkg" root, WorkspaceMember "alpha" alphaDir]
          let builddPid = mkPkgId "buildd" "1.0.0"
              devdepPid = mkPkgId "devdep" "1.0.0"
              sharedPid = mkPkgId "shared_reg" "1.0.0"
              meta = lockfileToMetadata lock members analysis
          map pkgIdName (metadataWorkspaceMembers meta) `shouldBe'` ["alpha", "rootpkg"]
          let expected =
                CargoMetadata
                  [ Package "alpha" "0.1.0" alphaPid Nothing Nothing [] Nothing
                  , Package "buildd" "1.0.0" builddPid Nothing Nothing [] (Just registrySource)
                  , Package "devdep" "1.0.0" devdepPid Nothing Nothing [] (Just registrySource)
                  , Package "rootpkg" "0.1.0" rootPid Nothing Nothing [] Nothing
                  , Package "shared_reg" "1.0.0" sharedPid Nothing Nothing [] (Just registrySource)
                  ]
                  [alphaPid, rootPid]
                  ( Resolve
                      [ ResolveNode alphaPid [NodeDependency sharedPid [nullKind]]
                      , ResolveNode builddPid []
                      , ResolveNode devdepPid []
                      , ResolveNode rootPid [NodeDependency alphaPid [nullKind], NodeDependency builddPid [devKind "build"], NodeDependency devdepPid [devKind "dev"], NodeDependency sharedPid [nullKind]]
                      , ResolveNode sharedPid []
                      ]
                  )
          meta `shouldBe'` expected

          let graph = buildGraph False meta
              builddDep = mkDep "buildd" "1.0.0" CargoType [EnvDevelopment]
              devdepDep = mkDep "devdep" "1.0.0" CargoType [EnvDevelopment]
              sharedDep = mkDep "shared_reg" "1.0.0" CargoType [EnvProduction]
          expectDeps' [builddDep, devdepDep, sharedDep] graph
          expectDirect' [builddDep, devdepDep, sharedDep] graph
          map dependencyName (Graphing.directList graph) `shouldNotContain'` ["rootpkg"]

    it' "git deps: tagged keeps the crate version, untagged uses the commit hash" $ do
      root <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/git_ws")
      members <- enumerateWorkspaceMembers root
      analysis <- analyzeManifests root members
      case parseCargoLock gitWsLock of
        Left err -> expectationFailure' ("parseCargoLock failed: " ++ show err)
        Right lock -> do
          let appDir = root </> $(mkRelDir "crates/app")
              appPid = PackageId "app" "0.1.0" ("path+file://" <> toText (toFilePath appDir))
          members `shouldBe'` [WorkspaceMember "app" appDir]
          let taggedPid = PackageId "tagged" "1.2.3" "git+https://github.com/x/tagged?tag=v1.2.3"
              untaggedPid = PackageId "untagged" "0.5.0" "git+https://github.com/x/untagged?rev=deadbeef"
              meta = lockfileToMetadata lock members analysis
              expected =
                CargoMetadata
                  [ Package "app" "0.1.0" appPid Nothing Nothing [] Nothing
                  , Package "tagged" "1.2.3" taggedPid Nothing Nothing [] (Just "git+https://github.com/x/tagged?tag=v1.2.3#feedface")
                  , Package "untagged" "0.5.0" untaggedPid Nothing Nothing [] (Just "git+https://github.com/x/untagged?rev=deadbeef#0abc123def456")
                  ]
                  [appPid]
                  ( Resolve
                      [ ResolveNode appPid [NodeDependency taggedPid [nullKind], NodeDependency untaggedPid [nullKind]]
                      , ResolveNode taggedPid []
                      , ResolveNode untaggedPid []
                      ]
                  )
          meta `shouldBe'` expected

          -- The commit-hash version for an untagged git dep (and git-backed
          -- locator names) only surface when 'emitGitBackedLocators' is True,
          -- so this test drives 'buildGraph' with True.
          let graph = buildGraph True meta
              taggedDep = mkDep "github.com/x/tagged#tagged" "1.2.3" CargoType [EnvProduction]
              untaggedDep = mkDep "github.com/x/untagged#untagged" "0abc123def456" CargoType [EnvProduction]
          expectDeps' [taggedDep, untaggedDep] graph
          expectDirect' [taggedDep, untaggedDep] graph

    it' "non-member package build-dep subtree is labeled Production (accepted gap)" $ do
      -- "ext" is a git package, NOT a workspace member. Its edge to
      -- "ext_build" inherits the child's propagated kind set rather than a
      -- kind of its own. "ext_build" is Production here because it is
      -- reachable from the member "app" under a prod dependency, so its
      -- propagated set is {prod}; this pins the accepted gap (see the
      -- Strategy.Cargo.Lock header).
      root <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/nonmember_ws")
      members <- enumerateWorkspaceMembers root
      analysis <- analyzeManifests root members
      case parseCargoLock nonmemberWsLock of
        Left err -> expectationFailure' ("parseCargoLock failed: " ++ show err)
        Right lock -> do
          let appDir = root </> $(mkRelDir "crates/app")
              appPid = PackageId "app" "0.1.0" ("path+file://" <> toText (toFilePath appDir))
          members `shouldBe'` [WorkspaceMember "app" appDir]
          let extPid = PackageId "ext" "1.0.0" "git+https://github.com/x/ext?tag=v1.0"
              extBuildPid = mkPkgId "ext_build" "0.2.0"
              meta = lockfileToMetadata lock members analysis
              expected =
                CargoMetadata
                  [ Package "app" "0.1.0" appPid Nothing Nothing [] Nothing
                  , Package "ext" "1.0.0" extPid Nothing Nothing [] (Just "git+https://github.com/x/ext?tag=v1.0#abc123")
                  , Package "ext_build" "0.2.0" extBuildPid Nothing Nothing [] (Just registrySource)
                  ]
                  [appPid]
                  ( Resolve
                      [ ResolveNode appPid [NodeDependency extPid [nullKind]]
                      , ResolveNode extPid [NodeDependency extBuildPid [nullKind]]
                      , ResolveNode extBuildPid []
                      ]
                  )
          meta `shouldBe'` expected

          let graph = buildGraph False meta
              extDep = mkDep "ext" "1.0.0" CargoType [EnvProduction]
              extBuildDep = mkDep "ext_build" "0.2.0" CargoType [EnvProduction]
          expectDeps' [extDep, extBuildDep] graph
          expectDirect' [extDep] graph

    it' "name-version disambiguation: two ansi_term versions stay distinct" $ do
      -- "ansi_term 0.12.1" (a normal dep) and "ansi_term 0.11.0" (a
      -- build-dep) must resolve to two distinct PackageIds, producing two
      -- separate CargoType vertices that share a name but differ in version.
      root <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/disambig")
      members <- enumerateWorkspaceMembers root
      analysis <- analyzeManifests root members
      case parseCargoLock disambigLock of
        Left err -> expectationFailure' ("parseCargoLock failed: " ++ show err)
        Right lock -> do
          members `shouldBe'` [WorkspaceMember "disambig" root]
          let dPid = PackageId "disambig" "0.1.0" ("path+file://" <> toText (toFilePath root))
              at11 = mkPkgId "ansi_term" "0.11.0"
              at12 = mkPkgId "ansi_term" "0.12.1"
              meta = lockfileToMetadata lock members analysis
              expected =
                CargoMetadata
                  [ Package "disambig" "0.1.0" dPid Nothing Nothing [] Nothing
                  , Package "ansi_term" "0.11.0" at11 Nothing Nothing [] (Just registrySource)
                  , Package "ansi_term" "0.12.1" at12 Nothing Nothing [] (Just registrySource)
                  ]
                  [dPid]
                  ( Resolve
                      [ ResolveNode dPid [NodeDependency at12 [nullKind], NodeDependency at11 [devKind "build"]]
                      , ResolveNode at11 []
                      , ResolveNode at12 []
                      ]
                  )
          meta `shouldBe'` expected

          let graph = buildGraph False meta
              at11Dep = mkDep "ansi_term" "0.11.0" CargoType [EnvDevelopment]
              at12Dep = mkDep "ansi_term" "0.12.1" CargoType [EnvProduction]
          expectDeps' [at11Dep, at12Dep] graph
          expectDirect' [at11Dep, at12Dep] graph

    it' "a dep declared with a compatible version req in two sections keeps both kinds" $ do
      -- The member declares ansi_term "0.12" under both [dependencies] and
      -- [build-dependencies]; the lockfile resolves a single 0.12.1, which
      -- satisfies ^0.12, so that one version carries both kinds.
      root <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/compat_unify")
      members <- enumerateWorkspaceMembers root
      analysis <- analyzeManifests root members
      case parseCargoLock compatUnifyLock of
        Left err -> expectationFailure' ("parseCargoLock failed: " ++ show err)
        Right lock -> do
          members `shouldBe'` [WorkspaceMember "compat_unify" root]
          let cuPid = PackageId "compat_unify" "0.1.0" ("path+file://" <> toText (toFilePath root))
              at12 = mkPkgId "ansi_term" "0.12.1"
              meta = lockfileToMetadata lock members analysis
              expected =
                CargoMetadata
                  [ Package "compat_unify" "0.1.0" cuPid Nothing Nothing [] Nothing
                  , Package "ansi_term" "0.12.1" at12 Nothing Nothing [] (Just registrySource)
                  ]
                  [cuPid]
                  ( Resolve
                      [ ResolveNode cuPid [NodeDependency at12 [nullKind, devKind "build"]]
                      , ResolveNode at12 []
                      ]
                  )
          meta `shouldBe'` expected

          let graph = buildGraph False meta
              at12Dep = mkDep "ansi_term" "0.12.1" CargoType [EnvProduction, EnvDevelopment]
          expectDeps' [at12Dep] graph
          expectDirect' [at12Dep] graph

    it' "a lockfile that drifted past every declared req falls back to the union of kinds" $ do
      -- The member declares ansi_term "0.11" (prod) and "0.12" (build);
      -- the lockfile contains only 0.13.0, which satisfies neither req under
      -- caret semantics. No requirement matches, so the declared-kind union is used.
      root <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/drifted")
      members <- enumerateWorkspaceMembers root
      analysis <- analyzeManifests root members
      case parseCargoLock driftedLock of
        Left err -> expectationFailure' ("parseCargoLock failed: " ++ show err)
        Right lock -> do
          members `shouldBe'` [WorkspaceMember "drifted" root]
          let dPid = PackageId "drifted" "0.1.0" ("path+file://" <> toText (toFilePath root))
              at13 = mkPkgId "ansi_term" "0.13.0"
              meta = lockfileToMetadata lock members analysis
              expected =
                CargoMetadata
                  [ Package "drifted" "0.1.0" dPid Nothing Nothing [] Nothing
                  , Package "ansi_term" "0.13.0" at13 Nothing Nothing [] (Just registrySource)
                  ]
                  [dPid]
                  ( Resolve
                      [ ResolveNode dPid [NodeDependency at13 [nullKind, devKind "build"]]
                      , ResolveNode at13 []
                      ]
                  )
          meta `shouldBe'` expected

          let graph = buildGraph False meta
              at13Dep = mkDep "ansi_term" "0.13.0" CargoType [EnvProduction, EnvDevelopment]
          expectDeps' [at13Dep] graph
          expectDirect' [at13Dep] graph

    it' "a versionless (path) declaration alongside a versioned req gets the union fallback" $ do
      -- The member declares shared as a path dependency under [dependencies]
      -- (no version requirement) and shared "1.0" under [build-dependencies].
      -- The registry copy (1.2.0) matches ^1.0, so it carries only the build
      -- kind; the path copy (0.1.0) satisfies no requirement, so it falls
      -- back to the union of all declared kinds.
      root <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/versionless")
      members <- enumerateWorkspaceMembers root
      analysis <- analyzeManifests root members
      case parseCargoLock versionlessLock of
        Left err -> expectationFailure' ("parseCargoLock failed: " ++ show err)
        Right lock -> do
          members `shouldBe'` [WorkspaceMember "versionless" root]
          let vPid = PackageId "versionless" "0.1.0" ("path+file://" <> toText (toFilePath root))
              sharedDir = root </> $(mkRelDir "libs/shared")
              sharedPathPid = PackageId "shared" "0.1.0" ("file://" <> toText (toFilePath sharedDir))
              sharedRegPid = mkPkgId "shared" "1.2.0"
              meta = lockfileToMetadata lock members analysis
              expected =
                CargoMetadata
                  [ Package "versionless" "0.1.0" vPid Nothing Nothing [] Nothing
                  , Package "shared" "0.1.0" sharedPathPid Nothing Nothing [] Nothing
                  , Package "shared" "1.2.0" sharedRegPid Nothing Nothing [] (Just registrySource)
                  ]
                  [vPid]
                  ( Resolve
                      [ ResolveNode vPid [NodeDependency sharedPathPid [nullKind, devKind "build"], NodeDependency sharedRegPid [devKind "build"]]
                      , ResolveNode sharedPathPid []
                      , ResolveNode sharedRegPid []
                      ]
                  )
          meta `shouldBe'` expected

-- ===========================================================================
-- Propagation of dependency kinds through the lockfile graph

-- | A member's [dev-dependencies] edge reaches "a", which depends on "b".
-- Both edges must carry the dev kind: "a" keeps its declared kind, and "b"
-- inherits the kinds under which "a" was reached.
devChainLock :: Text
devChainLock =
  "version = 3\n"
    <> "\n[[package]]\nname = \"a\"\nversion = \"1.0.0\"\nsource = \"registry+https://github.com/rust-lang/crates.io-index\"\nchecksum = \"aa\"\ndependencies = [\"b 1.0.0\"]\n"
    <> "\n[[package]]\nname = \"app\"\nversion = \"0.1.0\"\ndependencies = [\"a 1.0.0\"]\n"
    <> "\n[[package]]\nname = \"b\"\nversion = \"1.0.0\"\nsource = \"registry+https://github.com/rust-lang/crates.io-index\"\nchecksum = \"bb\"\n"

-- | Two registry packages in a dev-dependency cycle (legal in a Cargo.lock:
-- "a" depends on "b" and "b" depends on "a"), reached from the member
-- "app" via a [dev-dependencies] edge. The propagation walk must terminate,
-- and both packages inherit the dev kind.
cyclicLock :: Text
cyclicLock =
  "version = 3\n"
    <> "\n[[package]]\nname = \"a\"\nversion = \"1.0.0\"\nsource = \"registry+https://github.com/rust-lang/crates.io-index\"\nchecksum = \"aa\"\ndependencies = [\"b 1.0.0\"]\n"
    <> "\n[[package]]\nname = \"app\"\nversion = \"0.1.0\"\ndependencies = [\"a 1.0.0\"]\n"
    <> "\n[[package]]\nname = \"b\"\nversion = \"1.0.0\"\nsource = \"registry+https://github.com/rust-lang/crates.io-index\"\nchecksum = \"bb\"\ndependencies = [\"a 1.0.0\"]\n"

-- | "b" is reachable from the member via a prod edge (through "a") and a dev
-- edge (through "c"), so it must carry both kinds.
mixedReachLock :: Text
mixedReachLock =
  "version = 3\n"
    <> "\n[[package]]\nname = \"a\"\nversion = \"1.0.0\"\nsource = \"registry+https://github.com/rust-lang/crates.io-index\"\nchecksum = \"aa\"\ndependencies = [\"b 1.0.0\"]\n"
    <> "\n[[package]]\nname = \"app\"\nversion = \"0.1.0\"\ndependencies = [\"a 1.0.0\", \"c 1.0.0\"]\n"
    <> "\n[[package]]\nname = \"b\"\nversion = \"1.0.0\"\nsource = \"registry+https://github.com/rust-lang/crates.io-index\"\nchecksum = \"bb\"\n"
    <> "\n[[package]]\nname = \"c\"\nversion = \"1.0.0\"\nsource = \"registry+https://github.com/rust-lang/crates.io-index\"\nchecksum = \"cc\"\ndependencies = [\"b 1.0.0\"]\n"

-- | A member's [build-dependencies] edge reaches "a", which depends on "b".
-- Both edges must carry the build kind.
buildChainLock :: Text
buildChainLock =
  "version = 3\n"
    <> "\n[[package]]\nname = \"a\"\nversion = \"1.0.0\"\nsource = \"registry+https://github.com/rust-lang/crates.io-index\"\nchecksum = \"aa\"\ndependencies = [\"b 1.0.0\"]\n"
    <> "\n[[package]]\nname = \"app\"\nversion = \"0.1.0\"\ndependencies = [\"a 1.0.0\"]\n"
    <> "\n[[package]]\nname = \"b\"\nversion = \"1.0.0\"\nsource = \"registry+https://github.com/rust-lang/crates.io-index\"\nchecksum = \"bb\"\n"

-- | "orphan" is a [[package]] entry no member edge reaches (stale lockfile
-- residue). Its outgoing edge must fall back to the default (Production).
orphanLock :: Text
orphanLock =
  "version = 3\n"
    <> "\n[[package]]\nname = \"app\"\nversion = \"0.1.0\"\n"
    <> "\n[[package]]\nname = \"orphan\"\nversion = \"1.0.0\"\nsource = \"registry+https://github.com/rust-lang/crates.io-index\"\nchecksum = \"oo\"\ndependencies = [\"orphan_child 2.0.0\"]\n"
    <> "\n[[package]]\nname = \"orphan_child\"\nversion = \"2.0.0\"\nsource = \"registry+https://github.com/rust-lang/crates.io-index\"\nchecksum = \"cc\"\n"

propagationSpecs :: Spec
propagationSpecs =
  describe "lockfile dependency-kind propagation" $ do
    it' "a dev-only transitive chain labels the whole subtree dev" $ do
      root <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/dev_chain_ws")
      members <- enumerateWorkspaceMembers root
      analysis <- analyzeManifests root members
      case parseCargoLock devChainLock of
        Left err -> expectationFailure' ("parseCargoLock failed: " ++ show err)
        Right lock -> do
          let appDir = root </> $(mkRelDir "crates/app")
              appPid = PackageId "app" "0.1.0" ("path+file://" <> toText (toFilePath appDir))
          members `shouldBe'` [WorkspaceMember "app" appDir]
          let aPid = mkPkgId "a" "1.0.0"
              bPid = mkPkgId "b" "1.0.0"
              meta = lockfileToMetadata lock members analysis
              expected =
                CargoMetadata
                  [ Package "a" "1.0.0" aPid Nothing Nothing [] (Just registrySource)
                  , Package "app" "0.1.0" appPid Nothing Nothing [] Nothing
                  , Package "b" "1.0.0" bPid Nothing Nothing [] (Just registrySource)
                  ]
                  [appPid]
                  ( Resolve
                      [ ResolveNode aPid [NodeDependency bPid [devKind "dev"]]
                      , ResolveNode appPid [NodeDependency aPid [devKind "dev"]]
                      , ResolveNode bPid []
                      ]
                  )
          meta `shouldBe'` expected

          let graph = buildGraph False meta
              aDep = mkDep "a" "1.0.0" CargoType [EnvDevelopment]
              bDep = mkDep "b" "1.0.0" CargoType [EnvDevelopment]
          expectDeps' [aDep, bDep] graph
          expectDirect' [aDep] graph

    it' "a package reachable via both prod and dev edges carries both kinds" $ do
      root <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/mixed_reach_ws")
      members <- enumerateWorkspaceMembers root
      analysis <- analyzeManifests root members
      case parseCargoLock mixedReachLock of
        Left err -> expectationFailure' ("parseCargoLock failed: " ++ show err)
        Right lock -> do
          let appDir = root </> $(mkRelDir "crates/app")
              appPid = PackageId "app" "0.1.0" ("path+file://" <> toText (toFilePath appDir))
          members `shouldBe'` [WorkspaceMember "app" appDir]
          let aPid = mkPkgId "a" "1.0.0"
              bPid = mkPkgId "b" "1.0.0"
              cPid = mkPkgId "c" "1.0.0"
              meta = lockfileToMetadata lock members analysis
              expected =
                CargoMetadata
                  [ Package "a" "1.0.0" aPid Nothing Nothing [] (Just registrySource)
                  , Package "app" "0.1.0" appPid Nothing Nothing [] Nothing
                  , Package "b" "1.0.0" bPid Nothing Nothing [] (Just registrySource)
                  , Package "c" "1.0.0" cPid Nothing Nothing [] (Just registrySource)
                  ]
                  [appPid]
                  ( Resolve
                      [ -- Both non-member edges into "b" inherit "b"'s full
                        -- propagated set {prod, dev}: "b" is reachable under
                        -- both kinds, whichever parent the edge comes from.
                        ResolveNode aPid [NodeDependency bPid [nullKind, devKind "dev"]]
                      , ResolveNode appPid [NodeDependency aPid [nullKind], NodeDependency cPid [devKind "dev"]]
                      , ResolveNode bPid []
                      , ResolveNode cPid [NodeDependency bPid [nullKind, devKind "dev"]]
                      ]
                  )
          meta `shouldBe'` expected

          let graph = buildGraph False meta
              aDep = mkDep "a" "1.0.0" CargoType [EnvProduction]
              bDep = mkDep "b" "1.0.0" CargoType [EnvProduction, EnvDevelopment]
              cDep = mkDep "c" "1.0.0" CargoType [EnvDevelopment]
          expectDeps' [aDep, bDep, cDep] graph
          expectDirect' [aDep, cDep] graph

    it' "a dev-dependency cycle between non-member packages terminates and labels both dev" $ do
      -- "a" and "b" reference each other in the lockfile (cargo permits this
      -- via dev-dependency cycles). The reachability walk's seen set must
      -- terminate, and both packages inherit the dev kind under which the
      -- member "app" reaches them.
      root <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/cyclic_ws")
      members <- enumerateWorkspaceMembers root
      analysis <- analyzeManifests root members
      case parseCargoLock cyclicLock of
        Left err -> expectationFailure' ("parseCargoLock failed: " ++ show err)
        Right lock -> do
          let appDir = root </> $(mkRelDir "crates/app")
              appPid = PackageId "app" "0.1.0" ("path+file://" <> toText (toFilePath appDir))
          members `shouldBe'` [WorkspaceMember "app" appDir]
          let aPid = mkPkgId "a" "1.0.0"
              bPid = mkPkgId "b" "1.0.0"
              meta = lockfileToMetadata lock members analysis
              expected =
                CargoMetadata
                  [ Package "a" "1.0.0" aPid Nothing Nothing [] (Just registrySource)
                  , Package "app" "0.1.0" appPid Nothing Nothing [] Nothing
                  , Package "b" "1.0.0" bPid Nothing Nothing [] (Just registrySource)
                  ]
                  [appPid]
                  ( Resolve
                      [ ResolveNode aPid [NodeDependency bPid [devKind "dev"]]
                      , ResolveNode appPid [NodeDependency aPid [devKind "dev"]]
                      , ResolveNode bPid [NodeDependency aPid [devKind "dev"]]
                      ]
                  )
          meta `shouldBe'` expected

          let graph = buildGraph False meta
              aDep = mkDep "a" "1.0.0" CargoType [EnvDevelopment]
              bDep = mkDep "b" "1.0.0" CargoType [EnvDevelopment]
          expectDeps' [aDep, bDep] graph
          expectDirect' [aDep] graph

    it' "a build-only transitive chain labels the whole subtree build" $ do
      root <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/build_chain_ws")
      members <- enumerateWorkspaceMembers root
      analysis <- analyzeManifests root members
      case parseCargoLock buildChainLock of
        Left err -> expectationFailure' ("parseCargoLock failed: " ++ show err)
        Right lock -> do
          let appDir = root </> $(mkRelDir "crates/app")
              appPid = PackageId "app" "0.1.0" ("path+file://" <> toText (toFilePath appDir))
          members `shouldBe'` [WorkspaceMember "app" appDir]
          let aPid = mkPkgId "a" "1.0.0"
              bPid = mkPkgId "b" "1.0.0"
              meta = lockfileToMetadata lock members analysis
              expected =
                CargoMetadata
                  [ Package "a" "1.0.0" aPid Nothing Nothing [] (Just registrySource)
                  , Package "app" "0.1.0" appPid Nothing Nothing [] Nothing
                  , Package "b" "1.0.0" bPid Nothing Nothing [] (Just registrySource)
                  ]
                  [appPid]
                  ( Resolve
                      [ ResolveNode aPid [NodeDependency bPid [devKind "build"]]
                      , ResolveNode appPid [NodeDependency aPid [devKind "build"]]
                      , ResolveNode bPid []
                      ]
                  )
          meta `shouldBe'` expected

          let graph = buildGraph False meta
              aDep = mkDep "a" "1.0.0" CargoType [EnvDevelopment]
              bDep = mkDep "b" "1.0.0" CargoType [EnvDevelopment]
          expectDeps' [aDep, bDep] graph
          expectDirect' [aDep] graph

    it' "an orphan package unreachable from any member defaults to production" $ do
      -- "orphan" is stale lockfile residue: no member edge reaches it. Its
      -- outgoing edge must fall back to the default (Production) rather than
      -- inheriting a propagated kind.
      root <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/orphan_ws")
      members <- enumerateWorkspaceMembers root
      analysis <- analyzeManifests root members
      case parseCargoLock orphanLock of
        Left err -> expectationFailure' ("parseCargoLock failed: " ++ show err)
        Right lock -> do
          let appDir = root </> $(mkRelDir "crates/app")
              appPid = PackageId "app" "0.1.0" ("path+file://" <> toText (toFilePath appDir))
          members `shouldBe'` [WorkspaceMember "app" appDir]
          let orphanPid = mkPkgId "orphan" "1.0.0"
              childPid = mkPkgId "orphan_child" "2.0.0"
              meta = lockfileToMetadata lock members analysis
              expected =
                CargoMetadata
                  [ Package "app" "0.1.0" appPid Nothing Nothing [] Nothing
                  , Package "orphan" "1.0.0" orphanPid Nothing Nothing [] (Just registrySource)
                  , Package "orphan_child" "2.0.0" childPid Nothing Nothing [] (Just registrySource)
                  ]
                  [appPid]
                  ( Resolve
                      [ ResolveNode appPid []
                      , ResolveNode orphanPid [NodeDependency childPid [nullKind]]
                      , ResolveNode childPid []
                      ]
                  )
          meta `shouldBe'` expected
