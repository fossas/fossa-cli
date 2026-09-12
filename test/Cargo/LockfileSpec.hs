{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Stage 2 tests for the Cargo.lock -> CargoMetadata conversion pipeline.
--
-- Covers:
--   * workspace member enumeration (single crate, virtual workspace,
--     members + default-members);
--   * per-member dependency-kind derivation (prod / dev / build, renames,
--     workspace inheritance, target tables, non-member path deps);
--   * the pure lockfile-vs-metadata source decision;
--   * golden lockfile -> CargoMetadata conversions (2-member workspace with a
--     prod+build member path dep, a non-member build-dep subtree, git deps
--     tagged/untagged, and name-version disambiguation).
module Cargo.LockfileSpec (
  spec,
) where

import Control.Effect.Lift (sendIO)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.String.Conversion (toText)
import Data.Text (Text)
import DepTypes
import GraphUtil
import Graphing qualified
import Path (mkRelDir, toFilePath, (</>))
import Path.IO (makeAbsolute)
import Strategy.Cargo
import Strategy.CargoLock
import Test.Effect (expectationFailure', it', shouldBe', shouldContain', shouldNotContain')
import Test.Hspec (Spec, describe, it)
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
  goldenConversionSpecs

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
      -- string dep + renamed dep (package = "serde") both collapse to "serde".
      kinds ("app", "serde") `shouldBe'` Just (Set.singleton DepProd)
      -- declared in all three sections.
      kinds ("app", "dual") `shouldBe'` Just (Set.fromList [DepProd, DepBuild, DepDev])
      kinds ("app", "cc") `shouldBe'` Just (Set.singleton DepBuild)
      kinds ("app", "approx") `shouldBe'` Just (Set.singleton DepDev)
      -- a dep under [target.'cfg(unix)'.dev-dependencies].
      kinds ("app", "unixtool") `shouldBe'` Just (Set.singleton DepDev)
      -- resolvable workspace inheritance keeps the key as the crate name.
      kinds ("app", "shared") `shouldBe'` Just (Set.singleton DepProd)
      -- unresolvable workspace inheritance still classifies as prod + warns.
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
          -- The root package is a member alongside the glob-matched one.
          members `shouldBe'` [WorkspaceMember "rootpkg" root, WorkspaceMember "alpha" alphaDir]
          let builddPid = mkPkgId "buildd" "1.0.0"
              devdepPid = mkPkgId "devdep" "1.0.0"
              sharedPid = mkPkgId "shared_reg" "1.0.0"
              meta = lockfileToMetadata lock members analysis
          -- The root package is in the member ids (lockfile order).
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
              -- The root's dev/build dep edges carry their kind-derived
              -- (non-Production) labels.
              builddDep = mkDep "buildd" "1.0.0" CargoType [EnvDevelopment]
              devdepDep = mkDep "devdep" "1.0.0" CargoType [EnvDevelopment]
              sharedDep = mkDep "shared_reg" "1.0.0" CargoType [EnvProduction]
          expectDeps' [builddDep, devdepDep, sharedDep] graph
          expectDirect' [builddDep, devdepDep, sharedDep] graph
          -- shrinkRoots: the root package is not a direct dep vertex.
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
      -- "ext_build" cannot be classified, because dependency kinds are only
      -- known for member manifests. "ext_build" is therefore treated as
      -- Production even though, semantically, it is a build-dep of "ext". This
      -- pins the documented, accepted gap (see the Strategy.CargoLock header).
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
              -- Production (not Development) is the accepted gap.
              extBuildDep = mkDep "ext_build" "0.2.0" CargoType [EnvProduction]
          expectDeps' [extDep, extBuildDep] graph
          -- "ext" is a non-member root; "ext_build" is its (Production) child.
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
                      [ ResolveNode dPid [NodeDependency at12 [nullKind, devKind "build"], NodeDependency at11 [nullKind, devKind "build"]]
                      , ResolveNode at11 []
                      , ResolveNode at12 []
                      ]
                  )
          meta `shouldBe'` expected

          let graph = buildGraph False meta
              at11Dep = mkDep "ansi_term" "0.11.0" CargoType [EnvProduction, EnvDevelopment]
              at12Dep = mkDep "ansi_term" "0.12.1" CargoType [EnvProduction, EnvDevelopment]
          expectDeps' [at11Dep, at12Dep] graph
          expectDirect' [at11Dep, at12Dep] graph
