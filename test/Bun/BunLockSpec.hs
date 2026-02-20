{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Bun.BunLockSpec (
  spec,
) where

import Data.Aeson (eitherDecodeStrict)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.String.Conversion (encodeUtf8)
import Data.Text (Text)
import Data.Text.IO qualified as TextIO
import Data.Text.Jsonc (stripJsonc)
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (GitType, NodeJSType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.ReadFS (readContentsJsonc)
import GraphUtil (expectEdge)
import Graphing (Graphing)
import Graphing qualified
import Path (Abs, File, Path, fromAbsFile, mkRelDir, mkRelFile, (</>))
import Path.IO (getCurrentDir)
import Strategy.Node.Bun.BunLock (
  BunLockfile (..),
  BunPackage (..),
  BunPackageDeps (..),
  BunWorkspace (..),
  buildGraph,
  parseResolution,
 )
import Test.Effect (it', shouldBe')
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, runIO, shouldBe)

spec :: Spec
spec = do
  currentDir <- runIO getCurrentDir
  let testdata = currentDir </> $(mkRelDir "test/Bun/testdata")
  let jsoncPath = testdata </> $(mkRelFile "jsonc/bun.lock")
  let depsPath = testdata </> $(mkRelFile "dependencies/bun.lock")
  let wsPath = testdata </> $(mkRelFile "workspaces/bun.lock")
  let bunProjectPath = testdata </> $(mkRelFile "bun-project/bun.lock")
  let gitDepsPath = testdata </> $(mkRelFile "git-deps/bun.lock")
  let mixedEnvsPath = testdata </> $(mkRelFile "mixed-envs/bun.lock")

  parseResolutionSpec
  jsoncSpec jsoncPath
  dependenciesSpec depsPath
  workspacesSpec wsPath
  bunProjectSpec bunProjectPath
  gitDepsSpec gitDepsPath
  mixedEnvsSpec mixedEnvsPath

parseResolutionSpec :: Spec
parseResolutionSpec = describe "parseResolution" $ do
  it "parses unscoped packages" $
    parseResolution "lodash@4.17.21" `shouldBe` ("lodash", "4.17.21")

  it "parses scoped packages" $
    parseResolution "@scope/pkg@1.0.0" `shouldBe` ("@scope/pkg", "1.0.0")

  it "parses file references" $
    parseResolution "pkg@file:../local" `shouldBe` ("pkg", "file:../local")

  it "parses workspace references" $
    parseResolution "pkg@workspace:packages/a" `shouldBe` ("pkg", "workspace:packages/a")

  it "parses github references" $
    parseResolution "pkg@github:user/repo#abc123" `shouldBe` ("pkg", "github:user/repo#abc123")

  it "parses git+ references" $
    parseResolution "pkg@git+https://github.com/user/repo#abc123" `shouldBe` ("pkg", "git+https://github.com/user/repo#abc123")

-- | JSONC: verifies that line comments, block comments, and trailing commas
-- are stripped correctly and the result parses as a valid bun lockfile.
jsoncSpec :: Path Abs File -> Spec
jsoncSpec path =
  describe "jsonc" $ do
    it' "parses bun.lock with comments and trailing commas" $ do
      lockfile <- readContentsJsonc @BunLockfile path
      wsName (workspaces lockfile Map.! "") `shouldBe'` "jsonc-test"
      wsDependencies (workspaces lockfile Map.! "") `shouldBe'` Map.fromList [("lodash", "^4.17.21")]

-- | Dependencies: all dependency types on a single root workspace,
-- transitive dependency chains, and environment labeling.
dependenciesSpec :: Path Abs File -> Spec
dependenciesSpec path =
  describe "dependencies" $ do
    it' "parses production, dev, and optional dependencies" $ do
      lockfile <- readContentsJsonc @BunLockfile path
      let rootWs = workspaces lockfile Map.! ""
      wsDependencies rootWs `shouldBe'` Map.fromList [("express", "^4.18.2")]
      wsDevDependencies rootWs `shouldBe'` Map.fromList [("typescript", "^5.0.0")]
      wsOptionalDependencies rootWs `shouldBe'` Map.fromList [("fsevents", "^2.3.3")]

    it' "parses transitive dependency info" $ do
      lockfile <- readContentsJsonc @BunLockfile path
      let expressPkg = packages lockfile Map.! "express"
      pkgDepsDependencies (pkgDeps expressPkg) `shouldBe'` Map.fromList [("accepts", "~1.3.8")]

    describe "graph" $ do
      checkGraph path $ \graph -> do
        let directDeps = Graphing.directList graph

        it "marks production dependencies as direct" $
          directDeps `shouldContainDep` mkProdDep "express" "4.18.2"

        it "marks dev dependencies as direct" $
          directDeps `shouldContainDep` mkDevDep "typescript" "5.3.3"

        it "marks optional dependencies as direct" $
          directDeps `shouldContainDep` mkProdDep "fsevents" "2.3.3"

        it "creates transitive edges" $ do
          expectEdge graph (mkProdDep "express" "4.18.2") (mkProdDep "accepts" "1.3.8")
          expectEdge graph (mkProdDep "accepts" "1.3.8") (mkProdDep "mime-types" "2.1.35")

        it "labels transitive deps of dev deps as production" $ do
          expectEdge graph (mkDevDep "typescript" "5.3.3") (mkProdDep "semver" "7.6.0")

-- | Workspaces: multiple workspaces, workspace refs, and workspace
-- package filtering from the final graph.
workspacesSpec :: Path Abs File -> Spec
workspacesSpec path =
  describe "workspaces" $ do
    it' "parses multiple workspaces" $ do
      lockfile <- readContentsJsonc @BunLockfile path
      let ws = workspaces lockfile
      Map.size ws `shouldBe'` 3
      wsName (ws Map.! "") `shouldBe'` "workspace-root"
      wsName (ws Map.! "packages/app") `shouldBe'` "@types/app"
      wsName (ws Map.! "packages/utils") `shouldBe'` "utils"

    it' "parses workspace package resolutions" $ do
      lockfile <- readContentsJsonc @BunLockfile path
      pkgResolution (packages lockfile Map.! "@types/app") `shouldBe'` "@types/app@workspace:packages/app"
      pkgResolution (packages lockfile Map.! "utils") `shouldBe'` "utils@workspace:packages/utils"

    describe "graph" $ do
      checkGraph path $ \graph -> do
        let directDeps = Graphing.directList graph

        it "marks sub-workspace dependencies as direct" $
          directDeps `shouldContainDep` mkProdDep "lodash" "4.17.21"

        it "excludes workspace packages from graph" $ do
          let names = map dependencyName (Graphing.vertexList graph)
          names `shouldNotContain` "@types/app"
          names `shouldNotContain` "utils"

-- | Bun project: a real-world bun.lock from the bun project itself.
-- Covers scoped packages, git refs, nested package keys, optional/peer deps
-- in packages, and large dependency counts.
bunProjectSpec :: Path Abs File -> Spec
bunProjectSpec path =
  describe "bun-project" $ do
    it' "parses workspaces" $ do
      lockfile <- readContentsJsonc @BunLockfile path
      Map.size (workspaces lockfile) `shouldBe'` 3
      wsName (workspaces lockfile Map.! "") `shouldBe'` "bun"

    it' "parses scoped and workspace package resolutions" $ do
      lockfile <- readContentsJsonc @BunLockfile path
      let pkgs = packages lockfile
      pkgResolution (pkgs Map.! "@types/bun") `shouldBe'` "@types/bun@workspace:packages/@types/bun"
      pkgResolution (pkgs Map.! "esbuild") `shouldBe'` "esbuild@0.21.5"

    it' "parses transitive dependency info" $ do
      lockfile <- readContentsJsonc @BunLockfile path
      let lezerCpp = packages lockfile Map.! "@lezer/cpp"
      Map.member "@lezer/common" (pkgDepsDependencies $ pkgDeps lezerCpp) `shouldBe'` True

    describe "graph" $ do
      checkGraph path $ \graph -> do
        it "marks dev dependencies as direct" $ do
          let directDeps = Graphing.directList graph
          directDeps `shouldContainDep` mkDevDep "esbuild" "0.21.5"
          directDeps `shouldContainDep` mkDevDep "typescript" "5.9.2"

        it "creates transitive edges" $
          expectEdge graph (mkDevDep "@lezer/cpp" "1.1.3") (mkDevDep "@lezer/common" "1.3.0")

        it "includes git dependencies as GitType" $ do
          let gitDeps = filter (\d -> dependencyType d == GitType) (Graphing.vertexList graph)
          gitDeps `shouldContainDep` mkGitDep "oven-sh/bun.report" "912ca63"

        it "excludes workspace packages from graph" $ do
          let names = map dependencyName (Graphing.vertexList graph)
          names `shouldNotContain` "@types/bun"
          names `shouldNotContain` "bun-types"

-- | Git dependencies: github: and git+ resolution types.
gitDepsSpec :: Path Abs File -> Spec
gitDepsSpec path =
  describe "git-deps" $ do
    describe "graph" $ do
      checkGraph path $ \graph -> do
        let directDeps = Graphing.directList graph

        it "includes github: dependencies as GitType with repo as name and ref as version" $
          directDeps `shouldContainDep` mkGitDep' EnvProduction "user/repo" "abc123"

        it "includes git+ dependencies as GitType with URL as name and ref as version" $
          directDeps `shouldContainDep` mkGitDep' EnvProduction "https://github.com/other/project.git" "def456"

        it "creates edges from git+ deps to their transitive deps" $
          expectEdge
            graph
            (mkGitDep' EnvProduction "https://github.com/other/project.git" "def456")
            (mkProdDep "lodash" "4.17.21")

-- | Mixed environments: the same package appears as a production dependency
-- in one workspace and a dev dependency in another. With LabeledGrapher the
-- environments merge into a single vertex rather than creating duplicates.
mixedEnvsSpec :: Path Abs File -> Spec
mixedEnvsSpec path =
  describe "mixed-envs" $ do
    describe "graph" $ do
      checkGraph path $ \graph -> do
        let directDeps = Graphing.directList graph

        it "merges environments when a package is prod in one workspace and dev in another" $ do
          directDeps `shouldContainDep` mkBothEnvsDep "lodash" "4.17.21"

        it "produces a single vertex for the package" $ do
          let lodashVertices = filter (\d -> dependencyName d == "lodash") (Graphing.vertexList graph)
          length lodashVertices `shouldBe` 1

-- | Parse a bun.lock in IO for graph tests (outside the effect stack).
checkGraph :: Path Abs File -> (Graphing Dependency -> Spec) -> Spec
checkGraph path graphSpec = do
  result <- runIO $ parseBunLockIO path
  case result of
    Left err ->
      describe (fromAbsFile path) $
        it "should parse" (expectationFailure err)
    Right lockfile -> graphSpec (buildGraph lockfile)

parseBunLockIO :: Path Abs File -> IO (Either String BunLockfile)
parseBunLockIO path = do
  contents <- TextIO.readFile (fromAbsFile path)
  case stripJsonc contents of
    Left err -> pure $ Left err
    Right stripped -> pure $ eitherDecodeStrict (encodeUtf8 stripped)

shouldContainDep :: [Dependency] -> Dependency -> Expectation
shouldContainDep deps dep
  | dep `elem` deps = pure ()
  | otherwise = expectationFailure $ show (dependencyName dep) ++ " not found in direct dependencies"

shouldNotContain :: (Eq a, Show a) => [a] -> a -> Expectation
shouldNotContain xs x
  | x `elem` xs = expectationFailure $ show x ++ " should not be in " ++ show xs
  | otherwise = pure ()

mkProdDep :: Text -> Text -> Dependency
mkProdDep name version = mkDep NodeJSType name version EnvProduction

mkDevDep :: Text -> Text -> Dependency
mkDevDep name version = mkDep NodeJSType name version EnvDevelopment

mkGitDep :: Text -> Text -> Dependency
mkGitDep name version = mkDep GitType name version EnvDevelopment

mkGitDep' :: DepEnvironment -> Text -> Text -> Dependency
mkGitDep' env name version = mkDep GitType name version env

mkBothEnvsDep :: Text -> Text -> Dependency
mkBothEnvsDep name version =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = name
    , dependencyVersion = Just (CEq version)
    , dependencyLocations = mempty
    , dependencyEnvironments = Set.fromList [EnvProduction, EnvDevelopment]
    , dependencyTags = mempty
    }

mkDep :: DepType -> Text -> Text -> DepEnvironment -> Dependency
mkDep depType name version env =
  Dependency
    { dependencyType = depType
    , dependencyName = name
    , dependencyVersion = Just (CEq version)
    , dependencyLocations = mempty
    , dependencyEnvironments = Set.singleton env
    , dependencyTags = mempty
    }
