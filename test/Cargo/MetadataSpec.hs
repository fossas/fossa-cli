module Cargo.MetadataSpec (
  spec,
) where

import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes
import GraphUtil
import Graphing qualified
import Strategy.Cargo
import Test.Hspec (shouldBe, shouldSatisfy)
import Test.Hspec qualified as Test

expectedMetadataPre1_77 :: CargoMetadata
expectedMetadataPre1_77 = CargoMetadata [] [jfmtId] $ Resolve expectedResolveNodes

expectedResolveNodes :: [ResolveNode]
expectedResolveNodes = [ansiTermNode, clapNode, jfmtNode]

registrySource :: Text.Text
registrySource = "registry+https://github.com/rust-lang/crates.io-index"

mkPkgId :: Text.Text -> Text.Text -> PackageId
mkPkgId name ver = PackageId name ver registrySource

mkDep :: Text -> Text -> DepType -> [DepEnvironment] -> Dependency
mkDep name version depType envs = Dependency depType name (Just $ CEq version) [] (Set.fromList envs) Map.empty

ansiTermId :: PackageId
ansiTermId = mkPkgId "ansi_term" "0.11.0"

ansiTermDep :: Dependency
ansiTermDep = mkDep "ansi_term" "0.11.0" CargoType [EnvProduction]

clapId :: PackageId
clapId = mkPkgId "clap" "2.33.0"

clapDep :: Dependency
clapDep = mkDep "clap" "2.33.0" CargoType [EnvProduction]

jfmtId :: PackageId
jfmtId = PackageId "jfmt" "1.0.0" "path+file:///path/to/jfmt.rs"

ansiTermNode :: ResolveNode
ansiTermNode = ResolveNode ansiTermId []

clapNode :: ResolveNode
clapNode = ResolveNode clapId [NodeDependency ansiTermId [NodeDepKind Nothing $ Just "cfg(not(windows))"]]

jfmtNode :: ResolveNode
jfmtNode = ResolveNode jfmtId [NodeDependency clapId [nullKind]]

nullKind :: NodeDepKind
nullKind = NodeDepKind Nothing Nothing

spec :: Test.Spec
spec = do
  Test.describe "cargo metadata parser, < 1.77.0" $ do
    metaBytes <- Test.runIO $ BL.readFile "test/Cargo/testdata/expected-metadata.json"
    Test.it "should properly construct a resolution tree" $
      case eitherDecode metaBytes of
        Left err -> Test.expectationFailure $ "failed to parse: " ++ err
        Right result -> result `Test.shouldBe` expectedMetadataPre1_77

    Test.it "should build the correct graph" $ do
      let graph = buildGraph False expectedMetadataPre1_77
      expectDeps [ansiTermDep, clapDep] graph
      expectEdges [(clapDep, ansiTermDep)] graph
      expectDirect [clapDep] graph

  post1_77MetadataParseSpec

  extractGitCommitHashSpec

  gitCommitHashVersionSpec

  parseGitRepoUrlSpec

ansiTermIdNoVersion :: PackageId
ansiTermIdNoVersion = mkPkgId "ansi_term" "*"

ansiTermNodeNoVersion :: ResolveNode
ansiTermNodeNoVersion = ResolveNode ansiTermIdNoVersion []

fooPathDepId :: PackageId
fooPathDepId = PackageId "foo" "*" "file:///path/to/my/project/foo"

fooPathNode :: ResolveNode
fooPathNode = ResolveNode fooPathDepId []

fooDep :: Dependency
fooDep = mkDep "/path/to/my/project/foo" "*" UnresolvedPathType [EnvProduction]

barPathDepId :: PackageId
barPathDepId = PackageId "bar" "2.0.0" "git+ssh://github.com/user/bar"

barPathNode :: ResolveNode
barPathNode = ResolveNode barPathDepId []

barDep :: Dependency
barDep = mkDep "github.com/user/bar#bar" "2.0.0" CargoType [EnvProduction]

barDepFallback :: Dependency
barDepFallback = mkDep "bar" "2.0.0" CargoType [EnvProduction]

jfmtNodePost1_77 :: ResolveNode
jfmtNodePost1_77 = ResolveNode jfmtId [ansiTermNodeDep, fooNodeDep, barNodeDep]
  where
    ansiTermNodeDep = NodeDependency clapId [nullKind]
    fooNodeDep = NodeDependency fooPathDepId [nullKind]
    barNodeDep = NodeDependency barPathDepId [nullKind]

expectedResolveNodesPost1_77 :: [ResolveNode]
expectedResolveNodesPost1_77 = [ansiTermNodeNoVersion, fooPathNode, barPathNode, clapNode, jfmtNodePost1_77]

expectedMetadataPost1_77 :: CargoMetadata
expectedMetadataPost1_77 = CargoMetadata [] [jfmtId] $ Resolve expectedResolveNodesPost1_77

post1_77MetadataParseSpec :: Test.Spec
post1_77MetadataParseSpec =
  Test.describe "cargo metadata parser, >= 1.77.0" $ do
    metaBytes <- Test.runIO $ BL.readFile "test/Cargo/testdata/expected-metadata-1.77.2.json"
    Test.it "should properly construct a resolution tree" $
      case eitherDecode metaBytes of
        Left err -> Test.expectationFailure $ "failed to parse: " ++ err
        Right result -> result `Test.shouldBe` expectedMetadataPost1_77

    Test.it "should build the correct graph" $ do
      let graph = buildGraph True expectedMetadataPost1_77
      expectDeps [ansiTermDep, clapDep, fooDep, barDep] graph
      expectEdges [(clapDep, ansiTermDep)] graph
      expectDirect [clapDep, fooDep, barDep] graph

    Test.it "git deps should produce git-backed locator names" $ do
      let graph = buildGraph True expectedMetadataPost1_77
          vertices = Graphing.vertexList graph
          -- Find the cargo dep with bar's version to verify its name came from the git URL
          barVertex = find (\d -> dependencyVersion d == Just (CEq "2.0.0") && dependencyType d == CargoType) vertices
      fmap dependencyName barVertex `shouldBe` Just "github.com/user/bar#bar"

    Test.it "git deps with HTTPS and tag produce git-backed locator names" $ do
      let httpsGitId = PackageId "locator" "3.0.3" "git+https://github.com/fossas/locator-rs?tag=v3.0.3#54c724df"
          httpsGitNode = ResolveNode httpsGitId []
          meta = CargoMetadata [] [jfmtId] $ Resolve [jfmtNodeWithHttpsGit, httpsGitNode]
          jfmtNodeWithHttpsGit = ResolveNode jfmtId [NodeDependency httpsGitId [nullKind]]
          expectedDep = mkDep "github.com/fossas/locator-rs#locator" "3.0.3" CargoType [EnvProduction]
      Graphing.vertexList (buildGraph True meta) `shouldSatisfy` elem expectedDep

    Test.it "multiple crates from same workspace produce distinct locator names" $ do
      let wsSource = "git+https://github.com/fossas/locator-rs?tag=v3.0.3#54c724df"
          crateA = PackageId "locator" "3.0.3" wsSource
          crateB = PackageId "locator-codegen" "3.0.3" wsSource
          nodeA = ResolveNode crateA []
          nodeB = ResolveNode crateB []
          rootNode = ResolveNode jfmtId [NodeDependency crateA [nullKind], NodeDependency crateB [nullKind]]
          meta = CargoMetadata [] [jfmtId] $ Resolve [rootNode, nodeA, nodeB]
          depA = mkDep "github.com/fossas/locator-rs#locator" "3.0.3" CargoType [EnvProduction]
          depB = mkDep "github.com/fossas/locator-rs#locator-codegen" "3.0.3" CargoType [EnvProduction]
          graph = buildGraph True meta
      expectDeps [depA, depB] graph
      dependencyName depA `shouldBe` "github.com/fossas/locator-rs#locator"
      dependencyName depB `shouldBe` "github.com/fossas/locator-rs#locator-codegen"

    Test.it "git dep with tag and no crate name in fragment strips query from name" $ do
      -- When cargo uses the short form #version (no name in fragment), the parser
      -- falls back to the last path segment of the source URL.
      -- This must not include query parameters like ?tag=v0.3.6.
      let parsed = parsePkgId "git+https://github.com/fossas/broker?tag=v0.3.6#0.3.6" :: Maybe PackageId
      fmap pkgIdName parsed `shouldBe` Just "broker"
      fmap pkgIdVersion parsed `shouldBe` Just "0.3.6"
      -- Also verify the full round-trip through buildGraph produces the correct locator name.
      let gitId = PackageId "broker" "0.3.6" "git+https://github.com/fossas/broker?tag=v0.3.6"
          gitNode = ResolveNode gitId []
          meta = CargoMetadata [] [jfmtId] $ Resolve [jfmtNodeWithGit, gitNode]
          jfmtNodeWithGit = ResolveNode jfmtId [NodeDependency gitId [nullKind]]
          expectedDep = mkDep "github.com/fossas/broker#broker" "0.3.6" CargoType [EnvProduction]
      Graphing.vertexList (buildGraph True meta) `shouldSatisfy` elem expectedDep

    Test.it "git dep with branch and no crate name in fragment strips query from name" $ do
      let parsed = parsePkgId "git+https://github.com/fossas/broker?branch=main#0.3.6" :: Maybe PackageId
      fmap pkgIdName parsed `shouldBe` Just "broker"
      fmap pkgIdVersion parsed `shouldBe` Just "0.3.6"

    Test.it "git dep with rev and no crate name in fragment strips query from name" $ do
      let parsed = parsePkgId "git+https://github.com/fossas/broker?rev=abc123#0.3.6" :: Maybe PackageId
      fmap pkgIdName parsed `shouldBe` Just "broker"
      fmap pkgIdVersion parsed `shouldBe` Just "0.3.6"

    Test.it "registry deps remain unchanged" $ do
      let graph = buildGraph True expectedMetadataPost1_77
      Graphing.vertexList graph `shouldSatisfy` elem ansiTermDep
      dependencyName ansiTermDep `shouldBe` "ansi_term"

    Test.it "git deps fall back to plain crate names when server does not support git-backed locators" $ do
      let graph = buildGraph False expectedMetadataPost1_77
      expectDeps [ansiTermDep, clapDep, fooDep, barDepFallback] graph
      dependencyName barDepFallback `shouldBe` "bar"

extractGitCommitHashSpec :: Test.Spec
extractGitCommitHashSpec =
  Test.describe "extractGitCommitHash" $ do
    Test.it "extracts commit hash from git source URL" $
      extractGitCommitHash "git+https://github.com/fossas/foundation-libs#4bc3762e73f371717566fb075d02e1d25b21146e"
        `shouldBe` Just "4bc3762e73f371717566fb075d02e1d25b21146e"

    Test.it "extracts commit hash from git source URL with tag" $
      extractGitCommitHash "git+https://github.com/fossas/broker?tag=v0.3.6#abc123def456"
        `shouldBe` Just "abc123def456"

    Test.it "returns Nothing for registry source" $
      extractGitCommitHash "registry+https://github.com/rust-lang/crates.io-index"
        `shouldBe` Nothing

    Test.it "returns Nothing for path source" $
      extractGitCommitHash "path+file:///some/path"
        `shouldBe` Nothing

    Test.it "returns Nothing for empty string" $
      extractGitCommitHash ""
        `shouldBe` Nothing

mkPackage :: PackageId -> Maybe Text -> Package
mkPackage pid = Package (pkgIdName pid) (pkgIdVersion pid) pid Nothing Nothing []

gitCommitHashVersionSpec :: Test.Spec
gitCommitHashVersionSpec =
  Test.describe "git commit hash as version" $ do
    Test.it "uses commit hash for git dep without tag" $ do
      let gitId = PackageId "srclib" "0.1.0" "git+https://github.com/fossas/foundation-libs"
          gitPkg = mkPackage gitId (Just "git+https://github.com/fossas/foundation-libs#4bc3762e73f371717566fb075d02e1d25b21146e")
          gitNode = ResolveNode gitId []
          meta = CargoMetadata [gitPkg] [jfmtId] $ Resolve [jfmtNodeWith gitId, gitNode]
          graph = buildGraph True meta
          vertices = Graphing.vertexList graph
          dep = find (\d -> dependencyName d == "github.com/fossas/foundation-libs#srclib") vertices
      fmap dependencyVersion dep `shouldBe` Just (Just $ CEq "4bc3762e73f371717566fb075d02e1d25b21146e")

    Test.it "uses crate version for git dep with tag" $ do
      let gitId = PackageId "broker" "0.3.6" "git+https://github.com/fossas/broker?tag=v0.3.6"
          gitPkg = mkPackage gitId (Just "git+https://github.com/fossas/broker?tag=v0.3.6#abc123def456")
          gitNode = ResolveNode gitId []
          meta = CargoMetadata [gitPkg] [jfmtId] $ Resolve [jfmtNodeWith gitId, gitNode]
          graph = buildGraph True meta
          vertices = Graphing.vertexList graph
          dep = find (\d -> dependencyName d == "github.com/fossas/broker#broker") vertices
      fmap dependencyVersion dep `shouldBe` Just (Just $ CEq "0.3.6")

    Test.it "uses commit hash for git dep with branch" $ do
      let gitId = PackageId "mylib" "1.0.0" "git+https://github.com/owner/mylib?branch=main"
          gitPkg = mkPackage gitId (Just "git+https://github.com/owner/mylib?branch=main#deadbeef")
          gitNode = ResolveNode gitId []
          meta = CargoMetadata [gitPkg] [jfmtId] $ Resolve [jfmtNodeWith gitId, gitNode]
          graph = buildGraph True meta
          vertices = Graphing.vertexList graph
          dep = find (\d -> dependencyName d == "github.com/owner/mylib#mylib") vertices
      fmap dependencyVersion dep `shouldBe` Just (Just $ CEq "deadbeef")

    Test.it "uses commit hash for git dep with rev" $ do
      let gitId = PackageId "mylib" "1.0.0" "git+https://github.com/owner/mylib?rev=deadbeef"
          gitPkg = mkPackage gitId (Just "git+https://github.com/owner/mylib?rev=deadbeef#deadbeef")
          gitNode = ResolveNode gitId []
          meta = CargoMetadata [gitPkg] [jfmtId] $ Resolve [jfmtNodeWith gitId, gitNode]
          graph = buildGraph True meta
          vertices = Graphing.vertexList graph
          dep = find (\d -> dependencyName d == "github.com/owner/mylib#mylib") vertices
      fmap dependencyVersion dep `shouldBe` Just (Just $ CEq "deadbeef")

    Test.it "falls back to crate version when source map has no entry" $ do
      let gitId = PackageId "srclib" "0.1.0" "git+https://github.com/fossas/foundation-libs"
          gitNode = ResolveNode gitId []
          meta = CargoMetadata [] [jfmtId] $ Resolve [jfmtNodeWith gitId, gitNode]
          graph = buildGraph True meta
          vertices = Graphing.vertexList graph
          dep = find (\d -> dependencyName d == "github.com/fossas/foundation-libs#srclib") vertices
      fmap dependencyVersion dep `shouldBe` Just (Just $ CEq "0.1.0")
  where
    jfmtNodeWith depId = ResolveNode jfmtId [NodeDependency depId [nullKind]]

parseGitRepoUrlSpec :: Test.Spec
parseGitRepoUrlSpec =
  Test.describe "parseGitRepoUrl" $ do
    Test.it "parses HTTPS URL with tag" $
      parseGitRepoUrl "git+https://github.com/fossas/locator-rs?tag=v3.0.3#54c724df"
        `shouldBe` Just "github.com/fossas/locator-rs"

    Test.it "parses HTTPS URL with branch" $
      parseGitRepoUrl "git+https://github.com/owner/repo?branch=main#abc123"
        `shouldBe` Just "github.com/owner/repo"

    Test.it "parses HTTPS URL with rev" $
      parseGitRepoUrl "git+https://github.com/owner/repo?rev=abc123#abc123"
        `shouldBe` Just "github.com/owner/repo"

    Test.it "parses HTTPS URL with default branch" $
      parseGitRepoUrl "git+https://github.com/owner/repo#abc123"
        `shouldBe` Just "github.com/owner/repo"

    Test.it "parses SSH URL" $
      parseGitRepoUrl "git+ssh://git@github.com/owner/repo?tag=v1.0.0#abc123"
        `shouldBe` Just "github.com/owner/repo"

    Test.it "strips .git suffix" $
      parseGitRepoUrl "git+https://github.com/owner/repo.git?tag=v1.0.0#abc123"
        `shouldBe` Just "github.com/owner/repo"

    Test.it "returns Nothing for registry source" $
      parseGitRepoUrl "registry+https://github.com/rust-lang/crates.io-index"
        `shouldBe` Nothing

    Test.it "returns Nothing for path source" $
      parseGitRepoUrl "path+file:///some/path"
        `shouldBe` Nothing

    Test.it "parses bare git URL without fragment" $
      parseGitRepoUrl "git+https://github.com/owner/repo"
        `shouldBe` Just "github.com/owner/repo"

    Test.it "returns Nothing for empty string" $
      parseGitRepoUrl ""
        `shouldBe` Nothing
