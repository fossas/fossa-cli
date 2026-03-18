module Cargo.MetadataSpec (
  spec,
) where

import Data.Aeson
import Data.ByteString.Lazy qualified as BL
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
      let graph = buildGraph expectedMetadataPre1_77
      expectDeps [ansiTermDep, clapDep] graph
      expectEdges [(clapDep, ansiTermDep)] graph
      expectDirect [clapDep] graph

  post1_77MetadataParseSpec

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
      let graph = buildGraph expectedMetadataPost1_77
      expectDeps [ansiTermDep, clapDep, fooDep, barDep] graph
      expectEdges [(clapDep, ansiTermDep)] graph
      expectDirect [clapDep, fooDep, barDep] graph

    Test.it "git deps should produce git-backed locator names" $ do
      let graph = buildGraph expectedMetadataPost1_77
      -- barDep has a git+ssh source; its name should be repo-url#crate-name
      Graphing.vertexList graph `shouldSatisfy` elem barDep
      dependencyName barDep `shouldBe` "github.com/user/bar#bar"

    Test.it "git deps with HTTPS and tag produce git-backed locator names" $ do
      let httpsGitId = PackageId "locator" "3.0.3" "git+https://github.com/fossas/locator-rs?tag=v3.0.3#54c724df"
          httpsGitNode = ResolveNode httpsGitId []
          meta = CargoMetadata [] [jfmtId] $ Resolve [jfmtNodeWithHttpsGit, httpsGitNode]
          jfmtNodeWithHttpsGit = ResolveNode jfmtId [NodeDependency httpsGitId [nullKind]]
          expectedDep = mkDep "github.com/fossas/locator-rs#locator" "3.0.3" CargoType [EnvProduction]
      Graphing.vertexList (buildGraph meta) `shouldSatisfy` elem expectedDep

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
          graph = buildGraph meta
      expectDeps [depA, depB] graph
      dependencyName depA `shouldBe` "github.com/fossas/locator-rs#locator"
      dependencyName depB `shouldBe` "github.com/fossas/locator-rs#locator-codegen"

    Test.it "registry deps remain unchanged" $ do
      let graph = buildGraph expectedMetadataPost1_77
      Graphing.vertexList graph `shouldSatisfy` elem ansiTermDep
      dependencyName ansiTermDep `shouldBe` "ansi_term"

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

    Test.it "returns Nothing for empty string" $
      parseGitRepoUrl ""
        `shouldBe` Nothing
