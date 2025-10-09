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
import Graphing
import Strategy.Cargo
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
      let graph = pruneUnreachable $ buildGraph expectedMetadataPre1_77
      expectDeps [ansiTermDep, clapDep] graph
      expectEdges [(clapDep, ansiTermDep)] graph
      expectDirect [clapDep] graph

  post1_77MetadataParseSpec

ansiTermIdNoVersion :: PackageId
ansiTermIdNoVersion = mkPkgId "ansi_term" "*"

ansiTermNodeNoVersion :: ResolveNode
ansiTermNodeNoVersion = ResolveNode ansiTermIdNoVersion []

fooPathDepId :: PackageId
fooPathDepId = PackageId "foo" "*" "file:///path/to/my/project/foo"

fooPathNode :: ResolveNode
fooPathNode = ResolveNode fooPathDepId []

fooDep :: Dependency
fooDep = mkDep "foo" "*" UnresolvedPathType [EnvProduction]

barPathDepId :: PackageId
barPathDepId = PackageId "bar" "2.0.0" "git+ssh://github.com/user/bar"

barPathNode :: ResolveNode
barPathNode = ResolveNode barPathDepId []

barDep :: Dependency
barDep = mkDep "bar" "2.0.0" GitType [EnvProduction]

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
      let graph = pruneUnreachable $ buildGraph expectedMetadataPost1_77
      expectDeps [ansiTermDep, clapDep, fooDep, barDep] graph
      expectEdges [(clapDep, ansiTermDep)] graph
      expectDirect [clapDep, fooDep, barDep] graph
