module Cargo.MetadataSpec
  ( spec
  ) where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import DepTypes
import GraphUtil
import Graphing
import Strategy.Cargo
import qualified Test.Hspec as Test

expectedMetadata :: CargoMetadata
expectedMetadata = CargoMetadata [] [jfmtId] $ Resolve expectedResolveNodes

expectedResolveNodes :: [ResolveNode]
expectedResolveNodes = [ansiTermNode, clapNode, jfmtNode]

registrySource :: T.Text
registrySource = "(registry+https://github.com/rust-lang/crates.io-index)"

mkPkgId :: T.Text -> T.Text -> PackageId
mkPkgId name ver = PackageId name ver registrySource

mkDep :: Text -> Text -> [DepEnvironment] -> Dependency
mkDep name version envs = Dependency CargoType name (Just $ CEq version) [] envs M.empty

ansiTermId :: PackageId
ansiTermId = mkPkgId "ansi_term" "0.11.0"

ansiTermDep :: Dependency
ansiTermDep = mkDep "ansi_term" "0.11.0" [EnvProduction]

clapId :: PackageId
clapId = mkPkgId "clap" "2.33.0"

clapDep :: Dependency
clapDep = mkDep "clap" "2.33.0" [EnvProduction]

jfmtId :: PackageId
jfmtId = PackageId "jfmt" "1.0.0" "(path+file:///path/to/jfmt.rs)"

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
  Test.describe "cargo metadata parser" $ do
    metaBytes <- Test.runIO $ BL.readFile "test/Cargo/testdata/expected-metadata.json"
    Test.it "should properly construct a resolution tree" $
      case eitherDecode metaBytes of
        Left err -> Test.expectationFailure $ "failed to parse: " ++ err
        Right result -> result `Test.shouldBe` expectedMetadata

  Test.describe "cargo metadata graph" $ do
    let graph = pruneUnreachable $ buildGraph expectedMetadata

    Test.it "should build the correct graph" $ do
      expectDeps [ansiTermDep, clapDep] graph
      expectEdges [(clapDep, ansiTermDep)] graph
      expectDirect [clapDep] graph
