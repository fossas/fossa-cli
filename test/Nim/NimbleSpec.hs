module Nim.NimbleSpec (
  spec,
) where

import Data.Aeson (decodeFileStrict')
import Data.Text (Text)
import DepTypes (DepType (GitType), Dependency (Dependency), VerConstraint (CEq))
import GraphUtil (expectDeps, expectDirect, expectEdges)
import Strategy.Nim.NimbleLock (
  NimPackage (NimPackage),
  NimbleDownloadMethod (NimbleDownloadMethodGit),
  NimbleDump (..),
  NimbleLock (NimbleLock),
  NimbleRequire (..),
  PackageName (PackageName),
  buildGraph,
 )
import Test.Hspec (
  Spec,
  describe,
  it,
  runIO,
  shouldBe,
 )

expectedNimbleDumpContent :: NimbleDump
expectedNimbleDumpContent = NimbleDump [NimbleRequire $ PackageName "nim", NimbleRequire $ PackageName "C"]

expectedResolvedContent :: NimbleLock
expectedResolvedContent =
  NimbleLock
    [ NimPackage (PackageName "A") "0.1.1" "https://github.com/A" NimbleDownloadMethodGit "a" []
    , NimPackage (PackageName "B") "0.3.0" "https://github.com/B" NimbleDownloadMethodGit "b" [PackageName "A"]
    , NimPackage (PackageName "C") "0.4.3" "https://github.com/C" NimbleDownloadMethodGit "c" [PackageName "B", PackageName "A"]
    ]

mkDep :: Text -> Text -> Dependency
mkDep name version = Dependency GitType name (Just $ CEq version) [] mempty mempty

pkgA :: Dependency
pkgA = mkDep "https://github.com/A" "a"

pkgB :: Dependency
pkgB = mkDep "https://github.com/B" "b"

pkgC :: Dependency
pkgC = mkDep "https://github.com/C" "c"

spec :: Spec
spec = do
  testLockFile <- runIO $ decodeFileStrict' "test/Nim/testdata/nimble.lock"
  testNimbleDumpJsonOutput <- runIO $ decodeFileStrict' "test/Nim/testdata/nimble.dump.json.stdout"

  describe "nimble dump --json" $ do
    it "should parse output" $ do
      testNimbleDumpJsonOutput `shouldBe` Just expectedNimbleDumpContent

  describe "nimble.lock file" $ do
    it "should parse file" $ do
      testLockFile `shouldBe` Just expectedResolvedContent

  describe "buildGraph" $ do
    describe "when direct deps listings are provided" $ do
      let graph = buildGraph expectedResolvedContent $ Just $ NimbleDump [NimbleRequire $ PackageName "C"]

      it "should include all supported dependencies" $ do
        expectEdges [(pkgC, pkgB), (pkgC, pkgA), (pkgB, pkgA)] graph
        expectDeps [pkgC, pkgB, pkgA] graph

      it "should report direct dependencies" $ do
        expectDirect [pkgC] graph

    describe "when direct deps listings are not provided" $ do
      let graph' = buildGraph expectedResolvedContent Nothing

      it "should include all supported dependencies" $ do
        expectEdges [(pkgC, pkgB), (pkgC, pkgA), (pkgB, pkgA)] graph'
        expectDeps [pkgC, pkgB, pkgA] graph'

      it "should infer direct dependencies" $ do
        expectDirect [pkgC] graph'
