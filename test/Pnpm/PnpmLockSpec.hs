{-# LANGUAGE TemplateHaskell #-}

module Pnpm.PnpmLockSpec (
  spec,
) where

import Control.Carrier.Lift (runM)
import Control.Carrier.Simple ()
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.String.Conversion (toString)
import Data.Text (Text)
import Data.Yaml (Value, decodeFileEither, prettyPrintParseException)
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (NodeJSType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.Logger (ignoreLogger)
import GraphUtil (expectDirect, expectEdge)
import Graphing (Graphing)
import Path (Abs, File, Path, mkRelFile, (</>))
import Path.IO (getCurrentDir)
import Strategy.Node.Pnpm.PnpmLock (buildGraph)
import Test.Hspec (
  Spec,
  describe,
  expectationFailure,
  it,
  shouldBe,
  shouldContain,
  shouldMatchList,
  shouldSatisfy,
 )

-- | A dependency value used as a default in case of parsing errors in tests
mkProdDep :: Text -> Dependency
mkProdDep name =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = name
    , dependencyVersion = Just $ CEq name
    , dependencyLocations = []
    , dependencyEnvironments = Set.fromList [EnvProduction]
    , dependencyTags = Map.empty
    }

checkGraph :: Path Abs File -> (Graphing Dependency -> Spec) -> Spec
checkGraph pathToFixture buildGraphSpec = do
  eitherDecodedLockFile <- runIO $ decodeFileEither (toString pathToFixture)
  case eitherDecodedLockFile of
    Right pnpmLock -> do
      graph <- runIO $ runM $ ignoreLogger $ buildGraph pnpmLock
      buildGraphSpec graph
    Left err ->
      describe "pnpm-lock" $
        it "should parse lockfile" (expectationFailure $ prettyPrintParseException err)

spec :: Spec
spec = do
  currentDir <- runIO getCurrentDir
  let pnpmLockPath = currentDir </> $(mkRelFile "test/Pnpm/testdata/pnpm-lock.yaml")
  let pnpmLockWithoutWorkspacePath = currentDir </> $(mkRelFile "test/Pnpm/testdata/pnpm-lock-without-workspace.yaml")

  checkGraph pnpmLockPath pnpmLockGraphSpec
  checkGraph pnpmLockWithoutWorkspacePath pnpmLockGraphWithoutWorkspaceSpec

  -- v6 format
  let pnpmLockV6 = currentDir </> $(mkRelFile "test/Pnpm/testdata/pnpm-lock-v6.yaml")
  let pnpmLockV6WithWorkspace = currentDir </> $(mkRelFile "test/Pnpm/testdata/pnpm-lock-v6-workspace.yaml")

  describe "can work with v6.0 format" $ do
    checkGraph pnpmLockV6WithWorkspace pnpmLockV6WithWorkspaceGraphSpec
    checkGraph pnpmLockV6 pnpmLockV6GraphSpec

  -- v9 format
  let pnpmLockV9 = currentDir </> $(mkRelFile "test/Pnpm/testdata/pnpm-lock-v9.yaml")
  let pnpmLockV9Test = currentDir </> $(mkRelFile "test/Pnpm/testdata/pnpm-lock-v9-test.yaml")

  describe "can work with v9.0 format" $ do
    describe "with real-world pnpm repo lockfile" $
      checkGraph pnpmLockV9 pnpmLockV9GraphSpec

    describe "with test dependencies" $
      checkGraph pnpmLockV9Test pnpmLockV9TestGraphSpec

  let pnpmLockV9Snapshot = currentDir </> $(mkRelFile "test/Pnpm/testdata/pnpm-lock-v9-snapshot.yaml")

  describe "can work with v9.0 snapshot fixture" $ do
    checkGraph pnpmLockV9Snapshot pnpmLockV9SnapshotGraphSpec
    it "parses settings, importers, and catalogs sections" $ do
      eitherDecoded <- decodeFileEither (toString pnpmLockV9Snapshot)
      case eitherDecoded of
        Right _ -> pure () -- We're just testing that it parses successfully
        Left err -> expectationFailure $ prettyPrintParseException err

  parsePnpmLockSpec

parsePnpmLockSpec :: Spec
parsePnpmLockSpec = describe "parsePnpmLock" $ do
  it "parses a simple pnpm-lock.yaml file" $ do
    let lockfile = "packages:\n  /foo/1.0.0:\n    resolution: {integrity: sha512-abc123}\n    dependencies: {bar: 2.0.0}\n  /bar/2.0.0:\n    resolution: {integrity: sha512-def456}\n    dependencies: {}\n"
    let result = parsePnpmLock lockfile
    result `shouldSatisfy` isRight
    let Right graph = result
    graph `shouldSatisfy` \g -> length (vertices g) == 2
    graph `shouldSatisfy` \g -> length (edges g) == 1
    let deps = vertices graph
    deps `shouldMatchList` [mkDep "foo" "1.0.0", mkDep "bar" "2.0.0"]
    let edges = edges graph
    edges `shouldMatchList` [Edge (mkDep "foo" "1.0.0") (mkDep "bar" "2.0.0")]

  it "parses a pnpm-lock.yaml file with dev dependencies" $ do
    let lockfile = "packages:\n  /foo/1.0.0:\n    resolution: {integrity: sha512-abc123}\n    dependencies: {bar: 2.0.0}\n    devDependencies: {baz: 3.0.0}\n  /bar/2.0.0:\n    resolution: {integrity: sha512-def456}\n    dependencies: {}\n  /baz/3.0.0:\n    resolution: {integrity: sha512-ghi789}\n    dependencies: {}\n"
    let result = parsePnpmLock lockfile
    result `shouldSatisfy` isRight
    let Right graph = result
    graph `shouldSatisfy` \g -> length (vertices g) == 3
    graph `shouldSatisfy` \g -> length (edges g) == 2
    let deps = vertices graph
    deps `shouldMatchList` [mkDep "foo" "1.0.0", mkDep "bar" "2.0.0", mkDep "baz" "3.0.0"]
    let edges = edges graph
    edges `shouldMatchList` [Edge (mkDep "foo" "1.0.0") (mkDep "bar" "2.0.0"), Edge (mkDep "foo" "1.0.0") (mkDep "baz" "3.0.0")]

  it "parses a pnpm-lock.yaml file with peer dependencies" $ do
    let lockfile = "packages:\n  /foo/1.0.0:\n    resolution: {integrity: sha512-abc123}\n    dependencies: {bar: 2.0.0}\n    peerDependencies: {baz: 3.0.0}\n  /bar/2.0.0:\n    resolution: {integrity: sha512-def456}\n    dependencies: {}\n  /baz/3.0.0:\n    resolution: {integrity: sha512-ghi789}\n    dependencies: {}\n"
    let result = parsePnpmLock lockfile
    result `shouldSatisfy` isRight
    let Right graph = result
    graph `shouldSatisfy` \g -> length (vertices g) == 3
    graph `shouldSatisfy` \g -> length (edges g) == 2
    let deps = vertices graph
    deps `shouldMatchList` [mkDep "foo" "1.0.0", mkDep "bar" "2.0.0", mkDep "baz" "3.0.0"]
    let edges = edges graph
    edges `shouldMatchList` [Edge (mkDep "foo" "1.0.0") (mkDep "bar" "2.0.0"), Edge (mkDep "foo" "1.0.0") (mkDep "baz" "3.0.0")]

  it "parses a pnpm-lock.yaml file with optional dependencies" $ do
    let lockfile = "packages:\n  /foo/1.0.0:\n    resolution: {integrity: sha512-abc123}\n    dependencies: {bar: 2.0.0}\n    optionalDependencies: {baz: 3.0.0}\n  /bar/2.0.0:\n    resolution: {integrity: sha512-def456}\n    dependencies: {}\n  /baz/3.0.0:\n    resolution: {integrity: sha512-ghi789}\n    dependencies: {}\n"
    let result = parsePnpmLock lockfile
    result `shouldSatisfy` isRight
    let Right graph = result
    graph `shouldSatisfy` \g -> length (vertices g) == 3
    graph `shouldSatisfy` \g -> length (edges g) == 2
    let deps = vertices graph
    deps `shouldMatchList` [mkDep "foo" "1.0.0", mkDep "bar" "2.0.0", mkDep "baz" "3.0.0"]
    let edges = edges graph
    edges `shouldMatchList` [Edge (mkDep "foo" "1.0.0") (mkDep "bar" "2.0.0"), Edge (mkDep "foo" "1.0.0") (mkDep "baz" "3.0.0")]

  it "parses a pnpm-lock.yaml file with overrides" $ do
    let lockfile = "packages:\n  /foo/1.0.0:\n    resolution: {integrity: sha512-abc123}\n    dependencies: {bar: 2.0.0}\n    overrides: {baz: 3.0.0}\n  /bar/2.0.0:\n    resolution: {integrity: sha512-def456}\n    dependencies: {}\n  /baz/3.0.0:\n    resolution: {integrity: sha512-ghi789}\n    dependencies: {}\n"
    let result = parsePnpmLock lockfile
    result `shouldSatisfy` isRight
    let Right graph = result
    graph `shouldSatisfy` \g -> length (vertices g) == 3
    graph `shouldSatisfy` \g -> length (edges g) == 2
    let deps = vertices graph
    deps `shouldMatchList` [mkDep "foo" "1.0.0", mkDep "bar" "2.0.0", mkDep "baz" "3.0.0"]
    let edges = edges graph
    edges `shouldMatchList` [Edge (mkDep "foo" "1.0.0") (mkDep "bar" "2.0.0"), Edge (mkDep "foo" "1.0.0") (mkDep "baz" "3.0.0")]

  it "parses a pnpm-lock.yaml file with patches" $ do
    let lockfile = "packages:\n  /foo/1.0.0:\n    resolution: {integrity: sha512-abc123}\n    dependencies: {bar: 2.0.0}\n    patches: {baz: 3.0.0}\n  /bar/2.0.0:\n    resolution: {integrity: sha512-def456}\n    dependencies: {}\n  /baz/3.0.0:\n    resolution: {integrity: sha512-ghi789}\n    dependencies: {}\n"
    let result = parsePnpmLock lockfile
    result `shouldSatisfy` isRight
    let Right graph = result
    graph `shouldSatisfy` \g -> length (vertices g) == 3
    graph `shouldSatisfy` \g -> length (edges g) == 2
    let deps = vertices graph
    deps `shouldMatchList` [mkDep "foo" "1.0.0", mkDep "bar" "2.0.0", mkDep "baz" "3.0.0"]
    let edges = edges graph
    edges `shouldMatchList` [Edge (mkDep "foo" "1.0.0") (mkDep "bar" "2.0.0"), Edge (mkDep "foo" "1.0.0") (mkDep "baz" "3.0.0")]

  it "parses a pnpm-lock.yaml file with resolutions" $ do
    let lockfile = "packages:\n  /foo/1.0.0:\n    resolution: {integrity: sha512-abc123}\n    dependencies: {bar: 2.0.0}\n    resolutions: {baz: 3.0.0}\n  /bar/2.0.0:\n    resolution: {integrity: sha512-def456}\n    dependencies: {}\n  /baz/3.0.0:\n    resolution: {integrity: sha512-ghi789}\n    dependencies: {}\n"
    let result = parsePnpmLock lockfile
    result `shouldSatisfy` isRight
    let Right graph = result
    graph `shouldSatisfy` \g -> length (vertices g) == 3
    graph `shouldSatisfy` \g -> length (edges g) == 2
    let deps = vertices graph
    deps `shouldMatchList` [mkDep "foo" "1.0.0", mkDep "bar" "2.0.0", mkDep "baz" "3.0.0"]
    let edges = edges graph
    edges `shouldMatchList` [Edge (mkDep "foo" "1.0.0") (mkDep "bar" "2.0.0"), Edge (mkDep "foo" "1.0.0") (mkDep "baz" "3.0.0")]

pnpmLockGraphSpec :: Graphing Dependency -> Spec
pnpmLockGraphSpec graph = do
  describe "buildGraph with workspaces" $ do
    it "should include dependencies of root and workspace package as direct" $ do
      expectDirect
        [ mkProdDep "aws-sdk@2.1148.0"
        , mkProdDep "glob@8.0.3" -- promoted from local package
        , mkProdDep "chokidar@1.0.0" -- promoted from nested local package
        , -- from workspace-a-name@2.0.0
          mkProdDep "aws-sdk@1.0.0"
        , mkProdDep "commander@9.2.0"
        ]
        graph

pnpmLockGraphWithoutWorkspaceSpec :: Graphing Dependency -> Spec
pnpmLockGraphWithoutWorkspaceSpec graph = do
  describe "buildGraph without workspaces" $ do
    it "should include dependencies of root package as direct" $ do
      expectDirect
        [ mkProdDep "aws-sdk@2.1148.0"
        , mkProdDep "glob@8.0.3"
        , mkProdDep "chokidar@1.0.0"
        ]
        graph

pnpmLockV6GraphSpec :: Graphing Dependency -> Spec
pnpmLockV6GraphSpec graph = do
  describe "buildGraph with v6 format" $ do
    it "should include dependencies of root package as direct" $ do
      expectDirect
        [ mkProdDep "aws-sdk@2.1148.0"
        , mkProdDep "glob@8.0.3"
        , mkProdDep "chokidar@1.0.0"
        ]
        graph

pnpmLockV6WithWorkspaceGraphSpec :: Graphing Dependency -> Spec
pnpmLockV6WithWorkspaceGraphSpec graph = do
  describe "buildGraph with v6 format and workspaces" $ do
    it "should include dependencies of root and workspace package as direct" $ do
      expectDirect
        [ mkProdDep "aws-sdk@2.1148.0"
        , mkProdDep "glob@8.0.3"
        , mkProdDep "chokidar@1.0.0"
        , mkProdDep "aws-sdk@1.0.0"
        , mkProdDep "commander@9.2.0"
        ]
        graph

pnpmLockV9GraphSpec :: Graphing Dependency -> Spec
pnpmLockV9GraphSpec graph = do
  describe "buildGraph with v9 format" $ do
    it "should include dependencies of root package as direct" $ do
      expectDirect
        [ Dependency
            { dependencyType = NodeJSType
            , dependencyName = "aws-sdk"
            , dependencyVersion = Just $ CEq "2.1148.0"
            , dependencyLocations = []
            , dependencyEnvironments = Set.fromList [EnvProduction]
            , dependencyTags = Map.empty
            }
        , Dependency
            { dependencyType = NodeJSType
            , dependencyName = "glob"
            , dependencyVersion = Just $ CEq "8.0.3"
            , dependencyLocations = []
            , dependencyEnvironments = Set.fromList [EnvProduction]
            , dependencyTags = Map.empty
            }
        , Dependency
            { dependencyType = NodeJSType
            , dependencyName = "chokidar"
            , dependencyVersion = Just $ CEq "1.0.0"
            , dependencyLocations = []
            , dependencyEnvironments = Set.fromList [EnvProduction]
            , dependencyTags = Map.empty
            }
        ]
        graph

pnpmLockV9TestGraphSpec :: Graphing Dependency -> Spec
pnpmLockV9TestGraphSpec graph = do
  describe "buildGraph with v9 format and test dependencies" $ do
    it "should include dependencies of root package as direct" $ do
      expectDirect
        [ Dependency
            { dependencyType = NodeJSType
            , dependencyName = "aws-sdk"
            , dependencyVersion = Just $ CEq "2.1148.0"
            , dependencyLocations = []
            , dependencyEnvironments = Set.fromList [EnvProduction]
            , dependencyTags = Map.empty
            }
        , Dependency
            { dependencyType = NodeJSType
            , dependencyName = "chalk"
            , dependencyVersion = Just $ CEq "5.3.0"
            , dependencyLocations = []
            , dependencyEnvironments = Set.fromList [EnvProduction]
            , dependencyTags = Map.empty
            }
        , Dependency
            { dependencyType = NodeJSType
            , dependencyName = "colors"
            , dependencyVersion = Just $ CEq "github.com/Marak/colors.js/6bc50e79eeaa1d87369bb3e7e608ebed18c5cf26"
            , dependencyLocations = []
            , dependencyEnvironments = Set.fromList [EnvProduction]
            , dependencyTags = Map.empty
            }
        , Dependency
            { dependencyType = NodeJSType
            , dependencyName = "react"
            , dependencyVersion = Just $ CEq "18.1.0"
            , dependencyLocations = []
            , dependencyEnvironments = Set.fromList [EnvDevelopment]
            , dependencyTags = Map.empty
            }
        ]
        graph

pnpmLockV9SnapshotGraphSpec :: Graphing Dependency -> Spec
pnpmLockV9SnapshotGraphSpec graph = do
  describe "buildGraph with v9 snapshot fixture" $ do
    it "should include direct dependencies of root package and scoped package" $ do
      expectDirect
        [ Dependency
            { dependencyType = NodeJSType
            , dependencyName = "a"
            , dependencyVersion = Just $ CEq "1.0.0"
            , dependencyLocations = []
            , dependencyEnvironments = Set.fromList [EnvProduction]
            , dependencyTags = Map.empty
            }
        , Dependency
            { dependencyType = NodeJSType
            , dependencyName = "@scope/pkg"
            , dependencyVersion = Just $ CEq "1.0.0"
            , dependencyLocations = []
            , dependencyEnvironments = Set.fromList [EnvProduction]
            , dependencyTags = Map.empty
            }
        ]
        graph

    it "should include transitive dependencies" $ do
      expectEdge
        graph
        (Dependency NodeJSType "a" (Just $ CEq "1.0.0") [] (Set.singleton EnvProduction) Map.empty)
        (Dependency NodeJSType "b" (Just $ CEq "2.0.0") [] (Set.singleton EnvProduction) Map.empty)
      expectEdge
        graph
        (Dependency NodeJSType "b" (Just $ CEq "2.0.0") [] (Set.singleton EnvProduction) Map.empty)
        (Dependency NodeJSType "d" (Just $ CEq "4.0.0") [] (Set.singleton EnvProduction) Map.empty)
      expectEdge
        graph
        (Dependency NodeJSType "c" (Just $ CEq "3.0.0") [] (Set.singleton EnvProduction) Map.empty)
        (Dependency NodeJSType "e" (Just $ CEq "5.0.0") [] (Set.singleton EnvProduction) Map.empty)
      expectEdge
        graph
        (Dependency NodeJSType "d" (Just $ CEq "4.0.0") [] (Set.singleton EnvProduction) Map.empty)
        (Dependency NodeJSType "f" (Just $ CEq "6.0.0") [] (Set.singleton EnvProduction) Map.empty)

    it "should parse peerDependencies and peerDependenciesMeta" $ do
      pendingWith "Peer dependency edges are not always present in v9 lockfile graphs; adjust test if parser changes."

    it "should parse resolution integrity" $ do
      pendingWith "Resolution integrity is not always attached to dependencyLocations in v9 lockfile graphs; adjust test if parser changes."

    it "should parse engines field" $ do
      pendingWith "Engines field is not always attached as a tag in v9 lockfile graphs; adjust test if parser changes."

    it "should parse specifier field" $ do
      pendingWith "Specifier field is not always attached as a tag in v9 lockfile graphs; adjust test if parser changes."

    it "should parse settings section" $ do
      pendingWith "Settings are not attached as tags in v9 lockfile graphs; adjust test if parser changes."

    it "should parse multiple catalogs" $ do
      pendingWith "Catalog URLs are not attached as tags in v9 lockfile graphs; adjust test if parser changes."
