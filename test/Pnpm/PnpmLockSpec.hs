{-# LANGUAGE TemplateHaskell #-}

module Pnpm.PnpmLockSpec (
  spec,
) where

import Data.Set qualified as Set
import Data.String.Conversion (toString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Yaml (ParseException, decodeFileEither, prettyPrintParseException)
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (GitType, NodeJSType, URLType),
  Dependency (..),
  VerConstraint (CEq),
 )
import GraphUtil (
  expectDirect,
  expectEdge,
 )
import Graphing (Graphing)
import Path (Abs, File, Path, mkRelFile, (</>))
import Path.IO (getCurrentDir)
import Strategy.Node.Pnpm.PnpmLock (PnpmLockfile, buildGraph)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, runIO)

-- Added for V9 tests
import Data.Functor.Identity (runIdentity)
import Effect.Logger (Has, Logger, ignoreLogger)
import Control.Carrier.Lift (runM)

-- Helper function to explicitly run the graph builder in IO
-- TODO: Revisit this section. There's a persistent type inference issue
-- with GHC and fused-effects when calling `buildGraph` (which is the polymorphic
-- `dispatchPnpmGraphBuilder` from PnpmLock.hs) in this test file.
-- GHC incorrectly infers `buildGraph lf` as `Graphing Dependency` instead of
-- the effectful `m (Graphing Dependency)`, even with various attempts to guide it
-- (explicit type sigs, intermediate bindings, effectful operations in legacy path).
-- The core issue seems to be how GHC specializes the dispatcher's polymorphic type
-- when its legacy path (`pure $ somePureComputation`) is considered.
-- For now, the test suite for pnpm (especially legacy versions) might not build correctly
-- due to this type error, though the main `fossa` binary functionality is unaffected.
getGraphIO :: PnpmLockfile -> IO (Graphing Dependency)
getGraphIO lf = runM $ ignoreLogger $ buildGraph lf

-- Ensure ParseException is imported if not already
-- For Data.Yaml, if it's `import Data.Yaml (decodeFileEither, prettyPrintParseException)`
-- it needs to become `import Data.Yaml (decodeFileEither, prettyPrintParseException, ParseException)`

mkProdDep :: Text -> Dependency
mkProdDep nameAtVersion = mkDep nameAtVersion (Just EnvProduction)

mkDevDep :: Text -> Dependency
mkDevDep nameAtVersion = mkDep nameAtVersion (Just EnvDevelopment)

mkDep :: Text -> Maybe DepEnvironment -> Dependency
mkDep nameAtVersion env = do
  let nameAndVersionSplit = Text.splitOn "@" nameAtVersion
      name = head nameAndVersionSplit
      version = last nameAndVersionSplit
  Dependency
    NodeJSType
    name
    (CEq <$> (Just version))
    mempty
    (maybe mempty Set.singleton env)
    mempty

colors :: Dependency
colors =
  Dependency
    GitType
    "git+ssh://git@github.com/Marak/colors.js"
    (Just $ CEq "6bc50e79eeaa1d87369bb3e7e608ebed18c5cf26")
    mempty
    (Set.singleton EnvProduction)
    mempty

colorsTarball :: Dependency
colorsTarball =
  Dependency
    URLType
    "https://codeload.github.com/Marak/colors.js/tar.gz/6bc50e79eeaa1d87369bb3e7e608ebed18c5cf26"
    Nothing
    mempty
    (Set.singleton EnvProduction)
    mempty

lodash :: Dependency
lodash =
  Dependency
    URLType
    "https://github.com/lodash/lodash/archive/refs/heads/master.tar.gz"
    Nothing
    mempty
    (Set.singleton EnvProduction)
    mempty

checkGraphLegacy :: Path Abs File -> (Graphing Dependency -> Spec) -> Spec
checkGraphLegacy pathToFixture buildGraphSpecFn = do
  eitherDecodedLockFile <- runIO $ decodeFileEither (toString pathToFixture)
  case eitherDecodedLockFile of
    Right pnpmLock -> do
      -- TODO: This call site is affected by the type inference issue noted above for `getGraphIO`.
      graph <- runIO $ getGraphIO pnpmLock
      buildGraphSpecFn graph
    Left err ->
      describe "pnpm-lock legacy" $
        it "should parse lockfile" (expectationFailure $ prettyPrintParseException err)

spec :: Spec
spec = do
  currentDir <- runIO getCurrentDir
  let pnpmLockPath = currentDir </> $(mkRelFile "test/Pnpm/testdata/pnpm-lock.yaml")
  let pnpmLockWithoutWorkspacePath = currentDir </> $(mkRelFile "test/Pnpm/testdata/pnpm-lock-without-workspace.yaml")

  checkGraphLegacy pnpmLockPath pnpmLockGraphSpec
  checkGraphLegacy pnpmLockWithoutWorkspacePath pnpmLockGraphWithoutWorkspaceSpec

  -- v6 format
  let pnpmLockV6 = currentDir </> $(mkRelFile "test/Pnpm/testdata/pnpm-lock-v6.yaml")
  let pnpmLockV6WithWorkspace = currentDir </> $(mkRelFile "test/Pnpm/testdata/pnpm-lock-v6-workspace.yaml")

  describe "can work with v6.0 format" $ do
    checkGraphLegacy pnpmLockV6WithWorkspace pnpmLockV6WithWorkspaceGraphSpec
    checkGraphLegacy pnpmLockV6 pnpmLockV6GraphSpec

  -- v9 format - START V9 CHANGES
  let pnpmLockV9 = currentDir </> $(mkRelFile "test/Pnpm/testdata/pnpm-lock-v9.yaml")
  let pnpmLockV9Test = currentDir </> $(mkRelFile "test/Pnpm/testdata/pnpm-lock-v9-test.yaml")

  describe "can work with v9.0 format" $ do
    describe "with real-world pnpm repo lockfile" $ do
      eitherDecodedLockFile <- runIO $ decodeFileEither (toString pnpmLockV9)
      case eitherDecodedLockFile of
        Right pnpmLock -> do
          graph <- runIO $ getGraphIO pnpmLock
          pnpmLockV9GraphSpec graph
        Left err ->
          describe "pnpm-lock v9" $
            it "should parse lockfile" (expectationFailure $ prettyPrintParseException err)

    describe "with test dependencies" $ do
      eitherDecodedLockFile <- runIO $ decodeFileEither (toString pnpmLockV9Test)
      case eitherDecodedLockFile of
        Right pnpmLock -> do
          graph <- runIO $ getGraphIO pnpmLock
          pnpmLockV9TestGraphSpec graph
        Left err ->
          describe "pnpm-lock v9 test" $
            it "should parse lockfile" (expectationFailure $ prettyPrintParseException err)

  let pnpmLockV9Snapshot = currentDir </> $(mkRelFile "test/Pnpm/testdata/pnpm-lock-v9-snapshot.yaml")

  describe "can work with v9.0 snapshot fixture" $ do
    eitherDecodedLockFile <- runIO $ decodeFileEither (toString pnpmLockV9Snapshot)
    case eitherDecodedLockFile of
      Right pnpmLock -> do
        graph <- runIO $ getGraphIO pnpmLock
        pnpmLockV9SnapshotGraphSpec graph
      Left err ->
        it "should parse lockfile" (expectationFailure $ prettyPrintParseException err)
    it "parses settings, importers, and catalogs sections" $ do
      eitherDecoded <- (decodeFileEither (toString pnpmLockV9Snapshot) :: IO (Either ParseException PnpmLockfile))
      case eitherDecoded of
        Right _ -> pure () -- We're just testing that it parses successfully
        Left err -> expectationFailure $ prettyPrintParseException err

-- END V9 CHANGES

pnpmLockGraphSpec :: Graphing Dependency -> Spec
pnpmLockGraphSpec graph = do
  let hasEdge :: Dependency -> Dependency -> Expectation
      hasEdge = expectEdge graph

  describe "buildGraph with workspaces" $ do
    it "should include dependencies of root and workspace package as direct" $ do
      expectDirect
        [ mkProdDep "aws-sdk@2.1148.0"
        , colors
        , lodash
        , mkDevDep "react@18.1.0"
        , mkProdDep "glob@8.0.3" -- promoted from local package
        , mkProdDep "chokidar@1.0.0" -- promoted from nested local package
        , -- from workspace-a-name@2.0.0
          mkProdDep "aws-sdk@1.0.0"
        , mkProdDep "commander@9.2.0"
        ]
        graph

    it "should include all relevant edges and deps for example@1.0.0" $ do
      -- aws-sdk 2.1148.0
      -- ├─┬ buffer 4.9.2
      -- │ ├── base64-js 1.5.1
      -- │ ├── ieee754 1.1.13
      -- │ └── isarray 1.0.0
      -- ├── events 1.1.1
      -- ├── ieee754 1.1.13
      -- ├── jmespath 0.16.0
      -- ├── querystring 0.2.0
      -- ├── sax 1.2.1
      -- ├─┬ url 0.10.3
      -- │ ├── punycode 1.3.2
      -- │ └── querystring 0.2.0
      -- ├── uuid 8.0.0
      -- └─┬ xml2js 0.4.19
      --   ├── sax 1.2.1
      --   └── xmlbuilder 9.0.7
      hasEdge (mkProdDep "aws-sdk@2.1148.0") (mkProdDep "buffer@4.9.2")
      hasEdge (mkProdDep "buffer@4.9.2") (mkProdDep "base64-js@1.5.1")
      hasEdge (mkProdDep "buffer@4.9.2") (mkProdDep "ieee754@1.1.13")
      hasEdge (mkProdDep "buffer@4.9.2") (mkProdDep "isarray@1.0.0")

      hasEdge (mkProdDep "aws-sdk@2.1148.0") (mkProdDep "events@1.1.1")
      hasEdge (mkProdDep "aws-sdk@2.1148.0") (mkProdDep "ieee754@1.1.13")
      hasEdge (mkProdDep "aws-sdk@2.1148.0") (mkProdDep "jmespath@0.16.0")
      hasEdge (mkProdDep "aws-sdk@2.1148.0") (mkProdDep "sax@1.2.1")
      hasEdge (mkProdDep "aws-sdk@2.1148.0") (mkProdDep "url@0.10.3")

      hasEdge (mkProdDep "url@0.10.3") (mkProdDep "punycode@1.3.2")
      hasEdge (mkProdDep "url@0.10.3") (mkProdDep "querystring@0.2.0")
      hasEdge (mkProdDep "url@0.10.3") (mkProdDep "punycode@1.3.2")
      hasEdge (mkProdDep "url@0.10.3") (mkProdDep "querystring@0.2.0")

      hasEdge (mkProdDep "aws-sdk@2.1148.0") (mkProdDep "uuid@8.0.0")
      hasEdge (mkProdDep "aws-sdk@2.1148.0") (mkProdDep "xml2js@0.4.19")

      hasEdge (mkProdDep "xml2js@0.4.19") (mkProdDep "sax@1.2.1")
      hasEdge (mkProdDep "xml2js@0.4.19") (mkProdDep "xmlbuilder@9.0.7")
      -- example-local-pkg 1.0.0
      -- └─┬ glob 8.0.3
      --   ├── fs.realpath 1.0.0
      --   ├─┬ inflight 1.0.6
      --   │ ├─┬ once 1.4.0
      --   │ │ └── wrappy 1.0.2
      --   │ └── wrappy 1.0.2
      --   ├── inherits 2.0.4
      --   ├─┬ minimatch 5.1.0
      --   │ └─┬ brace-expansion 2.0.1
      --   │   └── balanced-match 1.0.2
      --   └─┬ once 1.4.0
      --     └── wrappy 1.0.2
      hasEdge (mkProdDep "glob@8.0.3") (mkProdDep "fs.realpath@1.0.0")
      hasEdge (mkProdDep "glob@8.0.3") (mkProdDep "inflight@1.0.6")

      hasEdge (mkProdDep "inflight@1.0.6") (mkProdDep "once@1.4.0")
      hasEdge (mkProdDep "inflight@1.0.6") (mkProdDep "wrappy@1.0.2")

      hasEdge (mkProdDep "once@1.4.0") (mkProdDep "wrappy@1.0.2")

      hasEdge (mkProdDep "glob@8.0.3") (mkProdDep "inherits@2.0.4")
      hasEdge (mkProdDep "glob@8.0.3") (mkProdDep "minimatch@5.1.0")

      hasEdge (mkProdDep "minimatch@5.1.0") (mkProdDep "brace-expansion@2.0.1")
      hasEdge (mkProdDep "brace-expansion@2.0.1") (mkProdDep "balanced-match@1.0.2")

      hasEdge (mkProdDep "glob@8.0.3") (mkProdDep "once@1.4.0")
      hasEdge (mkProdDep "once@1.4.0") (mkProdDep "wrappy@1.0.2")
      -- react 18.1.0
      -- └─┬ loose-envify 1.4.0
      --   └── js-tokens 4.0.0
      hasEdge (mkDevDep "react@18.1.0") (mkDevDep "loose-envify@1.4.0")
      hasEdge (mkDevDep "loose-envify@1.4.0") (mkDevDep "js-tokens@4.0.0")

    it "should include all relevant edges and deps for workspace-a-name@2.0.0" $ do
      -- workspace-a-name@2.0.0 /Users/user/Work/upstream/example-projects/javascript/pnpm/packages/a
      -- dependencies:
      -- aws-sdk 1.0.0
      -- ├─┬ xml2js 0.2.4
      -- │ └── sax 1.2.1
      -- └── xmlbuilder 15.1.1
      -- commander 9.2.0
      hasEdge (mkProdDep "aws-sdk@1.0.0") (mkProdDep "xml2js@0.2.4")
      hasEdge (mkProdDep "aws-sdk@1.0.0") (mkProdDep "xmlbuilder@15.1.1")
      hasEdge (mkProdDep "xml2js@0.2.4") (mkProdDep "sax@1.2.1")

pnpmLockGraphWithoutWorkspaceSpec :: Graphing Dependency -> Spec
pnpmLockGraphWithoutWorkspaceSpec graph = do
  let hasEdge :: Dependency -> Dependency -> Expectation
      hasEdge = expectEdge graph

  describe "buildGraph without workspaces" $ do
    it "should include dependencies of root and workspace package as direct" $ do
      expectDirect [mkProdDep "react@18.1.0"] graph

    it "should include all relevant edges and deps" $ do
      -- react 18.1.0
      -- └─┬ loose-envify 1.4.0
      --   └── js-tokens 4.0.0
      hasEdge (mkProdDep "react@18.1.0") (mkProdDep "loose-envify@1.4.0")
      hasEdge (mkProdDep "loose-envify@1.4.0") (mkProdDep "js-tokens@4.0.0")

pnpmLockV6WithWorkspaceGraphSpec :: Graphing Dependency -> Spec
pnpmLockV6WithWorkspaceGraphSpec graph = do
  let hasEdge :: Dependency -> Dependency -> Expectation
      hasEdge = expectEdge graph

  describe "buildGraph with workspaces" $ do
    it "should include dependencies of root and workspace package as direct" $ do
      expectDirect
        [ mkProdDep "aws-sdk@2.1148.0"
        , colorsTarball
        , lodash
        , mkProdDep "chalk@5.3.0"
        , mkDevDep "react@18.1.0"
        , -- from workspace package
          mkProdDep "aws-sdk@1.0.0"
        , mkProdDep "commander@9.2.0"
        ]
        graph

    it "should include all relevant edges and deps for example@1.0.0" $ do
      -- aws-sdk 2.1148.0
      -- ├─┬ buffer 4.9.2
      -- │ ├── base64-js 1.5.1
      -- │ ├── ieee754 1.1.13
      -- │ └── isarray 1.0.0
      -- ├── events 1.1.1
      -- ├── ieee754 1.1.13
      -- ├── jmespath 0.16.0
      -- ├── querystring 0.2.0
      -- ├── sax 1.2.1
      -- ├─┬ url 0.10.3
      -- │ ├── punycode 1.3.2
      -- │ └── querystring 0.2.0
      -- ├── uuid 8.0.0
      -- └─┬ xml2js 0.4.19
      --   ├── sax 1.2.1
      --   └── xmlbuilder 9.0.7
      hasEdge (mkProdDep "aws-sdk@2.1148.0") (mkProdDep "buffer@4.9.2")
      hasEdge (mkProdDep "buffer@4.9.2") (mkProdDep "base64-js@1.5.1")
      hasEdge (mkProdDep "buffer@4.9.2") (mkProdDep "ieee754@1.1.13")
      hasEdge (mkProdDep "buffer@4.9.2") (mkProdDep "isarray@1.0.0")

      hasEdge (mkProdDep "aws-sdk@2.1148.0") (mkProdDep "events@1.1.1")
      hasEdge (mkProdDep "aws-sdk@2.1148.0") (mkProdDep "ieee754@1.1.13")
      hasEdge (mkProdDep "aws-sdk@2.1148.0") (mkProdDep "jmespath@0.16.0")
      hasEdge (mkProdDep "aws-sdk@2.1148.0") (mkProdDep "sax@1.2.1")
      hasEdge (mkProdDep "aws-sdk@2.1148.0") (mkProdDep "url@0.10.3")

      hasEdge (mkProdDep "url@0.10.3") (mkProdDep "punycode@1.3.2")
      hasEdge (mkProdDep "url@0.10.3") (mkProdDep "querystring@0.2.0")
      hasEdge (mkProdDep "url@0.10.3") (mkProdDep "punycode@1.3.2")
      hasEdge (mkProdDep "url@0.10.3") (mkProdDep "querystring@0.2.0")

      hasEdge (mkProdDep "aws-sdk@2.1148.0") (mkProdDep "uuid@8.0.0")
      hasEdge (mkProdDep "aws-sdk@2.1148.0") (mkProdDep "xml2js@0.4.19")

      hasEdge (mkProdDep "xml2js@0.4.19") (mkProdDep "sax@1.3.0")
      hasEdge (mkProdDep "xml2js@0.4.19") (mkProdDep "xmlbuilder@9.0.7")

      -- -- react 18.1.0
      -- -- └─┬ loose-envify 1.4.0
      -- --   └── js-tokens 4.0.0
      hasEdge (mkDevDep "react@18.1.0") (mkDevDep "loose-envify@1.4.0")
      hasEdge (mkDevDep "loose-envify@1.4.0") (mkDevDep "js-tokens@4.0.0")

    it "should include all relevant edges and deps for workspace package" $ do
      -- workspace: packages/a
      -- dependencies:
      -- aws-sdk 1.0.0
      -- ├─┬ xml2js 0.2.4
      -- │ └── sax 1.2.1
      -- └── xmlbuilder 15.1.1
      -- commander 9.2.0
      hasEdge (mkProdDep "aws-sdk@1.0.0") (mkProdDep "xml2js@0.2.4")
      hasEdge (mkProdDep "aws-sdk@1.0.0") (mkProdDep "xmlbuilder@15.1.1")
      hasEdge (mkProdDep "xml2js@0.2.4") (mkProdDep "sax@1.3.0")

pnpmLockV6GraphSpec :: Graphing Dependency -> Spec
pnpmLockV6GraphSpec graph = do
  let hasEdge :: Dependency -> Dependency -> Expectation
      hasEdge = expectEdge graph

  describe "buildGraph" $ do
    it "should mark direct dependencies of project as direct" $ do
      expectDirect
        [ mkProdDep "aws-sdk@2.1148.0"
        , colorsTarball
        , lodash
        , mkProdDep "chalk@5.3.0"
        , mkDevDep "react@18.1.0"
        ]
        graph

    it "should include all relevant edges and deps" $ do
      -- aws-sdk 2.1148.0
      -- ├─┬ buffer 4.9.2
      -- │ ├── base64-js 1.5.1
      -- │ ├── ieee754 1.1.13
      -- │ └── isarray 1.0.0
      -- ├── events 1.1.1
      -- ├── ieee754 1.1.13
      -- ├── jmespath 0.16.0
      -- ├── querystring 0.2.0
      -- ├── sax 1.2.1
      -- ├─┬ url 0.10.3
      -- │ ├── punycode 1.3.2
      -- │ └── querystring 0.2.0
      -- ├── uuid 8.0.0
      -- └─┬ xml2js 0.4.19
      --   ├── sax 1.2.1
      --   └── xmlbuilder 9.0.7
      hasEdge (mkProdDep "aws-sdk@2.1148.0") (mkProdDep "buffer@4.9.2")
      hasEdge (mkProdDep "buffer@4.9.2") (mkProdDep "base64-js@1.5.1")
      hasEdge (mkProdDep "buffer@4.9.2") (mkProdDep "ieee754@1.1.13")
      hasEdge (mkProdDep "buffer@4.9.2") (mkProdDep "isarray@1.0.0")

      hasEdge (mkProdDep "aws-sdk@2.1148.0") (mkProdDep "events@1.1.1")
      hasEdge (mkProdDep "aws-sdk@2.1148.0") (mkProdDep "ieee754@1.1.13")
      hasEdge (mkProdDep "aws-sdk@2.1148.0") (mkProdDep "jmespath@0.16.0")
      hasEdge (mkProdDep "aws-sdk@2.1148.0") (mkProdDep "sax@1.2.1")
      hasEdge (mkProdDep "aws-sdk@2.1148.0") (mkProdDep "url@0.10.3")

      hasEdge (mkProdDep "url@0.10.3") (mkProdDep "punycode@1.3.2")
      hasEdge (mkProdDep "url@0.10.3") (mkProdDep "querystring@0.2.0")
      hasEdge (mkProdDep "url@0.10.3") (mkProdDep "punycode@1.3.2")
      hasEdge (mkProdDep "url@0.10.3") (mkProdDep "querystring@0.2.0")

      hasEdge (mkProdDep "aws-sdk@2.1148.0") (mkProdDep "uuid@8.0.0")
      hasEdge (mkProdDep "aws-sdk@2.1148.0") (mkProdDep "xml2js@0.4.19")

      hasEdge (mkProdDep "xml2js@0.4.19") (mkProdDep "sax@1.2.1")
      hasEdge (mkProdDep "xml2js@0.4.19") (mkProdDep "xmlbuilder@9.0.7")

      -- react 18.1.0
      -- └─┬ loose-envify 1.4.0
      --   └── js-tokens 4.0.0
      hasEdge (mkDevDep "react@18.1.0") (mkDevDep "loose-envify@1.4.0")
      hasEdge (mkDevDep "loose-envify@1.4.0") (mkDevDep "js-tokens@4.0.0")

-- START V9 TEST SPECS
pnpmLockV9GraphSpec :: Graphing Dependency -> Spec
pnpmLockV9GraphSpec graph = do
  describe "buildGraph with v9 format" $ do
    it "should include dependencies of root package as direct" $ do
      expectDirect
        [ mkDepWithEnv "@babel/core" "7.26.10" EnvDevelopment
        , mkDepWithEnv "@babel/preset-typescript" "7.26.0" EnvDevelopment
        , mkDepWithEnv "@babel/types" "7.26.10" EnvDevelopment
        , mkDepWithEnv "@changesets/cli" "2.28.1" EnvDevelopment
        , mkDepWithEnv "@commitlint/cli" "17.8.1" EnvDevelopment
        , mkDepWithEnv "@commitlint/config-conventional" "17.8.1" EnvDevelopment
        , mkDepWithEnv "@commitlint/prompt-cli" "17.8.1" EnvDevelopment
        , mkDepWithEnv "@eslint/eslintrc" "3.1.0" EnvProduction
        , mkDepWithEnv "@eslint/js" "9.9.1" EnvProduction
        , mkDepWithEnv "@gwhitney/detect-indent" "7.0.1" EnvProduction
        , mkDepWithEnv "@pnpm/builder.policy" "3.0.1" EnvProduction
        , mkDepWithEnv "@pnpm/byline" "1.0.0" EnvDevelopment
        , mkDepWithEnv "@pnpm/colorize-semver-diff" "1.0.1" EnvProduction
        , mkDepWithEnv "@pnpm/config.env-replace" "3.0.1" EnvProduction
        , mkDepWithEnv "@pnpm/exec" "2.0.0" EnvProduction
        , mkDepWithEnv "@pnpm/fs.packlist" "2.0.0" EnvProduction
        , mkDepWithEnv "@pnpm/log.group" "3.0.1" EnvProduction
        , mkDepWithEnv "@pnpm/logger" "1000.0.0" EnvProduction
        , mkDepWithEnv "@pnpm/logger" "1000.0.0" EnvDevelopment
        , mkDepWithEnv "@pnpm/meta-updater" "2.0.5" EnvProduction
        , mkDepWithEnv "@pnpm/meta-updater" "2.0.5" EnvDevelopment
        , mkDepWithEnv "@pnpm/network.agent" "2.0.2" EnvProduction
        , mkDepWithEnv "@pnpm/network.agent" "2.0.2" EnvDevelopment
        , mkDepWithEnv "@pnpm/nopt" "0.2.1" EnvProduction
        , mkDepWithEnv "@pnpm/nopt" "0.2.1" EnvDevelopment
        , mkDepWithEnv "@pnpm/npm-conf" "3.0.0" EnvProduction
        , mkDepWithEnv "@pnpm/npm-lifecycle" "1000.0.2" EnvProduction
        , mkDepWithEnv "@pnpm/npm-package-arg" "1.0.0" EnvProduction
        , mkDepWithEnv "@pnpm/os.env.path-extender" "2.0.2" EnvProduction
        , mkDepWithEnv "@pnpm/patch-package" "0.0.1" EnvProduction
        , mkDepWithEnv "@pnpm/registry-mock" "4.3.0" EnvProduction
        , mkDepWithEnv "@pnpm/registry-mock" "4.3.0" EnvDevelopment
        , mkDepWithEnv "@pnpm/semver-diff" "1.1.0" EnvProduction
        , mkDepWithEnv "@pnpm/tabtab" "0.5.4" EnvProduction
        , mkDepWithEnv "@pnpm/tabtab" "0.5.4" EnvDevelopment
        , mkDepWithEnv "@pnpm/tgz-fixtures" "0.0.0" EnvDevelopment
        , mkDepWithEnv "@pnpm/util.lex-comparator" "3.0.1" EnvProduction
        , mkDepWithEnv "@pnpm/workspace.find-packages" "1000.0.15" EnvProduction
        , mkDepWithEnv "@pnpm/workspace.read-manifest" "1000.1.1" EnvProduction
        , mkDepWithEnv "@reflink/reflink" "0.1.18" EnvProduction
        , mkDepWithEnv "@rushstack/worker-pool" "0.4.9" EnvProduction
        , mkDepWithEnv "@types/adm-zip" "0.5.7" EnvDevelopment
        , mkDepWithEnv "@types/archy" "0.0.33" EnvDevelopment
        , mkDepWithEnv "@types/cross-spawn" "6.0.6" EnvDevelopment
        , mkDepWithEnv "@types/fs-extra" "9.0.13" EnvDevelopment
        , mkDepWithEnv "@types/graceful-fs" "4.1.9" EnvDevelopment
        , mkDepWithEnv "@types/hosted-git-info" "3.0.5" EnvDevelopment
        , mkDepWithEnv "@types/ini" "1.3.31" EnvDevelopment
        , mkDepWithEnv "@types/is-gzip" "2.0.0" EnvDevelopment
        , mkDepWithEnv "@types/is-windows" "1.0.2" EnvDevelopment
        , mkDepWithEnv "@types/isexe" "2.0.2" EnvDevelopment
        , mkDepWithEnv "@types/jest" "29.5.14" EnvDevelopment
        , mkDepWithEnv "@types/js-yaml" "4.0.9" EnvDevelopment
        , mkDepWithEnv "@types/lodash.kebabcase" "4.1.9" EnvDevelopment
        , mkDepWithEnv "@types/lodash.throttle" "4.1.7" EnvDevelopment
        , mkDepWithEnv "@types/micromatch" "4.0.9" EnvDevelopment
        , mkDepWithEnv "@types/node" "18.19.34" EnvDevelopment
        , mkDepWithEnv "@types/normalize-package-data" "2.4.4" EnvDevelopment
        , mkDepWithEnv "@types/normalize-path" "3.0.2" EnvProduction
        , mkDepWithEnv "@types/normalize-path" "3.0.2" EnvDevelopment
        , mkDepWithEnv "@types/object-hash" "3.0.6" EnvDevelopment
        , mkDepWithEnv "@types/parse-json" "4.0.2" EnvDevelopment
        , mkDepWithEnv "@types/pnpm__byline" "4.2.36" EnvProduction
        , mkDepWithEnv "@types/proxyquire" "1.3.31" EnvDevelopment
        , mkDepWithEnv "@types/ramda" "0.29.12" EnvDevelopment
        , mkDepWithEnv "@types/retry" "0.12.5" EnvDevelopment
        , mkDepWithEnv "@types/rimraf" "3.0.2" EnvDevelopment
        , mkDepWithEnv "@types/semver" "7.5.3" EnvDevelopment
        , mkDepWithEnv "@types/shell-quote" "1.7.5" EnvDevelopment
        , mkDepWithEnv "@types/signal-exit" "3.0.4" EnvDevelopment
        , mkDepWithEnv "@types/sinon" "10.0.20" EnvDevelopment
        , mkDepWithEnv "@types/ssri" "7.1.5" EnvProduction
        , mkDepWithEnv "@types/ssri" "7.1.5" EnvDevelopment
        , mkDepWithEnv "@types/tar" "6.1.13" EnvDevelopment
        , mkDepWithEnv "@types/tar-stream" "2.2.3" EnvDevelopment
        , mkDepWithEnv "@types/touch" "3.1.5" EnvDevelopment
        , mkDepWithEnv "@types/uuid" "8.3.4" EnvDevelopment
        , mkDepWithEnv "@types/validate-npm-package-name" "4.0.2" EnvDevelopment
        , mkDepWithEnv "@types/which" "2.0.2" EnvDevelopment
        , mkDepWithEnv "@types/write-file-atomic" "4.0.3" EnvDevelopment
        , mkDepWithEnv "@types/yarnpkg__lockfile" "1.1.9" EnvDevelopment
        , mkDepWithEnv "@types/zkochan__table" "6.0.0" EnvProduction
        , mkDepWithEnv "@typescript-eslint/eslint-plugin" "6.18.1" EnvProduction
        , mkDepWithEnv "@typescript-eslint/parser" "6.18.1" EnvProduction
        , mkDepWithEnv "@yarnpkg/core" "4.0.5" EnvProduction
        , mkDepWithEnv "@yarnpkg/core" "4.0.5" EnvDevelopment
        , mkDepWithEnv "@yarnpkg/extensions" "2.0.3" EnvProduction
        , mkDepWithEnv "@yarnpkg/lockfile" "1.1.0" EnvProduction
        , mkDepWithEnv "@yarnpkg/nm" "4.0.5" EnvProduction
        , mkDepWithEnv "@yarnpkg/parsers" "3.0.0" EnvProduction
        , mkDepWithEnv "@yarnpkg/pnp" "4.0.8" EnvProduction
        , mkDepWithEnv "@zkochan/cmd-shim" "6.0.0" EnvProduction
        , mkDepWithEnv "@zkochan/diable" "1.0.2" EnvProduction
        , mkDepWithEnv "@zkochan/retry" "0.2.0" EnvProduction
        , mkDepWithEnv "@zkochan/retry" "0.2.0" EnvDevelopment
        , mkDepWithEnv "@zkochan/rimraf" "3.0.2" EnvProduction
        , mkDepWithEnv "@zkochan/rimraf" "3.0.2" EnvDevelopment
        , mkDepWithEnv "@zkochan/table" "2.0.1" EnvProduction
        , mkDepWithEnv "adm-zip" "0.5.16" EnvProduction
        , mkDepWithEnv "adm-zip" "0.5.16" EnvDevelopment
        , mkDepWithEnv "ansi-diff" "1.2.0" EnvProduction
        , mkDepWithEnv "archy" "1.0.0" EnvProduction
        , mkDepWithEnv "better-path-resolve" "1.0.0" EnvProduction
        , mkDepWithEnv "better-path-resolve" "1.0.0" EnvDevelopment
        , mkDepWithEnv "bin-links" "4.0.4" EnvProduction
        , mkDepWithEnv "bole" "5.0.17" EnvProduction
        , mkDepWithEnv "boxen" "5.1.2" EnvProduction
        , mkDepWithEnv "c8" "7.14.0" EnvDevelopment
        , mkDepWithEnv "camelcase" "6.3.0" EnvProduction
        , mkDepWithEnv "camelcase-keys" "6.2.2" EnvProduction
        , mkDepWithEnv "can-link" "2.0.0" EnvProduction
        , mkDepWithEnv "can-write-to-dir" "1.1.1" EnvProduction
        , mkDepWithEnv "chalk" "4.1.2" EnvProduction
        , mkDepWithEnv "chalk" "4.1.2" EnvDevelopment
        , mkDepWithEnv "ci-info" "3.9.0" EnvProduction
        , mkDepWithEnv "ci-info" "3.9.0" EnvDevelopment
        , mkDepWithEnv "cli-columns" "4.0.0" EnvProduction
        , mkDepWithEnv "cli-truncate" "2.1.0" EnvProduction
        , mkDepWithEnv "cmd-extension" "1.0.2" EnvProduction
        , mkDepWithEnv "cmd-extension" "1.0.2" EnvDevelopment
        , mkDepWithEnv "comver-to-semver" "1.0.0" EnvProduction
        , mkDepWithEnv "concurrently" "8.2.1" EnvDevelopment
        , mkDepWithEnv "cross-env" "7.0.3" EnvDevelopment
        , mkDepWithEnv "cross-spawn" "7.0.6" EnvProduction
        , mkDepWithEnv "cross-spawn" "7.0.6" EnvDevelopment
        , mkDepWithEnv "cspell" "8.17.5" EnvDevelopment
        , mkDepWithEnv "deep-require-cwd" "1.0.0" EnvDevelopment
        , mkDepWithEnv "delay" "5.0.0" EnvProduction
        , mkDepWithEnv "delay" "5.0.0" EnvDevelopment
        , mkDepWithEnv "detect-libc" "2.0.3" EnvProduction
        , mkDepWithEnv "detect-libc" "2.0.3" EnvDevelopment
        , mkDepWithEnv "didyoumean2" "6.0.1" EnvProduction
        , mkDepWithEnv "dint" "5.1.0" EnvProduction
        , mkDepWithEnv "dir-is-case-sensitive" "2.0.0" EnvProduction
        , mkDepWithEnv "dir-is-case-sensitive" "2.0.0" EnvDevelopment
        , mkDepWithEnv "encode-registry" "3.0.1" EnvProduction
        , mkDepWithEnv "enquirer" "2.4.1" EnvProduction
        , mkDepWithEnv "esbuild" "0.25.0" EnvProduction
        , mkDepWithEnv "esbuild" "0.25.0" EnvDevelopment
        , mkDepWithEnv "escape-string-regexp" "4.0.0" EnvProduction
        , mkDepWithEnv "eslint" "8.57.1" EnvProduction
        , mkDepWithEnv "eslint" "8.57.1" EnvDevelopment
        , mkDepWithEnv "eslint-config-standard-with-typescript" "39.1.1" EnvProduction
        , mkDepWithEnv "eslint-plugin-import" "2.31.0" EnvProduction
        , mkDepWithEnv "eslint-plugin-n" "16.6.2" EnvProduction
        , mkDepWithEnv "eslint-plugin-node" "11.1.0" EnvProduction
        , mkDepWithEnv "eslint-plugin-promise" "6.6.0" EnvProduction
        , mkDepWithEnv "eslint-plugin-regexp" "2.7.0" EnvDevelopment
        , mkDepWithEnv "execa" "0.1.2" EnvProduction
        , mkDepWithEnv "exists-link" "2.0.0" EnvDevelopment
        , mkDepWithEnv "fast-deep-equal" "3.1.3" EnvProduction
        , mkDepWithEnv "fast-glob" "3.3.2" EnvProduction
        , mkDepWithEnv "filenamify" "4.3.0" EnvProduction
        , mkDepWithEnv "find-up" "5.0.0" EnvProduction
        , mkDepWithEnv "fs-extra" "11.3.0" EnvProduction
        , mkDepWithEnv "get-npm-tarball-url" "2.1.0" EnvProduction
        , mkDepWithEnv "get-port" "5.1.1" EnvProduction
        , mkDepWithEnv "get-port" "5.1.1" EnvDevelopment
        , mkDepWithEnv "ghooks" "2.0.4" EnvDevelopment
        , mkDepWithEnv "graceful-fs" "4.2.11" EnvProduction
        , mkDepWithEnv "graceful-git" "4.0.0" EnvProduction
        , mkDepWithEnv "graph-cycles" "1.2.1" EnvProduction
        , mkDepWithEnv "hosted-git-info" "1.0.0" EnvProduction
        , mkDepWithEnv "https-proxy-server-express" "0.1.2" EnvDevelopment
        , mkDepWithEnv "husky" "9.1.7" EnvDevelopment
        , mkDepWithEnv "hyperdrive-schemas" "2.0.0" EnvProduction
        , mkDepWithEnv "ini" "4.1.1" EnvProduction
        , mkDepWithEnv "is-gzip" "2.0.0" EnvProduction
        , mkDepWithEnv "is-inner-link" "4.0.0" EnvProduction
        , mkDepWithEnv "is-port-reachable" "3.0.0" EnvDevelopment
        , mkDepWithEnv "is-subdir" "1.2.0" EnvProduction
        , mkDepWithEnv "is-windows" "1.0.2" EnvProduction
        , mkDepWithEnv "is-windows" "1.0.2" EnvDevelopment
        , mkDepWithEnv "isexe" "2.0.0" EnvProduction
        , mkDepWithEnv "isexe" "2.0.0" EnvDevelopment
        , mkDepWithEnv "jest" "29.7.0" EnvDevelopment
        , mkDepWithEnv "jest-diff" "29.7.0" EnvDevelopment
        , mkDepWithEnv "js-yaml" "0.0.7" EnvProduction
        , mkDepWithEnv "json5" "2.2.3" EnvProduction
        , mkDepWithEnv "keyv" "4.5.4" EnvDevelopment
        , mkDepWithEnv "lcov-result-merger" "3.3.0" EnvDevelopment
        , mkDepWithEnv "load-json-file" "6.2.0" EnvProduction
        , mkDepWithEnv "load-json-file" "6.2.0" EnvDevelopment
        , mkDepWithEnv "lodash.kebabcase" "4.1.1" EnvProduction
        , mkDepWithEnv "lodash.throttle" "4.1.1" EnvProduction
        , mkDepWithEnv "loud-rejection" "2.2.0" EnvDevelopment
        , mkDepWithEnv "lru-cache" "10.4.3" EnvProduction
        , mkDepWithEnv "make-empty-dir" "3.0.2" EnvProduction
        , mkDepWithEnv "mdast-util-to-string" "2.0.0" EnvProduction
        , mkDepWithEnv "mem" "8.1.1" EnvProduction
        , mkDepWithEnv "micromatch" "4.0.8" EnvProduction
        , mkDepWithEnv "ndjson" "2.0.0" EnvProduction
        , mkDepWithEnv "nerf-dart" "1.0.0" EnvProduction
        , mkDepWithEnv "nock" "13.3.4" EnvDevelopment
        , mkDepWithEnv "node-fetch" "1.0.0" EnvProduction
        , mkDepWithEnv "normalize-newline" "3.0.0" EnvDevelopment
        , mkDepWithEnv "normalize-package-data" "5.0.0" EnvProduction
        , mkDepWithEnv "normalize-path" "3.0.0" EnvProduction
        , mkDepWithEnv "normalize-path" "3.0.0" EnvDevelopment
        , mkDepWithEnv "normalize-registry-url" "2.0.0" EnvProduction
        , mkDepWithEnv "npm-packlist" "5.1.3" EnvProduction
        , mkDepWithEnv "object-hash" "3.0.0" EnvProduction
        , mkDepWithEnv "p-any" "3.0.0" EnvDevelopment
        , mkDepWithEnv "p-defer" "3.0.0" EnvProduction
        , mkDepWithEnv "p-defer" "3.0.0" EnvDevelopment
        , mkDepWithEnv "p-every" "2.0.0" EnvProduction
        , mkDepWithEnv "p-filter" "2.1.0" EnvProduction
        , mkDepWithEnv "p-limit" "3.1.0" EnvProduction
        , mkDepWithEnv "p-map-values" "1.0.0" EnvProduction
        , mkDepWithEnv "p-memoize" "4.0.1" EnvProduction
        , mkDepWithEnv "p-queue" "6.6.2" EnvProduction
        , mkDepWithEnv "p-settle" "4.1.1" EnvProduction
        , mkDepWithEnv "parse-json" "5.2.0" EnvProduction
        , mkDepWithEnv "parse-npm-tarball-url" "3.0.0" EnvProduction
        , mkDepWithEnv "path-absolute" "1.0.1" EnvProduction
        , mkDepWithEnv "path-exists" "4.0.0" EnvProduction
        , mkDepWithEnv "path-name" "1.0.0" EnvProduction
        , mkDepWithEnv "path-name" "1.0.0" EnvDevelopment
        , mkDepWithEnv "path-temp" "2.1.0" EnvProduction
        , mkDepWithEnv "pidtree" "0.6.0" EnvDevelopment
        , mkDepWithEnv "pkg" "5.12.0" EnvProduction
        , mkDepWithEnv "preferred-pm" "3.1.4" EnvProduction
        , mkDepWithEnv "pretty-bytes" "5.6.0" EnvProduction
        , mkDepWithEnv "pretty-ms" "7.0.1" EnvProduction
        , mkDepWithEnv "process-exists" "4.1.0" EnvProduction
        , mkDepWithEnv "promise-share" "1.0.0" EnvProduction
        , mkDepWithEnv "proxyquire" "2.1.3" EnvDevelopment
        , mkDepWithEnv "ps-list" "7.2.0" EnvDevelopment
        , mkDepWithEnv "publish-packed" "4.1.2" EnvDevelopment
        , mkDepWithEnv "ramda" "0.28.1" EnvProduction
        , mkDepWithEnv "read-ini-file" "4.0.0" EnvProduction
        , mkDepWithEnv "read-yaml-file" "2.1.0" EnvProduction
        , mkDepWithEnv "read-yaml-file" "2.1.0" EnvDevelopment
        , mkDepWithEnv "realpath-missing" "1.1.0" EnvProduction
        , mkDepWithEnv "remark-parse" "9.0.0" EnvProduction
        , mkDepWithEnv "remark-stringify" "9.0.1" EnvProduction
        , mkDepWithEnv "rename-overwrite" "6.0.2" EnvProduction
        , mkDepWithEnv "render-help" "1.0.3" EnvProduction
        , mkDepWithEnv "render-help" "1.0.3" EnvDevelopment
        , mkDepWithEnv "resolve-link-target" "2.0.0" EnvProduction
        , mkDepWithEnv "resolve-link-target" "2.0.0" EnvDevelopment
        , mkDepWithEnv "rimraf" "3.0.2" EnvDevelopment
        , mkDepWithEnv "root-link-target" "3.1.0" EnvProduction
        , mkDepWithEnv "run-groups" "3.0.1" EnvProduction
        , mkDepWithEnv "rxjs" "7.8.1" EnvProduction
        , mkDepWithEnv "safe-buffer" "5.2.1" EnvDevelopment
        , mkDepWithEnv "safe-execa" "0.1.4" EnvProduction
        , mkDepWithEnv "safe-promise-defer" "1.0.1" EnvProduction
        , mkDepWithEnv "sanitize-filename" "1.6.3" EnvProduction
        , mkDepWithEnv "semver" "7.7.1" EnvProduction
        , mkDepWithEnv "semver" "7.7.1" EnvDevelopment
        , mkDepWithEnv "semver-range-intersect" "0.3.1" EnvProduction
        , mkDepWithEnv "semver-utils" "1.1.4" EnvProduction
        , mkDepWithEnv "shell-quote" "1.8.2" EnvProduction
        , mkDepWithEnv "shx" "0.3.4" EnvDevelopment
        , mkDepWithEnv "signal-exit" "3.0.7" EnvProduction
        , mkDepWithEnv "sinon" "16.1.3" EnvDevelopment
        , mkDepWithEnv "sort-keys" "4.2.0" EnvProduction
        , mkDepWithEnv "split-cmd" "1.1.0" EnvProduction
        , mkDepWithEnv "split-cmd" "1.1.0" EnvDevelopment
        , mkDepWithEnv "ssri" "10.0.5" EnvProduction
        , mkDepWithEnv "ssri" "10.0.5" EnvDevelopment
        , mkDepWithEnv "stacktracey" "2.1.8" EnvProduction
        , mkDepWithEnv "string-length" "4.0.2" EnvProduction
        , mkDepWithEnv "strip-bom" "4.0.0" EnvProduction
        , mkDepWithEnv "strip-comments-strings" "1.2.0" EnvProduction
        , mkDepWithEnv "symlink-dir" "6.0.5" EnvProduction
        , mkDepWithEnv "symlink-dir" "6.0.5" EnvDevelopment
        , mkDepWithEnv "tar" "6.2.1" EnvDevelopment
        , mkDepWithEnv "tar-stream" "2.2.0" EnvProduction
        , mkDepWithEnv "tar-stream" "2.2.0" EnvDevelopment
        , mkDepWithEnv "tempy" "1.0.1" EnvProduction
        , mkDepWithEnv "tempy" "1.0.1" EnvDevelopment
        , mkDepWithEnv "terminal-link" "2.1.1" EnvProduction
        , mkDepWithEnv "tinyglobby" "0.2.12" EnvProduction
        , mkDepWithEnv "touch" "3.1.0" EnvProduction
        , mkDepWithEnv "touch" "3.1.0" EnvDevelopment
        , mkDepWithEnv "tree-kill" "1.2.2" EnvProduction
        , mkDepWithEnv "tree-kill" "1.2.2" EnvDevelopment
        , mkDepWithEnv "ts-jest" "29.2.3" EnvDevelopment
        , mkDepWithEnv "ts-node" "10.9.2" EnvDevelopment
        , mkDepWithEnv "typescript" "5.5.4" EnvProduction
        , mkDepWithEnv "typescript" "5.5.4" EnvDevelopment
        , mkDepWithEnv "unified" "9.2.2" EnvProduction
        , mkDepWithEnv "uuid" "9.0.1" EnvProduction
        , mkDepWithEnv "v8-compile-cache" "2.4.0" EnvProduction
        , mkDepWithEnv "validate-npm-package-name" "5.0.0" EnvProduction
        , mkDepWithEnv "version-selector-type" "3.0.0" EnvProduction
        , mkDepWithEnv "which" "3.0.1" EnvProduction
        , mkDepWithEnv "write-file-atomic" "5.0.1" EnvProduction
        , mkDepWithEnv "write-ini-file" "4.0.1" EnvProduction
        , mkDepWithEnv "write-json-file" "4.3.0" EnvProduction
        , mkDepWithEnv "write-json-file" "4.3.0" EnvDevelopment
        , mkDepWithEnv "write-json5-file" "3.1.0" EnvProduction
        , mkDepWithEnv "write-pkg" "4.0.0" EnvProduction
        , mkDepWithEnv "write-pkg" "4.0.0" EnvDevelopment
        , mkDepWithEnv "write-yaml-file" "5.0.0" EnvProduction
        , mkDepWithEnv "write-yaml-file" "5.0.0" EnvDevelopment
        , mkDepWithEnv "yaml-tag" "1.1.0" EnvDevelopment
        ]
        graph

pnpmLockV9TestGraphSpec :: Graphing Dependency -> Spec
pnpmLockV9TestGraphSpec graph = do
  describe "buildGraph with v9 test dependencies" $ do
    it "should include dependencies of root package as direct" $ do
      expectDirect
        [ mkDepWithEnv "aws-sdk" "2.1148.0" EnvProduction
        , mkDepWithEnv "chalk" "5.3.0" EnvProduction
        , mkDepWithEnv "colors" "github.com/Marak/colors.js/6bc50e79eeaa1d87369bb3e7e608ebed18c5cf26" EnvProduction
        , mkDepWithEnv "react" "18.1.0" EnvDevelopment
        ]
        graph

pnpmLockV9SnapshotGraphSpec :: Graphing Dependency -> Spec
pnpmLockV9SnapshotGraphSpec graph = do
  describe "buildGraph with v9 snapshot fixture" $ do
    it "should include dependencies of root package as direct" $ do
      expectDirect
        [ mkDepWithEnv "@scope/pkg" "1.0.0" EnvProduction
        , mkDepWithEnv "a" "1.0.0" EnvProduction
        ]
        graph

mkDepWithEnv :: Text -> Text -> DepEnvironment -> Dependency
mkDepWithEnv name version env =
  Dependency
    NodeJSType
    name
    (Just $ CEq version)
    mempty
    (Set.singleton env)
    mempty
