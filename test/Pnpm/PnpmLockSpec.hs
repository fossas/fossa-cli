{-# LANGUAGE TemplateHaskell #-}

module Pnpm.PnpmLockSpec (
  spec,
) where

import Data.Set qualified as Set
import Data.String.Conversion (toString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Yaml (decodeFileEither, prettyPrintParseException)
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
import Strategy.Node.Pnpm.PnpmLock (buildGraph)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, runIO)

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

checkGraph :: Path Abs File -> (Graphing Dependency -> Spec) -> Spec
checkGraph pathToFixture buildGraphSpec = do
  eitherDecodedLockFile <- runIO $ decodeFileEither (toString pathToFixture)
  case eitherDecodedLockFile of
    Right pnpmLock -> buildGraphSpec (buildGraph pnpmLock)
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
  let pnpmLockV9 = currentDir </> $(mkRelFile "test/Pnpm/testdata/pnpm-9-project/pnpm-lock.yaml")
  -- With the advent of lockfile v9, pnpm now has its own pnpm-workspace.yaml file.
  let pnpmLockV9Workspace = currentDir </> $(mkRelFile "test/Pnpm/testdata/pnpm-9-workspace-project/pnpm-lock.yaml")

  describe "works with v9 format" $ do
    checkGraph pnpmLockV9 pnpmLockV9GraphSpec
    describe "workspace" $ checkGraph pnpmLockV9Workspace pnpmLockV9GraphSpec

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

pnpmLockV9GraphSpec :: Graphing Dependency -> Spec
pnpmLockV9GraphSpec graph = do
  describe "buildGraph" $ do
    it "should mark direct dependencies of project as direct" $ do
      expectDirect
        [ mkProdDep "uri-js@4.4.1"
        , mkDevDep "colorjs@0.1.9"
        ]
        graph
