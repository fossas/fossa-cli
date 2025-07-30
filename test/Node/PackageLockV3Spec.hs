{-# LANGUAGE TemplateHaskell #-}

module Node.PackageLockV3Spec (
  spec,
) where

import Data.Aeson (eitherDecodeFileStrict')
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.String.Conversion (toString)
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (NodeJSType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.ReadFS (readContentsJson)
import GraphUtil (
  expectDep,
  expectDirect,
  expectEdge,
 )
import Graphing (Graphing)
import Path (Abs, Dir, File, Path, Rel, mkRelDir, mkRelFile, (</>))
import Path.IO (getCurrentDir)
import Strategy.Node.Npm.PackageLockV3 (
  NpmLockV3Resolved (unNpmLockV3Resolved),
  PackageLockV3 (packages),
  PackageLockV3Package (plV3PkgResolved),
  PackageName (PackageName),
  PackagePath (PackageLockV3PathKey, PackageLockV3Root, PackageLockV3WorkSpace),
  buildGraph,
  isV3Compatible,
  parsePathKey,
  vendorPrefixes,
 )
import Test.Effect (it', shouldBe')
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, runIO, shouldBe)

fixtureSimplePackageLockPath :: Path Rel File
fixtureSimplePackageLockPath = $(mkRelFile "simple-package-lock.json")

multiLevelVendorLockPath :: Path Rel File
multiLevelVendorLockPath = $(mkRelFile "multi-level-vendor-package-lock.json")

workspaceNestedModulePackageLockPath :: Path Rel File
workspaceNestedModulePackageLockPath = $(mkRelFile "ws-nested-module-package-lock.json")

devDependenciesLockPath :: Path Rel File
devDependenciesLockPath = $(mkRelFile "dev-dependencies-package-lock.json")

spec :: Spec
spec = do
  currentDir <- runIO getCurrentDir
  let parsingFixtureDir = currentDir </> $(mkRelDir "test/Node/testdata/lockfileV3/parsing/")
  let graphingFixtureDir = currentDir </> $(mkRelDir "test/Node/testdata/lockfileV3/graphing/")

  -- Resolved Field
  packageLockParseSpec parsingFixtureDir

  -- Helpers
  isV3CompatibleSpec
  parsePathKeySpec
  vendorPrefixesSpec

  -- Parse and Validate Graphing
  checkGraph (graphingFixtureDir </> fixtureSimplePackageLockPath) simplePackageLockGraphSpec
  checkGraph (graphingFixtureDir </> workspaceNestedModulePackageLockPath) workspaceNestedModulePackageLockGraphSpec
  checkGraph (graphingFixtureDir </> multiLevelVendorLockPath) multiLevelVendorLockGraphSpec
  checkGraph (graphingFixtureDir </> devDependenciesLockPath) devDependenciesLockGraphSpec

vendorPrefixesSpec :: Spec
vendorPrefixesSpec = do
  describe "vendorPrefixes" $ do
    it "should return all possible prefix until root, when workspace is not provided" $ do
      vendorPrefixes "node_modules/a" Nothing
        `shouldBe` [ "node_modules/a/node_modules/"
                   , "node_modules/"
                   ]

      vendorPrefixes "node_modules/a/node_modules/b" Nothing
        `shouldBe` [ "node_modules/a/node_modules/b/node_modules/"
                   , "node_modules/a/node_modules/"
                   , "node_modules/"
                   ]

      vendorPrefixes "node_modules/@someOrg/a/node_modules/b" Nothing
        `shouldBe` [ "node_modules/@someOrg/a/node_modules/b/node_modules/"
                   , "node_modules/@someOrg/a/node_modules/"
                   , "node_modules/"
                   ]

    it "should return all possible prefix until root, when workspace is provided" $ do
      vendorPrefixes "ws/myPkg" (Just "ws/myPkg")
        `shouldBe` [ "ws/myPkg/node_modules/"
                   ]

      vendorPrefixes "ws/myPkg/node_modules/a" (Just "ws/myPkg")
        `shouldBe` [ "ws/myPkg/node_modules/a/node_modules/"
                   , "ws/myPkg/node_modules/"
                   ]

      vendorPrefixes "ws/myPkg/node_modules/a/node_modules/b" (Just "ws/myPkg")
        `shouldBe` [ "ws/myPkg/node_modules/a/node_modules/b/node_modules/"
                   , "ws/myPkg/node_modules/a/node_modules/"
                   , "ws/myPkg/node_modules/"
                   ]

      vendorPrefixes "ws/myPkg/node_modules/@someOrg/a/node_modules/b" (Just "ws/myPkg")
        `shouldBe` [ "ws/myPkg/node_modules/@someOrg/a/node_modules/b/node_modules/"
                   , "ws/myPkg/node_modules/@someOrg/a/node_modules/"
                   , "ws/myPkg/node_modules/"
                   ]

isV3CompatibleSpec :: Spec
isV3CompatibleSpec = do
  describe "isV3Compatible" $ do
    it "should return false for version 1" $
      (isV3Compatible 1) `shouldBe` False

    it "should return false for version 2" $
      (isV3Compatible 2) `shouldBe` False

    it "should return true for version 3" $
      (isV3Compatible 3) `shouldBe` True

    it "should return false for version 4" $
      (isV3Compatible 4) `shouldBe` False

parsePathKeySpec :: Spec
parsePathKeySpec = do
  describe "parsePathKey" $ do
    describe "parse root" $
      it "should parse for root package path" $
        parsePathKey "" `shouldBe` PackageLockV3Root

    describe "parse package module" $ do
      it "should parse for top level path" $
        parsePathKey "node_modules/pkg"
          `shouldBe` PackageLockV3PathKey "node_modules/" (PackageName "pkg")

      it "should parse for vendor path" $
        parsePathKey "node_modules/pkg/node_modules/pkgDeep"
          `shouldBe` PackageLockV3PathKey "node_modules/pkg/node_modules/" (PackageName "pkgDeep")

      it "should parse for nested vendor path" $
        parsePathKey "node_modules/pkg/node_modules/pkgDeep/node_modules/pkgDeepLevel2"
          `shouldBe` PackageLockV3PathKey "node_modules/pkg/node_modules/pkgDeep/node_modules/" (PackageName "pkgDeepLevel2")

      it "should parse for vendor path in workspace" $
        parsePathKey "myWorkspace/node_modules/pkg"
          `shouldBe` PackageLockV3PathKey "myWorkspace/node_modules/" (PackageName "pkg")

      it "should parse for nested vendor path in workspace" $
        parsePathKey "myWorkspace/node_modules/pkg/node_modules/pkgDeep"
          `shouldBe` PackageLockV3PathKey "myWorkspace/node_modules/pkg/node_modules/" (PackageName "pkgDeep")

      it "should parse for org scoped packages" $
        parsePathKey "node_modules/@myOrg/pkg"
          `shouldBe` PackageLockV3PathKey "node_modules/" (PackageName "@myOrg/pkg")

    describe "parse workspace" $
      it "should parse for top level workspace" $
        parsePathKey "myWorkspace" `shouldBe` PackageLockV3WorkSpace "myWorkspace"

packageLockParseSpec :: Path Abs Dir -> Spec
packageLockParseSpec testDir =
  describe "parsing package-json.lock's resolved field" $ do
    it' "Should ignore \"resolved\": <bool> in package-lock.json" $ do
      let packageLock = testDir </> $(mkRelFile "boolean-resolved-package-lock.json")
      pkgLockV3 <- readContentsJson packageLock
      let foo =
            unNpmLockV3Resolved . plV3PkgResolved
              =<< Map.lookup
                (PackageLockV3PathKey "node_modules/" $ PackageName "foo")
                (packages pkgLockV3)
      foo `shouldBe'` Nothing

    it' "Should parse \"resolved\": <string> in package-lock.json" $ do
      let packageLock = testDir </> $(mkRelFile "string-resolved-package-lock.json")
      pkgLockV3 <- readContentsJson packageLock
      let foo =
            unNpmLockV3Resolved . plV3PkgResolved
              =<< Map.lookup
                (PackageLockV3PathKey "node_modules/" $ PackageName "foo")
                (packages pkgLockV3)
      foo `shouldBe'` Just "https://bar.npmjs.org/foo/-/foo-1.0.0.tgz"

    it' "Should parse dependency with no \"resolved\" key in package-lock.json" $ do
      let packageLock = testDir </> $(mkRelFile "absent-resolved-package-lock.json")
      pkgLockV3 <- readContentsJson packageLock
      let foo =
            unNpmLockV3Resolved . plV3PkgResolved
              =<< Map.lookup
                (PackageLockV3PathKey "node_modules/" $ PackageName "foo")
                (packages pkgLockV3)
      foo `shouldBe'` Nothing

simplePackageLockGraphSpec :: Graphing Dependency -> Spec
simplePackageLockGraphSpec graph = do
  let hasEdge :: Dependency -> Dependency -> Expectation
      hasEdge = expectEdge graph

  let hasDep :: Dependency -> Expectation
      hasDep dep = expectDep dep graph

  describe "buildGraph" $ do
    it "should include dependencies of root and workspace package as direct" $ do
      -- npm list --depth 0

      -- ├── 3@2.1.0
      -- ├── aws-sdk@2.1134.0
      -- ├── chalk@5.0.1
      -- ├── mocha@10.0.0
      -- ├── react@18.1.0
      -- └─┬ workspace-a-name@2.0.0 -> ./packages/a
      --   ├── aws-sdk@1.0.0
      --   └── commander@9.2.0

      expectDirect
        [ mkProdDep "3@2.1.0"
        , mkProdDep "aws-sdk@2.1134.0"
        , mkProdDep "react@18.1.0"
        , -- peer dep
          mkProdDep "chalk@5.0.1"
        , -- development dep
          mkDevDep "mocha@10.0.0"
        , -- from workspaces
          mkProdDep "aws-sdk@1.0.0"
        , mkProdDep "commander@9.2.0"
        ]
        graph

    it "should graph edge via top level path" $ do
      hasEdge (mkProdDep "3@2.1.0") (mkProdDep "2@1.0.2")

    it "should graph edge via vendored level path" $ do
      hasEdge (mkDevDep "log-symbols@4.1.0") (mkDevDep "chalk@4.1.2")
      hasEdge (mkProdDep "aws-sdk@1.0.0") (mkProdDep "xml2js@0.2.4")
      hasEdge (mkProdDep "xml2js@0.2.4") (mkProdDep "sax@1.2.1")

    it "should correctly graph workspace a's dependencies" $ do
      -- npm list --all --package-lock-only -- inspecting ./packages/a

      -- └─┬ workspace-a-name@2.0.0 -> ./packages/a
      --   ├─┬ aws-sdk@1.0.0
      --   │ ├─┬ xml2js@0.2.4
      --   │ │ └── sax@1.2.1 deduped
      --   │ └── xmlbuilder@9.0.7 deduped
      --   └── commander@9.2.0

      hasDep (mkProdDep "commander@9.2.0")
      hasDep (mkProdDep "aws-sdk@1.0.0")
      hasDep (mkProdDep "xml2js@0.2.4")
      hasDep (mkProdDep "xmlbuilder@9.0.7")
      hasDep (mkProdDep "sax@1.2.1")

      hasEdge (mkProdDep "aws-sdk@1.0.0") (mkProdDep "xml2js@0.2.4")
      hasEdge (mkProdDep "aws-sdk@1.0.0") (mkProdDep "xmlbuilder@9.0.7")
      hasEdge (mkProdDep "xml2js@0.2.4") (mkProdDep "sax@1.2.1")

    it "should correctly graph react's dependencies" $ do
      -- npm list --all --package-lock-only -- inspecting react

      -- ├─┬ react@18.1.0
      -- │ └─┬ loose-envify@1.4.0
      -- │   └── js-tokens@4.0.0

      expectDep (mkProdDep "react@18.1.0") graph
      expectDep (mkProdDep "loose-envify@1.4.0") graph
      expectDep (mkProdDep "js-tokens@4.0.0") graph

      hasEdge (mkProdDep "react@18.1.0") $ mkProdDep "loose-envify@1.4.0"
      hasEdge (mkProdDep "loose-envify@1.4.0") $ mkProdDep "js-tokens@4.0.0"

    it "should correctly graph 3's dependencies" $ do
      -- npm list --all --package-lock-only -- inspecting 3

      -- ├─┬ 3@2.1.0
      -- │ ├── 2@1.0.2
      -- │ ├── chunk-array@1.0.2
      -- │ └─┬ enforce-range@1.0.0
      -- │   └── 2@1.0.2 deduped

      hasDep (mkProdDep "3@2.1.0")
      hasDep (mkProdDep "2@1.0.2")
      hasDep (mkProdDep "chunk-array@1.0.2")
      hasDep (mkProdDep "enforce-range@1.0.0")

      hasEdge (mkProdDep "3@2.1.0") (mkProdDep "2@1.0.2")
      hasEdge (mkProdDep "3@2.1.0") (mkProdDep "chunk-array@1.0.2")
      hasEdge (mkProdDep "3@2.1.0") (mkProdDep "enforce-range@1.0.0")
      hasEdge (mkProdDep "enforce-range@1.0.0") (mkProdDep "2@1.0.2")

    it "should correctly graph aws-sdk@2.1134.0's dependencies" $ do
      -- npm list --all --package-lock-only -- inspecting aws-sdk@2.1124.0

      -- ├─┬ aws-sdk@2.1134.0
      -- │ ├─┬ buffer@4.9.2
      -- │ │ ├── base64-js@1.5.1
      -- │ │ ├── ieee754@1.1.13 deduped
      -- │ │ └── isarray@1.0.0
      -- │ ├── events@1.1.1
      -- │ ├── ieee754@1.1.13
      -- │ ├── jmespath@0.16.0
      -- │ ├── querystring@0.2.0
      -- │ ├── sax@1.2.1
      -- │ ├─┬ url@0.10.3
      -- │ │ ├── punycode@1.3.2
      -- │ │ └── querystring@0.2.0 deduped
      -- │ ├── uuid@3.3.2
      -- │ └─┬ xml2js@0.4.19
      -- │   ├── sax@1.2.1 deduped
      -- │   └── xmlbuilder@9.0.7

      hasDep (mkProdDep "aws-sdk@2.1134.0")
      hasDep (mkProdDep "buffer@4.9.2")
      hasDep (mkProdDep "base64-js@1.5.1")
      hasDep (mkProdDep "ieee754@1.1.13")
      hasDep (mkProdDep "isarray@1.0.0")

      hasEdge (mkProdDep "aws-sdk@2.1134.0") (mkProdDep "buffer@4.9.2")
      hasEdge (mkProdDep "buffer@4.9.2") (mkProdDep "base64-js@1.5.1")
      hasEdge (mkProdDep "buffer@4.9.2") (mkProdDep "ieee754@1.1.13")
      hasEdge (mkProdDep "buffer@4.9.2") (mkProdDep "isarray@1.0.0")

      hasDep (mkProdDep "events@1.1.1")
      hasDep (mkProdDep "jmespath@0.16.0")
      hasDep (mkProdDep "querystring@0.2.0")
      hasDep (mkProdDep "sax@1.2.1")

      hasEdge (mkProdDep "aws-sdk@2.1134.0") (mkProdDep "events@1.1.1")
      hasEdge (mkProdDep "aws-sdk@2.1134.0") (mkProdDep "ieee754@1.1.13")
      hasEdge (mkProdDep "aws-sdk@2.1134.0") (mkProdDep "jmespath@0.16.0")
      hasEdge (mkProdDep "aws-sdk@2.1134.0") (mkProdDep "querystring@0.2.0")
      hasEdge (mkProdDep "aws-sdk@2.1134.0") (mkProdDep "sax@1.2.1")

      hasDep (mkProdDep "url@0.10.3")
      hasDep (mkProdDep "punycode@1.3.2")
      hasDep (mkProdDep "querystring@0.2.0")

      hasEdge (mkProdDep "aws-sdk@2.1134.0") (mkProdDep "url@0.10.3")
      hasEdge (mkProdDep "url@0.10.3") (mkProdDep "punycode@1.3.2")
      hasEdge (mkProdDep "url@0.10.3") (mkProdDep "querystring@0.2.0")

      hasDep (mkProdDep "uuid@3.3.2")
      hasDep (mkProdDep "xml2js@0.4.19")
      hasDep (mkProdDep "xmlbuilder@9.0.7")

      hasEdge (mkProdDep "aws-sdk@2.1134.0") (mkProdDep "xml2js@0.4.19")
      hasEdge (mkProdDep "xml2js@0.4.19") (mkProdDep "sax@1.2.1")
      hasEdge (mkProdDep "xml2js@0.4.19") (mkProdDep "xmlbuilder@9.0.7")

multiLevelVendorLockGraphSpec :: Graphing Dependency -> Spec
multiLevelVendorLockGraphSpec graph = do
  let hasEdge :: Dependency -> Dependency -> Expectation
      hasEdge = expectEdge graph

  let hasDep :: Dependency -> Expectation
      hasDep dep = expectDep dep graph

  describe "buildGraph" $ do
    it "should include dependencies of root as direct" $ do
      expectDirect
        [ mkProdDep "express-session@1.17.3"
        , mkProdDep "log4js@6.7.0"
        ]
        graph

    it "should correctly graph dependencies resolved from multi-level vendored path" $ do
      hasDep (mkProdDep "debug@2.6.9")
      hasDep (mkProdDep "ms@2.0.0")
      hasEdge (mkProdDep "express-session@1.17.3") (mkProdDep "debug@2.6.9")
      hasEdge (mkProdDep "debug@2.6.9") (mkProdDep "ms@2.0.0")

      hasDep (mkProdDep "debug@4.3.4")
      hasDep (mkProdDep "ms@2.1.2")
      hasEdge (mkProdDep "log4js@6.7.0") (mkProdDep "debug@4.3.4")
      hasEdge (mkProdDep "debug@4.3.4") (mkProdDep "ms@2.1.2")

workspaceNestedModulePackageLockGraphSpec :: Graphing Dependency -> Spec
workspaceNestedModulePackageLockGraphSpec graph = do
  let hasEdge :: Dependency -> Dependency -> Expectation
      hasEdge = expectEdge graph

  let hasDep :: Dependency -> Expectation
      hasDep dep = expectDep dep graph

  describe "buildGraph" $ do
    it "should include dependencies of root and workspace package as direct" $ do
      -- └─┬ workspace-a-name@2.0.0 -> ./packages/a
      --   ├── b@2.0.0
      --   └── c@2.0.0
      expectDirect
        [ mkProdDep "b@2.0.0"
        , mkProdDep "c@2.0.0"
        ]
        graph

    it "should correctly graph b@1.0.0's dependencies (nested)" $ do
      -- └─┬ workspace-a-name@2.0.0 -> ./packages/a
      --   └─┬ b@2.0.0
      --     └── c@1.0.0

      hasDep (mkProdDep "b@2.0.0")
      hasDep (mkProdDep "c@1.0.0")
      hasEdge (mkProdDep "b@2.0.0") (mkProdDep "c@1.0.0")

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
    ["https://registry.npmjs.org/" <> name <> "/-/" <> name <> "-" <> version <> ".tgz"]
    (maybe mempty Set.singleton env)
    mempty

devDependenciesLockGraphSpec :: Graphing Dependency -> Spec
devDependenciesLockGraphSpec graph = do
  let hasDep :: Dependency -> Expectation
      hasDep dep = expectDep dep graph

  describe "buildGraph with devDependency environment propagation" $ do
    it "should correctly assign environments to all dependencies" $ do
      let allExpectedDeps =
            [ mkProdDep "prod-dep@1.0.0"
            , mkProdDep "prod-transitive@1.0.0"
            , mkDevDep "dev-dep@1.0.0"
            , mkDevDep "dev-transitive@1.0.0"
            , mkDevDep "shared-dep@1.0.0"
            ]
      mapM_ hasDep allExpectedDeps

checkGraph :: Path Abs File -> (Graphing Dependency -> Spec) -> Spec
checkGraph pathToFixture buildGraphSpec = do
  maybePkgLockV3 <- runIO $ eitherDecodeFileStrict' (toString pathToFixture)
  case maybePkgLockV3 of
    Left errMsg -> describe "packageLockV3" $ it "should parse lockfile" (expectationFailure errMsg)
    Right pkgLockV3 -> do
      buildGraphSpec (buildGraph pkgLockV3)
