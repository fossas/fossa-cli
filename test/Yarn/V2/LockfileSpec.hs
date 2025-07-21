{-# LANGUAGE TemplateHaskell #-}

module Yarn.V2.LockfileSpec (
  spec,
) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Algebra.Graph.AdjacencyMap.Extra qualified as AME
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Tagged (applyTag)
import Data.Text (Text)
import Data.Yaml (decodeFileThrow)
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (GitType, NodeJSType),
  Dependency (..),
  VerConstraint (CEq),
 )
import GraphUtil (
  expectDeps,
  expectDeps',
  expectDirect,
  expectDirect',
  expectEdges,
  expectEdges',
 )
import Path (mkRelFile, (</>))
import Path.IO (getCurrentDir)
import Strategy.Node.PackageJson (
  Development,
  FlatDeps (..),
  NodePackage (NodePackage),
  Production,
 )
import Strategy.Node.YarnV2.Lockfile (
  Descriptor (Descriptor),
  Locator (Locator),
  PackageDescription (
    PackageDescription,
    descDependencies,
    descResolution,
    descVersion
  ),
  YarnLockfile (..),
 )
import Strategy.Node.YarnV2.Resolvers (
  Package (GitPackage, NpmPackage, WorkspacePackage),
  resolveLocatorToPackage,
 )
import Strategy.Node.YarnV2.YarnLock (analyze, buildGraph, stitchLockfile)
import Test.Effect (it', shouldBe')
import Test.Hspec (Spec, describe, it, shouldBe)

-- End-to-end test on the example project from the dev docs
spec :: Spec
spec = do
  describe "YarnLockfile parser" $ do
    it "should be able to parse the example from the dev docs" $ do
      lockfile <- decodeFileThrow "test/Yarn/V2/testdata/yarn.lock-example"
      lockfile `shouldBe` exampleLockfile

  describe "stitchLockfile" $ do
    it' "should work on the example from the dev docs" $ do
      stitched <- stitchLockfile exampleLockfile
      stitched `shouldBe'` exampleStitched

  describe "resolveLocatorToPackage" $ do
    it' "should work on the example from the dev docs" $ do
      graph <- AME.gtraverse resolveLocatorToPackage exampleStitched
      graph `shouldBe'` exampleResolved

  describe "buildGraph" $ do
    it "should work on the example from the dev docs" $ do
      let graph = buildGraph exampleResolved mempty
      expectDeps [underscoreFromGitDep, underscoreFromNpmDep] graph
      expectDirect [underscoreFromGitDep, underscoreFromNpmDep] graph
      expectEdges [] graph

  describe "analyze" $
    it' "should work from beginning to end" $ do
      curdir <- getCurrentDir
      let yarnfile = curdir </> $(mkRelFile "test/Yarn/V2/testdata/yarn.lock-e2e")
      graph <- analyze yarnfile e2eFlatDeps
      expectDeps' allE2EDeps graph
      expectDirect' [acornJsxDep, isFqdnDep, onceDep, semverDep] graph
      expectEdges'
        [ (acornJsxDep, acornDep)
        , (lruCacheDep, yallistDep)
        , (onceDep, wrappyDep)
        , (semverDep, lruCacheDep)
        ]
        graph

---------- Example: lockfile

exampleLockfile :: YarnLockfile
exampleLockfile =
  YarnLockfile $
    Map.fromList
      [
        ( [Descriptor Nothing "toplevel" "workspace:."]
        , PackageDescription
            { descVersion = "0.0.0-use.local"
            , descResolution = Locator Nothing "toplevel" "workspace:."
            , descDependencies = []
            }
        )
      ,
        ( [Descriptor Nothing "foo" "workspace:foo"]
        , PackageDescription
            { descVersion = "0.0.0-use.local"
            , descResolution = Locator Nothing "foo" "workspace:foo"
            , descDependencies =
                [ Descriptor Nothing "underscore" "^1.13.0"
                ]
            }
        )
      ,
        ( [Descriptor Nothing "bar" "workspace:bar"]
        , PackageDescription
            { descVersion = "0.0.0-use.local"
            , descResolution = Locator Nothing "bar" "workspace:bar"
            , descDependencies =
                [ Descriptor Nothing "underscore" "1.13.1"
                ]
            }
        )
      ,
        ( [Descriptor Nothing "quux" "workspace:quux"]
        , PackageDescription
            { descVersion = "0.0.0-use.local"
            , descResolution = Locator Nothing "quux" "workspace:quux"
            , descDependencies =
                [ Descriptor Nothing "underscore" "jashkenas/underscore#tag=1.13.1"
                ]
            }
        )
      ,
        ( [Descriptor Nothing "underscore" "jashkenas/underscore#tag=1.13.1"]
        , PackageDescription
            { descVersion = "1.13.1"
            , descResolution = Locator Nothing "underscore" "https://github.com/jashkenas/underscore.git#commit=cbb48b79fc1205aa04feb03dbc055cdd28a12652"
            , descDependencies = []
            }
        )
      ,
        ( [Descriptor Nothing "underscore" "npm:1.13.1", Descriptor Nothing "underscore" "npm:^1.13.0"]
        , PackageDescription
            { descVersion = "1.13.1"
            , descResolution = Locator Nothing "underscore" "npm:1.13.1"
            , descDependencies = []
            }
        )
      ]

---------- Example: stitched lockfile

exampleStitched :: AM.AdjacencyMap Locator
exampleStitched =
  AM.overlays
    [ AM.vertex toplevelWorkspaceL
    , AM.vertex fooWorkspaceL
    , AM.vertex barWorkspaceL
    , AM.vertex quuxWorkspaceL
    , AM.vertex underscoreFromGitL
    , AM.vertex underscoreFromNpmL
    , AM.edge fooWorkspaceL underscoreFromNpmL
    , AM.edge barWorkspaceL underscoreFromNpmL
    , AM.edge quuxWorkspaceL underscoreFromGitL
    ]

toplevelWorkspaceL :: Locator
toplevelWorkspaceL = Locator Nothing "toplevel" "workspace:."

fooWorkspaceL :: Locator
fooWorkspaceL = Locator Nothing "foo" "workspace:foo"

barWorkspaceL :: Locator
barWorkspaceL = Locator Nothing "bar" "workspace:bar"

quuxWorkspaceL :: Locator
quuxWorkspaceL = Locator Nothing "quux" "workspace:quux"

underscoreFromGitL :: Locator
underscoreFromGitL = Locator Nothing "underscore" "https://github.com/jashkenas/underscore.git#commit=cbb48b79fc1205aa04feb03dbc055cdd28a12652"

underscoreFromNpmL :: Locator
underscoreFromNpmL = Locator Nothing "underscore" "npm:1.13.1"

---------- Example: resolved lockfile

exampleResolved :: AM.AdjacencyMap Package
exampleResolved =
  AM.overlays
    [ AM.vertex toplevelWorkspace
    , AM.vertex fooWorkspace
    , AM.vertex barWorkspace
    , AM.vertex quuxWorkspace
    , AM.vertex underscoreFromGit
    , AM.vertex underscoreFromNpm
    , AM.edge fooWorkspace underscoreFromNpm
    , AM.edge barWorkspace underscoreFromNpm
    , AM.edge quuxWorkspace underscoreFromGit
    ]

toplevelWorkspace :: Package
toplevelWorkspace = WorkspacePackage "."

fooWorkspace :: Package
fooWorkspace = WorkspacePackage "foo"

barWorkspace :: Package
barWorkspace = WorkspacePackage "bar"

quuxWorkspace :: Package
quuxWorkspace = WorkspacePackage "quux"

underscoreFromGit :: Package
underscoreFromGit = GitPackage "https://github.com/jashkenas/underscore.git" "cbb48b79fc1205aa04feb03dbc055cdd28a12652"

underscoreFromNpm :: Package
underscoreFromNpm = NpmPackage Nothing "underscore" "1.13.1"

---------- Example: built graph

underscoreFromGitDep :: Dependency
underscoreFromGitDep =
  Dependency
    { dependencyType = GitType
    , dependencyName = "https://github.com/jashkenas/underscore.git"
    , dependencyVersion = Just (CEq "cbb48b79fc1205aa04feb03dbc055cdd28a12652")
    , dependencyEnvironments = mempty
    , dependencyLocations = []
    , dependencyTags = Map.empty
    }

underscoreFromNpmDep :: Dependency
underscoreFromNpmDep =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "underscore"
    , dependencyVersion = Just (CEq "1.13.1")
    , dependencyEnvironments = mempty
    , dependencyLocations = []
    , dependencyTags = Map.empty
    }

-- ====================== Yarn V2 with package.json data ======================

mkDep :: Text -> Text -> [DepEnvironment] -> Dependency
mkDep name version envList =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = name
    , dependencyVersion = Just $ CEq version
    , dependencyLocations = mempty
    , dependencyEnvironments = Set.fromList envList
    , dependencyTags = mempty
    }

allE2EDeps :: [Dependency]
allE2EDeps =
  [ acornDep
  , acornJsxDep
  , isFqdnDep
  , lruCacheDep
  , onceDep
  , semverDep
  , wrappyDep
  , yallistDep
  ]

acornDep :: Dependency
acornDep = mkDep "acorn" "3.3.0" [EnvDevelopment]

acornJsxDep :: Dependency
acornJsxDep = mkDep "acorn-jsx" "3.0.1" [EnvDevelopment]

isFqdnDep :: Dependency
isFqdnDep = mkDep "is-fqdn" "2.0.1" [EnvDevelopment]

lruCacheDep :: Dependency
lruCacheDep = mkDep "lru-cache" "6.0.0" [EnvProduction]

onceDep :: Dependency
onceDep = mkDep "once" "1.4.0" [EnvProduction]

semverDep :: Dependency
semverDep = mkDep "semver" "7.3.5" [EnvProduction]

wrappyDep :: Dependency
wrappyDep = mkDep "wrappy" "1.0.2" [EnvProduction]

yallistDep :: Dependency
yallistDep = mkDep "yallist" "4.0.0" [EnvProduction]

acornJsxNodePkg :: NodePackage
acornJsxNodePkg = NodePackage "acorn-jsx" "^3.0.0"

isFqdnNodePkg :: NodePackage
isFqdnNodePkg = NodePackage "is-fqdn" "^2.0.1"

onceNodePkg :: NodePackage
onceNodePkg = NodePackage "once" "^1.3.0"

semverNodePkg :: NodePackage
semverNodePkg = NodePackage "semver" "^7.3.2"

e2eFlatDeps :: FlatDeps
e2eFlatDeps =
  FlatDeps
    { directDeps = applyTag @Production $ Set.fromList [onceNodePkg, semverNodePkg]
    , devDeps = applyTag @Development $ Set.fromList [acornJsxNodePkg, isFqdnNodePkg]
    , manifests = mempty
    , resolutions = mempty
    }
