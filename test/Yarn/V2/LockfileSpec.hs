module Yarn.V2.LockfileSpec (
  spec,
) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Algebra.Graph.AdjacencyMap.Extra qualified as AME
import Control.Carrier.Diagnostics
import Data.Map.Strict qualified as M
import Data.Yaml (decodeFileThrow)
import DepTypes
import GraphUtil
import Strategy.Yarn.V2.Lockfile
import Strategy.Yarn.V2.Resolvers
import Strategy.Yarn.V2.YarnLock (buildGraph, stitchLockfile)
import Test.Hspec hiding (expectationFailure, shouldBe)
import Test.Hspec.Expectations.Pretty

-- End-to-end test on the example project from the dev docs
spec :: Spec
spec = do
  describe "YarnLockfile parser" $ do
    it "should be able to parse the example from the dev docs" $ do
      lockfile <- decodeFileThrow "test/Yarn/V2/testdata/yarn.lock"
      lockfile `shouldBe` exampleLockfile

  describe "stitchLockfile" $ do
    it "should work on the example from the dev docs" $ do
      case run (runDiagnostics (stitchLockfile exampleLockfile)) of
        Left err -> expectationFailure (show (renderFailureBundle err))
        Right stitched -> stitched `shouldBe` exampleStitched

  describe "resolveLocatorToPackage" $ do
    it "should work on the example from the dev docs" $ do
      case run (runDiagnostics (AME.gtraverse resolveLocatorToPackage exampleStitched)) of
        Left err -> expectationFailure (show (renderFailureBundle err))
        Right graph -> graph `shouldBe` exampleResolved

  describe "buildGraph" $ do
    it "should work on the example from the dev docs" $ do
      let graph = buildGraph exampleResolved
      expectDeps [underscoreFromGitDep, underscoreFromNpmDep] graph
      expectDirect [underscoreFromGitDep, underscoreFromNpmDep] graph
      expectEdges [] graph

---------- Example: lockfile

exampleLockfile :: YarnLockfile
exampleLockfile =
  YarnLockfile $
    M.fromList
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
    , dependencyEnvironments = []
    , dependencyLocations = []
    , dependencyTags = M.empty
    }

underscoreFromNpmDep :: Dependency
underscoreFromNpmDep =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "underscore"
    , dependencyVersion = Just (CEq "1.13.1")
    , dependencyEnvironments = []
    , dependencyLocations = []
    , dependencyTags = M.empty
    }
