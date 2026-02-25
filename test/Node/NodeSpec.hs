{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Node.NodeSpec (spec) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Data.Foldable (for_)
import Data.Glob (unsafeGlobRel)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty (nonEmpty)
import Data.Tagged (applyTag)
import Graphing qualified
import Path (Abs, Dir, Path, mkRelDir, mkRelFile, (</>))
import Path.IO (getCurrentDir)
import Strategy.Node (NodeProject (NPMLock), discover, extractDepListsForTargets, findWorkspaceBuildTargets, getDeps)
import Strategy.Node.PackageJson (
  FlatDeps (..),
  Manifest (..),
  NodePackage (NodePackage),
  PackageJson (
    PackageJson,
    packageDeps,
    packageDevDeps,
    packageLicense,
    packageLicenses,
    packageName,
    packagePeerDeps,
    packageVersion,
    packageWorkspaces
  ),
  PkgJsonGraph (PkgJsonGraph, jsonGraph, jsonLookup),
  PkgJsonLicense (LicenseText),
  PkgJsonWorkspaces (PkgJsonWorkspaces, unWorkspaces),
  Production,
 )
import Test.Effect (it', shouldBe')
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Types (
  BuildTarget (BuildTarget),
  DependencyResults (
    DependencyResults,
    dependencyGraph,
    dependencyGraphBreadth,
    dependencyManifestFiles
  ),
  DiscoveredProject (DiscoveredProject, projectBuildTargets, projectData, projectPath, projectType),
  DiscoveredProjectType (NpmProjectType),
  FoundTargets (FoundTargets, ProjectWithoutTargets),
  GraphBreadth (Complete),
 )

spec :: Spec
spec = do
  currDir <- runIO getCurrentDir
  pkgJsonWorkspaceSpec currDir
  npmLockAnalysisSpec currDir
  workspaceBuildTargetsSpec currDir
  extractDepListsForTargetsSpec currDir

discoveredWorkSpaceProj :: Path Abs Dir -> DiscoveredProject NodeProject
discoveredWorkSpaceProj currDir =
  DiscoveredProject
    { projectType = NpmProjectType
    , projectPath = currDir </> $(mkRelDir "test/Node/testdata/workspace-test/")
    , projectBuildTargets =
        maybe ProjectWithoutTargets FoundTargets . nonEmpty $
          Set.fromList [BuildTarget "workspace-test", BuildTarget "pkg-a", BuildTarget "pkg-b"]
    , projectData =
        NPMLock
          ( Manifest
              { unManifest = currDir </> $(mkRelFile "test/Node/testdata/workspace-test/package-lock.json")
              }
          )
          ( PkgJsonGraph
              { jsonGraph =
                  AM.edges
                    [
                      ( Manifest{unManifest = workspaceManifest}
                      , Manifest{unManifest = packageBManifest}
                      )
                    ,
                      ( Manifest{unManifest = workspaceManifest}
                      , Manifest{unManifest = packageAManifest}
                      )
                    ]
              , jsonLookup =
                  Map.fromList
                    [
                      ( Manifest{unManifest = packageBManifest}
                      , PackageJson
                          { packageName = Just "pkg-b"
                          , packageVersion = Just "1.0.0"
                          , packageWorkspaces = PkgJsonWorkspaces{unWorkspaces = []}
                          , packageDeps = Map.empty
                          , packageDevDeps = Map.empty
                          , packageLicense = Just (LicenseText "ISC")
                          , packageLicenses = Nothing
                          , packagePeerDeps = Map.empty
                          }
                      )
                    ,
                      ( Manifest{unManifest = workspaceManifest}
                      , PackageJson
                          { packageName = Just "workspace-test"
                          , packageVersion = Just "1.0.0"
                          , packageWorkspaces =
                              PkgJsonWorkspaces
                                { unWorkspaces =
                                    [ unsafeGlobRel "pkg-a"
                                    , unsafeGlobRel "nested/pkg-b"
                                    ]
                                }
                          , packageDeps = Map.empty
                          , packageDevDeps = Map.empty
                          , packageLicense = Just (LicenseText "ISC")
                          , packageLicenses = Nothing
                          , packagePeerDeps = Map.empty
                          }
                      )
                    ,
                      ( Manifest{unManifest = packageAManifest}
                      , PackageJson
                          { packageName = Just "pkg-a"
                          , packageVersion = Just "1.0.0"
                          , packageWorkspaces = PkgJsonWorkspaces{unWorkspaces = []}
                          , packageDeps = Map.empty
                          , packageDevDeps = Map.empty
                          , packageLicense = Just (LicenseText "ISC")
                          , packageLicenses = Nothing
                          , packagePeerDeps = Map.empty
                          }
                      )
                    ]
              }
          )
    }
  where
    packageAManifest = currDir </> $(mkRelFile "test/Node/testdata/workspace-test/pkg-a/package.json")
    packageBManifest = currDir </> $(mkRelFile "test/Node/testdata/workspace-test/nested/pkg-b/package.json")
    workspaceManifest = currDir </> $(mkRelFile "test/Node/testdata/workspace-test/package.json")

discoveredWorkSpaceProjDeps :: Path Abs Dir -> DependencyResults
discoveredWorkSpaceProjDeps currDir =
  DependencyResults
    { dependencyGraph = Graphing.empty
    , dependencyGraphBreadth = Complete
    , dependencyManifestFiles = [currDir </> $(mkRelFile "test/Node/testdata/workspace-test/package-lock.json")]
    }

pkgJsonWorkspaceSpec :: Path Abs Dir -> Spec
pkgJsonWorkspaceSpec currDir = describe "NPM workspace detection" $ do
  let workspaceDir = currDir </> $(mkRelDir "test/Node/testdata/workspace-test")
  it' "Discovers workspace projects for workspaces " $ do
    discoveredProjects <- discover workspaceDir
    discoveredProjects `shouldBe'` [discoveredWorkSpaceProj currDir]

npmLockAnalysisSpec :: Path Abs Dir -> Spec
npmLockAnalysisSpec currDir = do
  let workspaceDir = currDir </> $(mkRelDir "test/Node/testdata/workspace-test")

  describe "NPM Lock analysis" $ do
    it' "Ignores workspace packages in discovery/analysis of an NPMLock project" $ do
      discoveredProjects <- discover workspaceDir
      for_ discoveredProjects $
        \DiscoveredProject{..} ->
          do
            depGraph <- getDeps projectBuildTargets projectData
            depGraph `shouldBe'` discoveredWorkSpaceProjDeps currDir

workspaceBuildTargetsSpec :: Path Abs Dir -> Spec
workspaceBuildTargetsSpec currDir = describe "findWorkspaceBuildTargets" $ do
  it "returns FoundTargets with root and workspace member names" $ do
    let graph = workspaceGraphWithDeps currDir
        targets = findWorkspaceBuildTargets graph
        expected =
          maybe ProjectWithoutTargets FoundTargets . nonEmpty $
            Set.fromList [BuildTarget "workspace-test", BuildTarget "pkg-a", BuildTarget "pkg-b"]
    targets `shouldBe` expected

  it "returns ProjectWithoutTargets for single-package project" $ do
    let singleManifest = currDir </> $(mkRelFile "test/Node/testdata/workspace-test/package.json")
        graph =
          PkgJsonGraph
            { jsonGraph = AM.vertex (Manifest singleManifest)
            , jsonLookup =
                Map.fromList
                  [
                    ( Manifest singleManifest
                    , emptyPackageJson{packageName = Just "my-app"}
                    )
                  ]
            }
    findWorkspaceBuildTargets graph `shouldBe` ProjectWithoutTargets

extractDepListsForTargetsSpec :: Path Abs Dir -> Spec
extractDepListsForTargetsSpec currDir = describe "extractDepListsForTargets" $ do
  let graph = workspaceGraphWithDeps currDir

  it "includes all deps when ProjectWithoutTargets" $ do
    let result = extractDepListsForTargets ProjectWithoutTargets graph
    -- Should include deps from root, pkg-a, and pkg-b
    let expectedDirect =
          Set.fromList
            [ NodePackage "lodash" "^4.0.0"
            , NodePackage "express" "^4.0.0"
            , NodePackage "husky" "^8.0.0"
            ]
    directDeps result `shouldBe` applyTag @Production expectedDirect

  it "scopes deps to selected targets only" $ do
    let targets =
          maybe ProjectWithoutTargets FoundTargets . nonEmpty $
            Set.fromList [BuildTarget "pkg-a"]
        result = extractDepListsForTargets targets graph
    -- Should include only pkg-a's deps, not root or pkg-b
    let expectedDirect = Set.fromList [NodePackage "lodash" "^4.0.0"]
    directDeps result `shouldBe` applyTag @Production expectedDirect

  it "includes root deps when root target is selected" $ do
    let targets =
          maybe ProjectWithoutTargets FoundTargets . nonEmpty $
            Set.fromList [BuildTarget "workspace-test"]
        result = extractDepListsForTargets targets graph
    let expectedDirect = Set.fromList [NodePackage "husky" "^8.0.0"]
    directDeps result `shouldBe` applyTag @Production expectedDirect

  it "includes all deps when all targets selected" $ do
    let targets =
          maybe ProjectWithoutTargets FoundTargets . nonEmpty $
            Set.fromList [BuildTarget "workspace-test", BuildTarget "pkg-a", BuildTarget "pkg-b"]
        result = extractDepListsForTargets targets graph
    let expectedDirect =
          Set.fromList
            [ NodePackage "lodash" "^4.0.0"
            , NodePackage "express" "^4.0.0"
            , NodePackage "husky" "^8.0.0"
            ]
    directDeps result `shouldBe` applyTag @Production expectedDirect

-- | A workspace graph with actual dependencies for testing extractDepListsForTargets.
workspaceGraphWithDeps :: Path Abs Dir -> PkgJsonGraph
workspaceGraphWithDeps currDir =
  PkgJsonGraph
    { jsonGraph =
        AM.edges
          [ (Manifest rootManifest, Manifest pkgAManifest)
          , (Manifest rootManifest, Manifest pkgBManifest)
          ]
    , jsonLookup =
        Map.fromList
          [
            ( Manifest rootManifest
            , emptyPackageJson
                { packageName = Just "workspace-test"
                , packageDeps = Map.fromList [("husky", "^8.0.0")]
                , packageWorkspaces =
                    PkgJsonWorkspaces
                      { unWorkspaces =
                          [ unsafeGlobRel "pkg-a"
                          , unsafeGlobRel "nested/pkg-b"
                          ]
                      }
                }
            )
          ,
            ( Manifest pkgAManifest
            , emptyPackageJson
                { packageName = Just "pkg-a"
                , packageDeps = Map.fromList [("lodash", "^4.0.0")]
                }
            )
          ,
            ( Manifest pkgBManifest
            , emptyPackageJson
                { packageName = Just "pkg-b"
                , packageDeps = Map.fromList [("express", "^4.0.0")]
                }
            )
          ]
    }
  where
    rootManifest = currDir </> $(mkRelFile "test/Node/testdata/workspace-test/package.json")
    pkgAManifest = currDir </> $(mkRelFile "test/Node/testdata/workspace-test/pkg-a/package.json")
    pkgBManifest = currDir </> $(mkRelFile "test/Node/testdata/workspace-test/nested/pkg-b/package.json")

emptyPackageJson :: PackageJson
emptyPackageJson =
  PackageJson
    { packageName = Nothing
    , packageVersion = Nothing
    , packageWorkspaces = PkgJsonWorkspaces{unWorkspaces = []}
    , packageDeps = Map.empty
    , packageDevDeps = Map.empty
    , packageLicense = Nothing
    , packageLicenses = Nothing
    , packagePeerDeps = Map.empty
    }
