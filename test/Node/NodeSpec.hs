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
import DepTypes (DepEnvironment (EnvProduction), Dependency (dependencyEnvironments, dependencyName))
import Graphing qualified
import Path (Abs, Dir, Path, mkRelDir, mkRelFile, (</>))
import Path.IO (getCurrentDir)
import Strategy.Node (NodeProject (NPMLock), discover, extractDepListsForTargets, findWorkspaceBuildTargets, getDeps, pkgGraph, resolveNpmV3WorkspacePaths, resolvePnpmImporterKeys, workspaceRootTargetName)
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
import Test.Effect (expectationFailure', it', shouldBe', shouldSatisfy')
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
  dotslashWorkspaceSpec currDir
  selfLoopWorkspaceSpec currDir
  npmLockAnalysisSpec currDir
  workspaceBuildTargetsSpec currDir
  extractDepListsForTargetsSpec currDir
  resolveNpmV3WorkspacePathsSpec currDir
  resolvePnpmImporterKeysSpec currDir
  unnamedWorkspaceRootSpec currDir

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

dotslashWorkspaceSpec :: Path Abs Dir -> Spec
dotslashWorkspaceSpec currDir = describe "Workspace globs with a leading ./" $ do
  let workspaceDir = currDir </> $(mkRelDir "test/Node/testdata/workspace-dotslash")
  it' "Links a ./-prefixed workspace member to its root" $ do
    discoveredProjects <- discover workspaceDir
    case discoveredProjects of
      [DiscoveredProject{..}] ->
        findWorkspaceBuildTargets (pkgGraph projectData)
          `shouldBe'` targetSet [BuildTarget "dotslash-root", BuildTarget "pkg-a"]
      _ ->
        expectationFailure' $
          "expected a single workspace project, got " <> show (length discoveredProjects)

  -- "shared" is a transitive of the member's production dep and of the root's
  -- dev tool. If the ./-linked member is severed it is only reachable through
  -- the dev tool, so it loses its production environment and the default filter
  -- drops it.
  it' "Keeps a ./-linked member's production transitive in the production environment" $ do
    discoveredProjects <- discover workspaceDir
    graphs <- traverse (\DiscoveredProject{..} -> dependencyGraph <$> getDeps projectBuildTargets projectData) discoveredProjects
    let sharedEnvs =
          foldMap (foldMap dependencyEnvironments . filter ((== "shared") . dependencyName) . Graphing.vertexList) graphs
    sharedEnvs `shouldSatisfy'` Set.member EnvProduction

selfLoopWorkspaceSpec :: Path Abs Dir -> Spec
selfLoopWorkspaceSpec currDir = describe "Whole-root workspace globs" $ do
  let workspaceDir = currDir </> $(mkRelDir "test/Node/testdata/workspace-selfloop")
  it' "Does not treat a '.' workspace glob as a cycle" $ do
    discoveredProjects <- discover workspaceDir
    case discoveredProjects of
      [DiscoveredProject{..}] ->
        findWorkspaceBuildTargets (pkgGraph projectData)
          `shouldBe'` targetSet [BuildTarget "selfloop-root", BuildTarget "pkg-a"]
      _ ->
        expectationFailure' $
          "expected a single workspace project, got " <> show (length discoveredProjects)

targetSet :: [BuildTarget] -> FoundTargets
targetSet = maybe ProjectWithoutTargets FoundTargets . nonEmpty . Set.fromList

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

resolveNpmV3WorkspacePathsSpec :: Path Abs Dir -> Spec
resolveNpmV3WorkspacePathsSpec currDir = describe "resolveNpmV3WorkspacePaths" $ do
  let graph = workspaceGraphWithDeps currDir
      forTargets names =
        resolveNpmV3WorkspacePaths
          (maybe ProjectWithoutTargets FoundTargets . nonEmpty $ Set.fromList (map BuildTarget names))
          graph

  it "returns Nothing when unscoped" $
    resolveNpmV3WorkspacePaths ProjectWithoutTargets graph `shouldBe` Nothing

  it "maps a workspace name to its root-relative lockfile path key" $
    -- pkg-a lives at ./pkg-a, where the folder basename equals the package name,
    -- so npm omits the lockfile "name"; resolving via package.json still finds it.
    forTargets ["pkg-a"] `shouldBe` Just (Set.fromList ["pkg-a"])

  it "maps the root target to the empty path key" $
    forTargets ["workspace-test"] `shouldBe` Just (Set.fromList [""])

  it "maps a nested workspace name to its nested path key" $
    forTargets ["pkg-b"] `shouldBe` Just (Set.fromList ["nested/pkg-b"])

  it "maps every selected target" $
    forTargets ["workspace-test", "pkg-a", "pkg-b"]
      `shouldBe` Just (Set.fromList ["", "pkg-a", "nested/pkg-b"])

  it "resolves no paths when no target matches a manifest" $
    forTargets ["does-not-exist"] `shouldBe` Just Set.empty

resolvePnpmImporterKeysSpec :: Path Abs Dir -> Spec
resolvePnpmImporterKeysSpec currDir = describe "resolvePnpmImporterKeys" $ do
  let graph = workspaceGraphWithDeps currDir
      forTargets names =
        resolvePnpmImporterKeys
          (maybe ProjectWithoutTargets FoundTargets . nonEmpty $ Set.fromList (map BuildTarget names))
          graph

  it "returns Nothing when unscoped" $
    resolvePnpmImporterKeys ProjectWithoutTargets graph `shouldBe` Nothing

  it "maps the root target to the \".\" importer key" $
    -- pnpm spells the workspace root "." where npm spells it "".
    forTargets ["workspace-test"] `shouldBe` Just (Set.fromList ["."])

  it "maps a workspace name to its root-relative importer key" $
    forTargets ["pkg-b"] `shouldBe` Just (Set.fromList ["nested/pkg-b"])

  it "maps every selected target" $
    forTargets ["workspace-test", "pkg-a", "pkg-b"]
      `shouldBe` Just (Set.fromList [".", "pkg-a", "nested/pkg-b"])

  it "resolves no importers when no target matches a manifest" $
    forTargets ["does-not-exist"] `shouldBe` Just Set.empty

-- | A pnpm workspace root usually keeps its configuration in
-- pnpm-workspace.yaml, so its package.json commonly has no @name@. Such a root
-- must still yield build targets, and the name it is given must be the one the
-- importer-key resolution understands.
unnamedWorkspaceRootSpec :: Path Abs Dir -> Spec
unnamedWorkspaceRootSpec currDir = describe "workspace root without a name" $ do
  let graph = unnamedRootWorkspaceGraph currDir

  it "names the root target after the root directory" $
    workspaceRootTargetName graph `shouldBe` Just "workspace-test"

  it "still exposes the root and every member as build targets" $
    findWorkspaceBuildTargets graph
      `shouldBe` (maybe ProjectWithoutTargets FoundTargets . nonEmpty $ Set.fromList (map BuildTarget ["workspace-test", "pkg-a", "pkg-b"]))

  it "resolves the fallback root target back to the root importer" $
    resolvePnpmImporterKeys
      (maybe ProjectWithoutTargets FoundTargets . nonEmpty $ Set.fromList [BuildTarget "workspace-test"])
      graph
      `shouldBe` Just (Set.fromList ["."])

-- | 'workspaceGraphWithDeps' with the root's @name@ field removed.
unnamedRootWorkspaceGraph :: Path Abs Dir -> PkgJsonGraph
unnamedRootWorkspaceGraph currDir =
  graph{jsonLookup = Map.adjust (\pj -> pj{packageName = Nothing}) (Manifest rootManifest) (jsonLookup graph)}
  where
    graph = workspaceGraphWithDeps currDir
    rootManifest = currDir </> $(mkRelFile "test/Node/testdata/workspace-test/package.json")

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
