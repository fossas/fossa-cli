{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Node.NodeSpec (spec) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Data.Foldable (for_)
import Data.Glob (unsafeGlobRel)
import Data.Map qualified as Map
import Graphing qualified
import Path (Abs, Dir, Path, mkRelDir, mkRelFile, (</>))
import Path.IO (getCurrentDir)
import Strategy.Node (NodeProject (NPMLock), discover, getDeps)
import Strategy.Node.PackageJson (
  Manifest (..),
  PackageJson (
    PackageJson,
    packageDeps,
    packageDevDeps,
    packageLicense,
    packageLicenses,
    packageName,
    packagePeerDeps,
    packageVersion,
    packageWorkspaces,
    packageResolutions
  ),
  PkgJsonGraph (PkgJsonGraph, jsonGraph, jsonLookup),
  PkgJsonLicense (LicenseText),
  PkgJsonWorkspaces (PkgJsonWorkspaces, unWorkspaces),
 )
import Test.Effect (it', shouldBe')
import Test.Hspec (Spec, describe, runIO)
import Types (
  DependencyResults (
    DependencyResults,
    dependencyGraph,
    dependencyGraphBreadth,
    dependencyManifestFiles
  ),
  DiscoveredProject (DiscoveredProject, projectBuildTargets, projectData, projectPath, projectType),
  DiscoveredProjectType (NpmProjectType),
  FoundTargets (ProjectWithoutTargets),
  GraphBreadth (Complete),
 )

spec :: Spec
spec = do
  currDir <- runIO getCurrentDir
  pkgJsonWorkspaceSpec currDir
  npmLockAnalysisSpec currDir

discoveredWorkSpaceProj :: Path Abs Dir -> DiscoveredProject NodeProject
discoveredWorkSpaceProj currDir =
  DiscoveredProject
    { projectType = NpmProjectType
    , projectPath = currDir </> $(mkRelDir "test/Node/testdata/workspace-test/")
    , projectBuildTargets = ProjectWithoutTargets
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
                          , packageResolutions = Map.empty
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
                          , packageResolutions = Map.empty
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
                          , packageResolutions = Map.empty
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
            depGraph <- getDeps projectData
            depGraph `shouldBe'` discoveredWorkSpaceProjDeps currDir
