{-# LANGUAGE TemplateHaskell #-}

module Node.NodeSpec (spec) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Data.Map qualified as Map
import Path
import Path.IO (getCurrentDir)
import Strategy.Node (NodeProject (NPMLock), discover)
import Strategy.Node.PackageJson (Manifest (..), PackageJson (PackageJson, packageDeps, packageDevDeps, packageLicense, packageLicenses, packageName, packagePeerDeps, packageVersion, packageWorkspaces), PkgJsonGraph (PkgJsonGraph, jsonGraph, jsonLookup), PkgJsonWorkspaces (PkgJsonWorkspaces, unWorkspaces), PkgJsonLicense (LicenseText))
import Test.Hspec (Spec, describe, it, runIO)
import Types (DiscoveredProject (DiscoveredProject, projectBuildTargets, projectData, projectPath, projectType), DiscoveredProjectType (NpmProjectType), FoundTargets (ProjectWithoutTargets))
import Data.Glob (unsafeGlobRel)
import Test.Effect (it', shouldBe')

spec :: Spec
spec = do pkgJsonWorkspaceSpec

-- npmLockAnalysisSpec

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
                      ( Manifest
                          { unManifest = workspaceManifest
                          }
                      , Manifest
                          { unManifest = packageBManifest
                          }
                      )
                    ,
                      ( Manifest
                          { unManifest = workspaceManifest
                          }
                      , Manifest
                          { unManifest = packageAManifest
                          }
                      )
                    ]
              , jsonLookup =
                  Map.fromList
                    [
                      ( Manifest
                          { unManifest = packageBManifest
                          }
                      , PackageJson
                          { packageName = Just "pkg-b"
                          , packageVersion = Just "1.0.0"
                          , packageWorkspaces =
                              PkgJsonWorkspaces
                                { unWorkspaces = []
                                }
                          , packageDeps = Map.empty
                          , packageDevDeps = Map.empty
                          , packageLicense =
                              Just
                                (LicenseText "ISC")
                          , packageLicenses = Nothing
                          , packagePeerDeps = Map.empty
                          }
                      )
                    ,
                      ( Manifest
                          { unManifest = workspaceManifest
                          }
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
                          , packageLicense =
                              Just
                                (LicenseText "ISC")
                          , packageLicenses = Nothing
                          , packagePeerDeps = Map.empty
                          }
                      )
                    ,
                      ( Manifest
                          { unManifest = packageAManifest
                          }
                      , PackageJson
                          { packageName = Just "pkg-a"
                          , packageVersion = Just "1.0.0"
                          , packageWorkspaces =
                              PkgJsonWorkspaces
                                { unWorkspaces = []
                                }
                          , packageDeps = Map.empty
                          , packageDevDeps = Map.empty
                          , packageLicense =
                              Just
                                (LicenseText "ISC")
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

pkgJsonWorkspaceSpec :: Spec
pkgJsonWorkspaceSpec = describe "NPM workspace detection" $ do
  currDir <- runIO getCurrentDir
  let workspaceDir = currDir </> $(mkRelDir "test/Node/testdata/workspace-test")
  it' "Discovers manifests for workspaces " $ do
    discoveredManifests <- discover workspaceDir
    discoveredManifests `shouldBe'` [discoveredWorkSpaceProj currDir]

-- npmLockAnalysisSpec :: Spec
-- npmLockAnalysisSpec = do
--   currDir <- runIO getCurrentDir
--   describe "NPM Lock analysis" $ do
--     it "Ignores workspace packages" $ do
--       res <- analyzeProject ( lockFile :: NodeProject)
