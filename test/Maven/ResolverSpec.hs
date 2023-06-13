{-# LANGUAGE TemplateHaskell #-}

module Maven.ResolverSpec (spec) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Data.Map qualified as Map
import Path
import Path.IO (getCurrentDir)
import Strategy.Maven.Pom.PomFile (MavenCoordinate (..), Pom (..))
import Strategy.Maven.Pom.Resolver (GlobalClosure (..), buildClosure)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

spec :: Spec
spec = describe "Maven Resolver" $ do
  currDir <- runIO getCurrentDir
  it "Finds both roots when one module has a parent but is not a submodule." $ do
    buildClosure (testPoms currDir) `shouldBe` (expectedClosure currDir)

springBootStarterParent :: Path Rel File
springBootStarterParent = $(mkRelFile "boot-starter-pom.xml")

minimalExampleWithParent :: Path Rel File
minimalExampleWithParent = $(mkRelFile "minimal-ex-pom.xml")

testPoms :: Path Abs Dir -> Map.Map (Path Abs File) Pom
testPoms dir =
  Map.fromList
    [
      ( dir </> springBootStarterParent
      , Pom
          { pomCoord =
              MavenCoordinate
                { coordGroup = "org.springframework.boot"
                , coordArtifact = "spring-boot-starter-parent"
                , coordVersion = "1.3.5.RELEASE"
                }
          , pomParentCoord =
              Just
                ( MavenCoordinate
                    { coordGroup = "org.springframework.boot"
                    , coordArtifact = "spring-boot-dependencies"
                    , coordVersion = "1.3.5.RELEASE"
                    }
                )
          , pomProperties = Map.empty
          , pomDependencyManagement = Map.empty
          , pomDependencies = Map.empty
          , pomLicenses = []
          , pomModules = []
          }
      )
    ,
      ( dir </> minimalExampleWithParent
      , Pom
          { pomCoord =
              MavenCoordinate
                { coordGroup = "org.springframework.boot"
                , coordArtifact = "SpringBootMavenExample"
                , coordVersion = "1.3.5.RELEASE"
                }
          , pomParentCoord =
              Just
                ( MavenCoordinate
                    { coordGroup = "org.springframework.boot"
                    , coordArtifact = "spring-boot-starter-parent"
                    , coordVersion = "1.3.5.RELEASE"
                    }
                )
          , pomProperties = Map.empty
          , pomDependencyManagement = Map.empty
          , pomDependencies = Map.empty
          , pomLicenses = []
          , pomModules = []
          }
      )
    ]

expectedClosure :: Path Abs Dir -> GlobalClosure
expectedClosure dir =
  GlobalClosure
    { globalGraph =
        AM.vertices
          [ MavenCoordinate
              { coordGroup = "org.springframework.boot"
              , coordArtifact = "SpringBootMavenExample"
              , coordVersion = "1.3.5.RELEASE"
              }
          , MavenCoordinate
              { coordGroup = "org.springframework.boot"
              , coordArtifact = "spring-boot-starter-parent"
              , coordVersion = "1.3.5.RELEASE"
              }
          ]
    , globalPoms =
        Map.fromList
          [
            ( MavenCoordinate
                { coordGroup = "org.springframework.boot"
                , coordArtifact = "SpringBootMavenExample"
                , coordVersion = "1.3.5.RELEASE"
                }
            ,
              ( dir </> minimalExampleWithParent
              , Pom
                  { pomCoord =
                      MavenCoordinate
                        { coordGroup = "org.springframework.boot"
                        , coordArtifact = "SpringBootMavenExample"
                        , coordVersion = "1.3.5.RELEASE"
                        }
                  , pomParentCoord =
                      Just
                        ( MavenCoordinate
                            { coordGroup = "org.springframework.boot"
                            , coordArtifact = "spring-boot-starter-parent"
                            , coordVersion = "1.3.5.RELEASE"
                            }
                        )
                  , pomProperties = Map.empty
                  , pomDependencyManagement = Map.empty
                  , pomDependencies = Map.empty
                  , pomLicenses = []
                  , pomModules = []
                  }
              )
            )
          ,
            ( MavenCoordinate
                { coordGroup = "org.springframework.boot"
                , coordArtifact = "spring-boot-starter-parent"
                , coordVersion = "1.3.5.RELEASE"
                }
            ,
              ( dir </> springBootStarterParent
              , Pom
                  { pomCoord =
                      MavenCoordinate
                        { coordGroup = "org.springframework.boot"
                        , coordArtifact = "spring-boot-starter-parent"
                        , coordVersion = "1.3.5.RELEASE"
                        }
                  , pomParentCoord =
                      Just
                        ( MavenCoordinate
                            { coordGroup = "org.springframework.boot"
                            , coordArtifact = "spring-boot-dependencies"
                            , coordVersion = "1.3.5.RELEASE"
                            }
                        )
                  , pomProperties = Map.empty
                  , pomDependencyManagement = Map.empty
                  , pomDependencies = Map.empty
                  , pomLicenses = []
                  , pomModules = []
                  }
              )
            )
          ]
    }
