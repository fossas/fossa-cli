module Maven.DepTreeSpec (spec) where

import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.IO qualified as TextIO
import DepTypes (
  DepEnvironment (..),
  DepType (MavenType),
  Dependency (..),
  VerConstraint (..),
 )
import GraphUtil
import Graphing (shrinkRoots)
import Strategy.Maven.Common (MavenDependency (..))
import Strategy.Maven.DepTree (
  DotGraph (..),
  PackageId (..),
  buildGraph,
  parseDotGraphs,
  toDependency,
 )
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.Megaparsec (shouldParse, shouldSucceedOn)
import Text.Megaparsec (parse)

-- TODO: Other tests:
--
-- - `toGraph` implementation
-- - Don't leave behind files after running
spec :: Spec
spec =
  describe "mvn dependency:tree" $ do
    single <- runIO $ TextIO.readFile fixtureSingleFile
    it "parses single dot graphs" $
      parse parseDotGraphs fixtureSingleFile single `shouldParse` fixtureSingleGraph

    it "should build dependency graph without project as dependency" $ do
      -- Setup
      let depRngCore =
            Dependency
              MavenType
              "org.apache.commons:commons-rng-core"
              (Just $ CEq "1.4-SNAPSHOT")
              []
              (Set.singleton EnvProduction)
              (mempty)
      let mavenDepRngCore = MavenDependency depRngCore (Set.fromList ["compile"])

      let depMath3 =
            Dependency
              MavenType
              "org.apache.commons:commons-math3"
              (Just $ CEq "3.6.1")
              []
              (Set.singleton EnvTesting)
              (mempty)
      let mavenDepMath3 = MavenDependency depMath3 (Set.fromList ["test"])

      let depJunit =
            Dependency
              MavenType
              "junit:junit"
              (Just $ CEq "4.13.1")
              []
              (Set.singleton EnvTesting)
              (mempty)
      let mavenDepJunit = MavenDependency depJunit (Set.fromList ["test"])

      let depRngClientApi =
            Dependency
              MavenType
              "org.apache.commons:commons-rng-client-api"
              (Just $ CEq "1.4-SNAPSHOT")
              []
              (Set.singleton EnvProduction)
              (mempty)
      let mavenDepRngClientApi = MavenDependency depRngClientApi (Set.fromList ["compile"])

      let depHamcrestCore =
            Dependency
              MavenType
              "org.hamcrest:hamcrest-core"
              (Just $ CEq "1.3")
              []
              (Set.singleton EnvTesting)
              (mempty)
      let mavenDepHamcrestCore = MavenDependency depHamcrestCore (Set.fromList ["test"])

      -- Act
      -- NOTE: Previously shrinkRoots was applied at this level, but it has now been moved upstream to allow for submodule filtering
      --       Adding shrinkRoots to our buildGraph function to mimic prior behavior
      let graph = shrinkRoots $ buildGraph fixtureSingleGraph

      -- Assert
      expectDeps [mavenDepRngCore, mavenDepMath3, mavenDepJunit, mavenDepRngClientApi, mavenDepHamcrestCore] graph
      expectDirect [mavenDepRngCore, mavenDepMath3, mavenDepJunit] graph
      expectEdges [(mavenDepRngCore, mavenDepRngClientApi), (mavenDepJunit, mavenDepHamcrestCore)] graph

    multi <- runIO $ TextIO.readFile fixtureMultiFile
    it "parses multiple dot graphs in one file" $
      parse parseDotGraphs fixtureMultiFile multi `shouldParse` fixtureMultiGraph

    acme <- runIO $ TextIO.readFile fixtureAcmeFile
    it "parses real-world test fixture" $
      parse parseDotGraphs fixtureAcmeFile `shouldSucceedOn` acme

    it "parses package IDs with platforms" $
      parse parseDotGraphs "" fixturePackageIDWithPlatformContents
        `shouldParse` fixturePackageIDWithPlatformGraph

    it "renders parsed dependencies" $ do
      let p =
            PackageId
              { groupName = "org.apache.commons"
              , artifactName = "commons-rng-parent"
              , artifactType = "pom"
              , artifactVersion = "1.4-SNAPSHOT"
              , artifactPlatform = Nothing
              , buildTag = Nothing
              }
      let dep =
            Dependency
              { dependencyType = MavenType
              , dependencyName = "org.apache.commons:commons-rng-parent"
              , dependencyVersion = Just (CEq "1.4-SNAPSHOT")
              , dependencyLocations = []
              , dependencyEnvironments = Set.singleton EnvProduction
              , dependencyTags = mempty
              }
      let d = MavenDependency dep (Set.fromList [])
      toDependency p `shouldBe` d

fixtureSingleFile :: FilePath
fixtureSingleFile = "test/Maven/testdata/fossa-deptree.dot"

fixtureSingleGraph :: [DotGraph]
fixtureSingleGraph =
  [ DotGraph
      { rootNode =
          PackageId
            { groupName = "org.apache.commons"
            , artifactName = "commons-rng-simple"
            , artifactType = "jar"
            , artifactVersion = "1.4-SNAPSHOT"
            , artifactPlatform = Nothing
            , buildTag = Nothing
            }
      , edgeList =
          [
            ( PackageId
                { groupName = "org.apache.commons"
                , artifactName = "commons-rng-simple"
                , artifactType = "jar"
                , artifactVersion = "1.4-SNAPSHOT"
                , artifactPlatform = Nothing
                , buildTag = Nothing
                }
            , PackageId
                { groupName = "org.apache.commons"
                , artifactName = "commons-rng-core"
                , artifactType = "jar"
                , artifactVersion = "1.4-SNAPSHOT"
                , artifactPlatform = Nothing
                , buildTag = Just "compile"
                }
            )
          ,
            ( PackageId
                { groupName = "org.apache.commons"
                , artifactName = "commons-rng-simple"
                , artifactType = "jar"
                , artifactVersion = "1.4-SNAPSHOT"
                , artifactPlatform = Nothing
                , buildTag = Nothing
                }
            , PackageId
                { groupName = "org.apache.commons"
                , artifactName = "commons-math3"
                , artifactType = "jar"
                , artifactVersion = "3.6.1"
                , artifactPlatform = Nothing
                , buildTag = Just "test"
                }
            )
          ,
            ( PackageId
                { groupName = "org.apache.commons"
                , artifactName = "commons-rng-simple"
                , artifactType = "jar"
                , artifactVersion = "1.4-SNAPSHOT"
                , artifactPlatform = Nothing
                , buildTag = Nothing
                }
            , PackageId
                { groupName = "junit"
                , artifactName = "junit"
                , artifactType = "jar"
                , artifactVersion = "4.13.1"
                , artifactPlatform = Nothing
                , buildTag = Just "test"
                }
            )
          ,
            ( PackageId
                { groupName = "org.apache.commons"
                , artifactName = "commons-rng-core"
                , artifactType = "jar"
                , artifactVersion = "1.4-SNAPSHOT"
                , artifactPlatform = Nothing
                , buildTag = Just "compile"
                }
            , PackageId
                { groupName = "org.apache.commons"
                , artifactName = "commons-rng-client-api"
                , artifactType = "jar"
                , artifactVersion = "1.4-SNAPSHOT"
                , artifactPlatform = Nothing
                , buildTag = Just "compile"
                }
            )
          ,
            ( PackageId
                { groupName = "junit"
                , artifactName = "junit"
                , artifactType = "jar"
                , artifactVersion = "4.13.1"
                , artifactPlatform = Nothing
                , buildTag = Just "test"
                }
            , PackageId
                { groupName = "org.hamcrest"
                , artifactName = "hamcrest-core"
                , artifactType = "jar"
                , artifactVersion = "1.3"
                , artifactPlatform = Nothing
                , buildTag = Just "test"
                }
            )
          ]
      }
  ]

fixtureMultiFile :: FilePath
fixtureMultiFile = "test/Maven/testdata/fossa-multideptree.dot"

fixtureMultiGraph :: [DotGraph]
fixtureMultiGraph =
  [ DotGraph
      { rootNode = PackageId{groupName = "org.apache.commons", artifactName = "commons-rng-parent", artifactType = "pom", artifactVersion = "1.4-SNAPSHOT", artifactPlatform = Nothing, buildTag = Nothing}
      , edgeList =
          [
            ( PackageId{groupName = "org.apache.commons", artifactName = "commons-rng-parent", artifactType = "pom", artifactVersion = "1.4-SNAPSHOT", artifactPlatform = Nothing, buildTag = Nothing}
            , PackageId{groupName = "junit", artifactName = "junit", artifactType = "jar", artifactVersion = "4.13.1", artifactPlatform = Nothing, buildTag = Just "test"}
            )
          ,
            ( PackageId{groupName = "junit", artifactName = "junit", artifactType = "jar", artifactVersion = "4.13.1", artifactPlatform = Nothing, buildTag = Just "test"}
            , PackageId{groupName = "org.hamcrest", artifactName = "hamcrest-core", artifactType = "jar", artifactVersion = "1.3", artifactPlatform = Nothing, buildTag = Just "test"}
            )
          ]
      }
  , DotGraph
      { rootNode = PackageId{groupName = "org.apache.commons", artifactName = "commons-rng-client-api", artifactType = "jar", artifactVersion = "1.4-SNAPSHOT", artifactPlatform = Nothing, buildTag = Nothing}
      , edgeList =
          [ (PackageId{groupName = "org.apache.commons", artifactName = "commons-rng-client-api", artifactType = "jar", artifactVersion = "1.4-SNAPSHOT", artifactPlatform = Nothing, buildTag = Nothing}, PackageId{groupName = "junit", artifactName = "junit", artifactType = "jar", artifactVersion = "4.13.1", artifactPlatform = Nothing, buildTag = Just "test"})
          ,
            ( PackageId{groupName = "junit", artifactName = "junit", artifactType = "jar", artifactVersion = "4.13.1", artifactPlatform = Nothing, buildTag = Just "test"}
            , PackageId{groupName = "org.hamcrest", artifactName = "hamcrest-core", artifactType = "jar", artifactVersion = "1.3", artifactPlatform = Nothing, buildTag = Just "test"}
            )
          ]
      }
  , DotGraph
      { rootNode = PackageId{groupName = "org.apache.commons", artifactName = "commons-rng-core", artifactType = "jar", artifactVersion = "1.4-SNAPSHOT", artifactPlatform = Nothing, buildTag = Nothing}
      , edgeList =
          [
            ( PackageId{groupName = "org.apache.commons", artifactName = "commons-rng-core", artifactType = "jar", artifactVersion = "1.4-SNAPSHOT", artifactPlatform = Nothing, buildTag = Nothing}
            , PackageId{groupName = "org.apache.commons", artifactName = "commons-rng-client-api", artifactType = "jar", artifactVersion = "1.4-SNAPSHOT", artifactPlatform = Nothing, buildTag = Just "compile"}
            )
          ,
            ( PackageId{groupName = "org.apache.commons", artifactName = "commons-rng-core", artifactType = "jar", artifactVersion = "1.4-SNAPSHOT", artifactPlatform = Nothing, buildTag = Nothing}
            , PackageId{groupName = "org.apache.commons", artifactName = "commons-math3", artifactType = "jar", artifactVersion = "3.6.1", artifactPlatform = Nothing, buildTag = Just "test"}
            )
          ,
            ( PackageId{groupName = "org.apache.commons", artifactName = "commons-rng-core", artifactType = "jar", artifactVersion = "1.4-SNAPSHOT", artifactPlatform = Nothing, buildTag = Nothing}
            , PackageId{groupName = "junit", artifactName = "junit", artifactType = "jar", artifactVersion = "4.13.1", artifactPlatform = Nothing, buildTag = Just "test"}
            )
          ,
            ( PackageId{groupName = "junit", artifactName = "junit", artifactType = "jar", artifactVersion = "4.13.1", artifactPlatform = Nothing, buildTag = Just "test"}
            , PackageId{groupName = "org.hamcrest", artifactName = "hamcrest-core", artifactType = "jar", artifactVersion = "1.3", artifactPlatform = Nothing, buildTag = Just "test"}
            )
          ]
      }
  , DotGraph
      { rootNode = PackageId{groupName = "org.apache.commons", artifactName = "commons-rng-simple", artifactType = "jar", artifactVersion = "1.4-SNAPSHOT", artifactPlatform = Nothing, buildTag = Nothing}
      , edgeList =
          [
            ( PackageId{groupName = "org.apache.commons", artifactName = "commons-rng-simple", artifactType = "jar", artifactVersion = "1.4-SNAPSHOT", artifactPlatform = Nothing, buildTag = Nothing}
            , PackageId{groupName = "org.apache.commons", artifactName = "commons-rng-core", artifactType = "jar", artifactVersion = "1.4-SNAPSHOT", artifactPlatform = Nothing, buildTag = Just "compile"}
            )
          ,
            ( PackageId{groupName = "org.apache.commons", artifactName = "commons-rng-simple", artifactType = "jar", artifactVersion = "1.4-SNAPSHOT", artifactPlatform = Nothing, buildTag = Nothing}
            , PackageId{groupName = "org.apache.commons", artifactName = "commons-math3", artifactType = "jar", artifactVersion = "3.6.1", artifactPlatform = Nothing, buildTag = Just "test"}
            )
          ,
            ( PackageId{groupName = "org.apache.commons", artifactName = "commons-rng-simple", artifactType = "jar", artifactVersion = "1.4-SNAPSHOT", artifactPlatform = Nothing, buildTag = Nothing}
            , PackageId{groupName = "junit", artifactName = "junit", artifactType = "jar", artifactVersion = "4.13.1", artifactPlatform = Nothing, buildTag = Just "test"}
            )
          ,
            ( PackageId{groupName = "org.apache.commons", artifactName = "commons-rng-core", artifactType = "jar", artifactVersion = "1.4-SNAPSHOT", artifactPlatform = Nothing, buildTag = Just "compile"}
            , PackageId{groupName = "org.apache.commons", artifactName = "commons-rng-client-api", artifactType = "jar", artifactVersion = "1.4-SNAPSHOT", artifactPlatform = Nothing, buildTag = Just "compile"}
            )
          ,
            ( PackageId{groupName = "junit", artifactName = "junit", artifactType = "jar", artifactVersion = "4.13.1", artifactPlatform = Nothing, buildTag = Just "test"}
            , PackageId{groupName = "org.hamcrest", artifactName = "hamcrest-core", artifactType = "jar", artifactVersion = "1.3", artifactPlatform = Nothing, buildTag = Just "test"}
            )
          ]
      }
  , DotGraph
      { rootNode = PackageId{groupName = "org.apache.commons", artifactName = "commons-rng-sampling", artifactType = "jar", artifactVersion = "1.4-SNAPSHOT", artifactPlatform = Nothing, buildTag = Nothing}
      , edgeList =
          [
            ( PackageId{groupName = "org.apache.commons", artifactName = "commons-rng-sampling", artifactType = "jar", artifactVersion = "1.4-SNAPSHOT", artifactPlatform = Nothing, buildTag = Nothing}
            , PackageId{groupName = "org.apache.commons", artifactName = "commons-rng-client-api", artifactType = "jar", artifactVersion = "1.4-SNAPSHOT", artifactPlatform = Nothing, buildTag = Just "compile"}
            )
          ,
            ( PackageId{groupName = "org.apache.commons", artifactName = "commons-rng-sampling", artifactType = "jar", artifactVersion = "1.4-SNAPSHOT", artifactPlatform = Nothing, buildTag = Nothing}
            , PackageId{groupName = "org.apache.commons", artifactName = "commons-rng-simple", artifactType = "jar", artifactVersion = "1.4-SNAPSHOT", artifactPlatform = Nothing, buildTag = Just "test"}
            )
          ,
            ( PackageId{groupName = "org.apache.commons", artifactName = "commons-rng-sampling", artifactType = "jar", artifactVersion = "1.4-SNAPSHOT", artifactPlatform = Nothing, buildTag = Nothing}
            , PackageId{groupName = "org.apache.commons", artifactName = "commons-math3", artifactType = "jar", artifactVersion = "3.6.1", artifactPlatform = Nothing, buildTag = Just "test"}
            )
          ,
            ( PackageId{groupName = "org.apache.commons", artifactName = "commons-rng-sampling", artifactType = "jar", artifactVersion = "1.4-SNAPSHOT", artifactPlatform = Nothing, buildTag = Nothing}
            , PackageId{groupName = "junit", artifactName = "junit", artifactType = "jar", artifactVersion = "4.13.1", artifactPlatform = Nothing, buildTag = Just "test"}
            )
          ,
            ( PackageId{groupName = "org.apache.commons", artifactName = "commons-rng-simple", artifactType = "jar", artifactVersion = "1.4-SNAPSHOT", artifactPlatform = Nothing, buildTag = Just "test"}
            , PackageId{groupName = "org.apache.commons", artifactName = "commons-rng-core", artifactType = "jar", artifactVersion = "1.4-SNAPSHOT", artifactPlatform = Nothing, buildTag = Just "test"}
            )
          ,
            ( PackageId{groupName = "junit", artifactName = "junit", artifactType = "jar", artifactVersion = "4.13.1", artifactPlatform = Nothing, buildTag = Just "test"}
            , PackageId{groupName = "org.hamcrest", artifactName = "hamcrest-core", artifactType = "jar", artifactVersion = "1.3", artifactPlatform = Nothing, buildTag = Just "test"}
            )
          ]
      }
  ]

fixtureAcmeFile :: FilePath
fixtureAcmeFile = "test/Maven/testdata/acme-deptree.dot"

fixturePackageIDWithPlatformContents :: Text
fixturePackageIDWithPlatformContents =
  "digraph \"com.fossa:fixture:pom:1.2.3\" {\
  \  \"com.fossa:fixture:pom:1.2.3\" -> \"com.fossa:platform-specific-dep:jar:linux-x86_64:4.5.6:compile\" ;\
  \}"

fixturePackageIDWithPlatformGraph :: [DotGraph]
fixturePackageIDWithPlatformGraph =
  [DotGraph{rootNode = root, edgeList = [(root, platformID)]}]
  where
    root =
      PackageId
        { groupName = "com.fossa"
        , artifactName = "fixture"
        , artifactType = "pom"
        , artifactVersion = "1.2.3"
        , artifactPlatform = Nothing
        , buildTag = Nothing
        }
    platformID =
      PackageId
        { groupName = "com.fossa"
        , artifactName = "platform-specific-dep"
        , artifactType = "jar"
        , artifactVersion = "4.5.6"
        , artifactPlatform = Just "linux-x86_64"
        , buildTag = Just "compile"
        }
