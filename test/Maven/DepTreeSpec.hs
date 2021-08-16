module Maven.DepTreeSpec (spec) where

import Data.Text (Text)
import Data.Text.IO qualified as TextIO
import Strategy.Maven.DepTree (DotGraph (..), PackageId (..), parseDotGraphs)
import Test.Hspec (Spec, describe, it, runIO)
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

    multi <- runIO $ TextIO.readFile fixtureMultiFile
    it "parses multiple dot graphs in one file" $
      parse parseDotGraphs fixtureMultiFile multi `shouldParse` fixtureMultiGraph

    acme <- runIO $ TextIO.readFile fixtureAcmeFile
    it "parses real-world test fixture" $
      parse parseDotGraphs fixtureAcmeFile `shouldSucceedOn` acme

    it "parses package IDs with platforms" $
      parse parseDotGraphs "" fixturePackageIDWithPlatformContents
        `shouldParse` fixturePackageIDWithPlatformGraph

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
