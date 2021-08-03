module Maven.DepTreeSpec (spec) where

import Data.Text.IO qualified as TextIO
import Strategy.Maven.DepTree (DotGraph (..), PackageId (..), parseDotGraph)
import Test.Hspec (Spec, describe, it, runIO)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (parse)

-- TODO: Other tests:
--
-- - `toGraph` implementation
-- - Don't leave behind files after running
spec :: Spec
spec =
  describe "mvn dependency:tree" $ do
    dotfile <- runIO $ TextIO.readFile fixtureFile

    it "parses dot files" $
      parse parseDotGraph fixtureFile dotfile `shouldParse` fixtureGraph

fixtureFile :: FilePath
fixtureFile = "test/Maven/testdata/fossa-deptree.dot"

fixtureGraph :: DotGraph
fixtureGraph =
  DotGraph
    { rootNode =
        PackageId
          { groupName = "org.apache.commons"
          , artifactName = "commons-rng-simple"
          , artifactType = "jar"
          , artifactVersion = "1.4-SNAPSHOT"
          , buildTag = Nothing
          }
    , edgeList =
        [
          ( PackageId
              { groupName = "org.apache.commons"
              , artifactName = "commons-rng-simple"
              , artifactType = "jar"
              , artifactVersion = "1.4-SNAPSHOT"
              , buildTag = Nothing
              }
          , PackageId
              { groupName = "org.apache.commons"
              , artifactName = "commons-rng-core"
              , artifactType = "jar"
              , artifactVersion = "1.4-SNAPSHOT"
              , buildTag = Just "compile"
              }
          )
        ,
          ( PackageId
              { groupName = "org.apache.commons"
              , artifactName = "commons-rng-simple"
              , artifactType = "jar"
              , artifactVersion = "1.4-SNAPSHOT"
              , buildTag = Nothing
              }
          , PackageId
              { groupName = "org.apache.commons"
              , artifactName = "commons-math3"
              , artifactType = "jar"
              , artifactVersion = "3.6.1"
              , buildTag = Just "test"
              }
          )
        ,
          ( PackageId
              { groupName = "org.apache.commons"
              , artifactName = "commons-rng-simple"
              , artifactType = "jar"
              , artifactVersion = "1.4-SNAPSHOT"
              , buildTag = Nothing
              }
          , PackageId
              { groupName = "junit"
              , artifactName = "junit"
              , artifactType = "jar"
              , artifactVersion = "4.13.1"
              , buildTag = Just "test"
              }
          )
        ,
          ( PackageId
              { groupName = "org.apache.commons"
              , artifactName = "commons-rng-core"
              , artifactType = "jar"
              , artifactVersion = "1.4-SNAPSHOT"
              , buildTag = Just "compile"
              }
          , PackageId
              { groupName = "org.apache.commons"
              , artifactName = "commons-rng-client-api"
              , artifactType = "jar"
              , artifactVersion = "1.4-SNAPSHOT"
              , buildTag = Just "compile"
              }
          )
        ,
          ( PackageId
              { groupName = "junit"
              , artifactName = "junit"
              , artifactType = "jar"
              , artifactVersion = "4.13.1"
              , buildTag = Just "test"
              }
          , PackageId
              { groupName = "org.hamcrest"
              , artifactName = "hamcrest-core"
              , artifactType = "jar"
              , artifactVersion = "1.3"
              , buildTag = Just "test"
              }
          )
        ]
    }
