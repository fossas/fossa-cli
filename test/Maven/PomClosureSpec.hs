{-# LANGUAGE QuasiQuotes #-}

module Maven.PomClosureSpec (spec) where

import Control.Effect.Lift (sendIO)
import Data.ByteString.Char8 qualified as BS
import Data.List (find, sort)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import GraphUtil (expectDeps')
import Graphing (shrinkRoots)
import Path (Abs, Dir, Path, reldir, relfile, toFilePath, (</>))
import Path.IO qualified as PIO
import Strategy.Maven.Plugin (Artifact (..), Edge (Edge), PluginOutput (..))
import Strategy.Maven.PluginStrategy (buildGraph)
import Strategy.Maven.Pom.Closure (
  MavenProjectClosure (..),
  extractSubmoduleFromCoordinate,
  findProjects,
  submodulesFromCoordinate,
 )
import Strategy.Maven.Pom.PomFile (MavenCoordinate (..))
import Test.Effect (EffectStack, itWithTempDir', shouldBe')
import Test.Hspec

spec :: Spec
spec = do
  describe "extractSubmoduleFromCoordinate" $ do
    it "should extract group:artifact from a coordinate" $ do
      extractSubmoduleFromCoordinate (MavenCoordinate "org.apache.poi" "poi-ooxml" "5.2.5")
        `shouldBe` "org.apache.poi:poi-ooxml"

    it "should ignore the version component" $ do
      extractSubmoduleFromCoordinate (MavenCoordinate "g" "a" "9.9.9")
        `shouldBe` "g:a"

  describe "submodulesFromCoordinate" $ do
    it "should return an empty set for an empty map" $ do
      submodulesFromCoordinate (Map.empty :: Map.Map MavenCoordinate ()) `shouldBe` Set.empty

    it "should extract a single submodule from a one-entry map" $ do
      let m = Map.singleton (MavenCoordinate "g" "a" "v") ()
      submodulesFromCoordinate m `shouldBe` Set.fromList ["g:a"]

    it "should deduplicate coordinates with the same group and artifact" $ do
      let m =
            Map.fromList
              [ (MavenCoordinate "g" "a" "1.0", ())
              , (MavenCoordinate "g" "a" "2.0", ()) -- same group:artifact, different version
              ]
      submodulesFromCoordinate m `shouldBe` Set.fromList ["g:a"]

    it "should include multiple distinct coordinates" $ do
      let m =
            Map.fromList
              [ (MavenCoordinate "org.apache.poi" "poi" "5.2.5", ())
              , (MavenCoordinate "org.apache.logging.log4j" "log4j-core" "2.21.1", ())
              , (MavenCoordinate "com.google.guava" "guava" "31.1", ())
              ]
      submodulesFromCoordinate m
        `shouldBe` Set.fromList
          [ "org.apache.poi:poi"
          , "org.apache.logging.log4j:log4j-core"
          , "com.google.guava:guava"
          ]
  describe "findProjects (parentless <modules> children)" $ do
    -- Pins the parentless-module regression: a module listed in an ancestor's
    -- <modules> but with no <parent> element is legal Maven, but the closure
    -- graph currently only seeds <parent> edges, so such a module is a
    -- disconnected vertex and missing from the aggregator's closure until
    -- <modules> edges are seeded into buildClosure.
    itWithTempDir' "includes parentless <module> children in the aggregator's closureSubmodules" $ \dir -> do
      createParentlessFixture dir
      closures <- findProjects dir
      rootSubmodules closures
        `shouldBe'` Set.fromList
          [ "com.example:child-parented"
          , "com.example:child-parentless"
          , "com.example:root-a"
          ]
    itWithTempDir' "discovers a single project closure for a tree with a parentless module" $ \dir -> do
      createParentlessFixture dir
      closures <- findProjects dir
      -- Once <modules> edges are seeded, the parentless module folds into the
      -- aggregator's closure and is no longer a standalone source vertex, so
      -- exactly one project closure should be discovered for this tree (today
      -- it is its own separate MavenProjectClosure, giving two).
      sort (map closureRootCoord closures) `shouldBe'` [rootCoord]
    itWithTempDir' "does not leak the parentless module into the final dependency graph" $ \dir -> do
      createParentlessFixture dir
      closures <- findProjects dir
      let submodules = rootSubmodules closures
          -- Maven's reactor reports both children as artifacts in the verbose
          -- dependency graph; the closure must mark them first-party so
          -- buildGraph promotes them to direct and shrinkRoots removes them.
          -- Modeled reactor edge (parented -> parentless, synthetic: the fixture
          -- POMs declare no such dependency); it puts both modules into the
          -- reported graph, as depgraph's aggregate output would for a reactor
          -- where one module depends on another.
          output =
            PluginOutput
              { outArtifacts = [childParentedArtifact, childParentlessArtifact]
              , outEdges = [Edge 0 1]
              }
          graph = shrinkRoots $ buildGraph submodules output
      expectDeps' [] graph

rootCoord :: MavenCoordinate
rootCoord = MavenCoordinate "com.example" "root-a" "1.0"

-- | The closureSubmodules of the project closure rooted at @rootCoord@.
-- Falls back to empty if that closure is absent, so each test's primary
-- assertion reports what it expected instead of erroring during lookup.
rootSubmodules :: [MavenProjectClosure] -> Set.Set Text
rootSubmodules = maybe Set.empty closureSubmodules . find ((== rootCoord) . closureRootCoord)

childParentedArtifact :: Artifact
childParentedArtifact =
  Artifact
    { artifactNumericId = 0
    , artifactGroupId = "com.example"
    , artifactArtifactId = "child-parented"
    , artifactVersion = "1.0"
    , artifactOptional = False
    , artifactScopes = ["compile"]
    , artifactIsDirect = False
    }

childParentlessArtifact :: Artifact
childParentlessArtifact =
  Artifact
    { artifactNumericId = 1
    , artifactGroupId = "com.example"
    , artifactArtifactId = "child-parentless"
    , artifactVersion = "1.0"
    , artifactOptional = False
    , artifactScopes = ["compile"]
    , artifactIsDirect = False
    }

-- | Writes the fixture tree into @dir@ (paths are derived at runtime so no
-- checked-in/absolute fixture path is hardcoded):
--
-- @
--   pom.xml                -- aggregator, com.example:root-a:1.0, packaging=pom,
--                           -- <modules> lists both children
--   child-parented/pom.xml -- has <parent> pointing at root-a
--   child-parentless/pom.xml -- NO <parent> element (legal Maven)
-- @
createParentlessFixture :: Path Abs Dir -> EffectStack ()
createParentlessFixture dir = do
  sendIO $ PIO.createDirIfMissing True (dir </> [reldir|child-parented|])
  sendIO $ PIO.createDirIfMissing True (dir </> [reldir|child-parentless|])
  sendIO $ BS.writeFile (toFilePath (dir </> [relfile|pom.xml|])) rootPom
  sendIO $ BS.writeFile (toFilePath (dir </> [reldir|child-parented|] </> [relfile|pom.xml|])) parentedChildPom
  sendIO $ BS.writeFile (toFilePath (dir </> [reldir|child-parentless|] </> [relfile|pom.xml|])) parentlessChildPom

-- | Concatenates lines of an XML document.
packLines :: [String] -> BS.ByteString
packLines = BS.concat . map BS.pack

-- | The aggregator lists both children via <modules>; only child-parented
-- declares a matching <parent> element.
rootPom :: BS.ByteString
rootPom =
  packLines
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    , "<project xmlns=\"http://maven.apache.org/POM/4.0.0\">\n"
    , "  <modelVersion>4.0.0</modelVersion>\n"
    , "  <groupId>com.example</groupId>\n"
    , "  <artifactId>root-a</artifactId>\n"
    , "  <version>1.0</version>\n"
    , "  <packaging>pom</packaging>\n"
    , "  <modules>\n"
    , "    <module>child-parented</module>\n"
    , "    <module>child-parentless</module>\n"
    , "  </modules>\n"
    , "</project>\n"
    ]

parentedChildPom :: BS.ByteString
parentedChildPom =
  packLines
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    , "<project xmlns=\"http://maven.apache.org/POM/4.0.0\">\n"
    , "  <modelVersion>4.0.0</modelVersion>\n"
    , "  <parent>\n"
    , "    <groupId>com.example</groupId>\n"
    , "    <artifactId>root-a</artifactId>\n"
    , "    <version>1.0</version>\n"
    , "    <relativePath>../pom.xml</relativePath>\n"
    , "  </parent>\n"
    , "  <artifactId>child-parented</artifactId>\n"
    , "</project>\n"
    ]

-- | No <parent> element: this child is only linked to the aggregator via the
-- aggregator's <modules> listing, which is the regression case.
parentlessChildPom :: BS.ByteString
parentlessChildPom =
  packLines
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    , "<project xmlns=\"http://maven.apache.org/POM/4.0.0\">\n"
    , "  <modelVersion>4.0.0</modelVersion>\n"
    , "  <groupId>com.example</groupId>\n"
    , "  <artifactId>child-parentless</artifactId>\n"
    , "  <version>1.0</version>\n"
    , "</project>\n"
    ]
