{-# LANGUAGE QuasiQuotes #-}

module Maven.PomClosureSpec (spec) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Control.Carrier.State.Strict qualified as S (execState, modify)
import Control.Effect.Lift (sendIO)
import Control.Exception (SomeException, try)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (traverse_)
import Data.List (find, isSuffixOf, sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Discovery.Walk (WalkStep (WalkSkipSome), fileName, walkWithFilters')
import Effect.ReadFS (doesFileExist)
import GraphUtil (expectDeps')
import Graphing (shrinkRoots)
import Path (Abs, Dir, File, Path, Rel, reldir, relfile, toFilePath, (</>))
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
import Strategy.Maven.Pom.Resolver (buildGlobalClosure, globalGraph, globalPoms)
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
  -- TEMPORARY DIAGNOSTIC (remove once the Windows CI failure is localized):
  -- one fixture, staged assertions. On Windows the first failing stage tells
  -- us exactly where discovery diverges from Linux.
  itWithTempDir' "DIAG staged closure discovery (temporary)" $ \dir -> do
    createParentlessFixture dir
    let poms = fixturePomPaths dir
        expectedCoords = [rootCoord, childParentedCoord, childParentlessCoord]

    -- Stage 1: everything we wrote is readable back from the same paths
    exists <- traverse doesFileExist poms
    shouldBe'
      (zip (map toFilePath poms) exists)
      (zip (map toFilePath poms) [True, True, True])

    -- Stage 2: the discovery walk finds exactly the three fixture poms
    -- (mirrors findPomFiles, including filters and the target/ skip)
    walked :: [Path Abs File] <-
      S.execState [] $ flip walkWithFilters' dir $ \_ _ files -> do
        let found = filter (\f -> "pom.xml" `isSuffixOf` fileName f || ".pom" `isSuffixOf` fileName f) files
        traverse_ (S.modify . (:)) found
        pure ((), WalkSkipSome ["target"])
    shouldBe' (sort walked) (sort poms)

    -- Stage 3: every pom loads and both edge kinds are present in the graph
    closure <- buildGlobalClosure walked
    shouldBe' (Map.size (globalPoms closure)) 3
    shouldBe' (sort (AM.vertexList (globalGraph closure))) (sort expectedCoords)
    shouldBe'
      (sort (AM.edgeList (globalGraph closure)))
      [ (rootCoord, childParentedCoord) -- <parent> edge
      , (rootCoord, childParentlessCoord) -- <modules> edge
      ]

    -- Stage 3b: determineProjectRoots keeps a root only if PIO.makeRelative
    -- succeeds for its pom (it throws otherwise; string-based under the hood).
    -- Assert per-pom so a failure shows which path failed and why.
    rels :: [(String, String)] <-
      mapM
        ( \p ->
            sendIO $ do
              r <- try (PIO.makeRelative dir p) :: IO (Either SomeException (Path Rel File))
              pure (toFilePath p, either (("threw: " ++) . show) (const "ok") r)
        )
        walked
    shouldBe' rels [(toFilePath p, "ok") | p <- walked]

    -- Stage 3c: determineProjectRoots actually compares the basedir against the
    -- paths stored in globalPoms. loadParent/loadSubmodules re-resolve poms via
    -- resolveFile (OS canonicalization), so a coordinate may be stored under a
    -- string-different path than the walked one; assert each stored path still
    -- relativizes under dir.
    let storedPaths :: [Path Abs File]
        storedPaths = mapMaybe (\c -> fst <$> Map.lookup c (globalPoms closure)) expectedCoords
    storedRels :: [(String, String)] <-
      mapM
        ( \p ->
            sendIO $ do
              r <- try (PIO.makeRelative dir p) :: IO (Either SomeException (Path Rel File))
              pure ("dir=" ++ toFilePath dir ++ " pom=" ++ toFilePath p, either (("threw: " ++) . show) (const "ok") r)
        )
        storedPaths
    shouldBe' storedRels [("dir=" ++ toFilePath dir ++ " pom=" ++ s, "ok") | s <- map toFilePath storedPaths]

    -- Stage 4: project-root selection keeps exactly the aggregator
    closures <- findProjects dir
    shouldBe' (sort (map closureRootCoord closures)) [rootCoord]
  describe "findProjects (parentless <modules> children)" $ do
    -- A module listed in an ancestor's <modules> but with no <parent> element is
    -- legal Maven. These tests pin that the closure graph carries <modules> edges:
    -- without them such a module is a disconnected vertex, missing from the
    -- aggregator's closureSubmodules, discovered as its own standalone project,
    -- and it leaks into the reported dependency graph as a fake external package.
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
      -- The parentless module belongs to the aggregator's closure rather than
      -- being its own standalone source vertex, so exactly one project closure
      -- is discovered for this tree; a second root here means <modules> edges
      -- are missing.
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

childParentedCoord :: MavenCoordinate
childParentedCoord = MavenCoordinate "com.example" "child-parented" "1.0"

childParentlessCoord :: MavenCoordinate
childParentlessCoord = MavenCoordinate "com.example" "child-parentless" "1.0"

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
-- | The three fixture pom locations.
fixturePomPaths :: Path Abs Dir -> [Path Abs File]
fixturePomPaths dir =
  [ dir </> [relfile|pom.xml|]
  , dir </> [reldir|child-parented|] </> [relfile|pom.xml|]
  , dir </> [reldir|child-parentless|] </> [relfile|pom.xml|]
  ]

createParentlessFixture :: Path Abs Dir -> EffectStack ()
createParentlessFixture dir = do
  sendIO $ PIO.createDirIfMissing True (dir </> [reldir|child-parented|])
  sendIO $ PIO.createDirIfMissing True (dir </> [reldir|child-parentless|])
  traverse_ (\(path, contents) -> sendIO (BS.writeFile (toFilePath path) contents)) $
    zip (fixturePomPaths dir) [rootPom, parentedChildPom, parentlessChildPom]

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
