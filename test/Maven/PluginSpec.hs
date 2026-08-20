{-# LANGUAGE QuasiQuotes #-}

module Maven.PluginSpec (spec) where

import Control.Effect.Lift (sendIO)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Tree (Tree (..))
import Path (Abs, File, Path, parent, reldir, relfile, toFilePath, (</>))
import Path.IO qualified as PIO
import Strategy.Maven.Plugin (
  Artifact (..),
  Edge (..),
  PluginOutput (..),
  VerboseArtifact (..),
  VerboseEdge (..),
  VerboseGraph (..),
  augmentWithDuplicateEdges,
  deriveVerboseGraphPaths,
  parsePluginOutput,
  parseVerboseGraphs,
  textArtifactToPluginOutput,
 )
import Strategy.Maven.PluginTree (TextArtifact (..), parseTextArtifact)
import Strategy.Maven.Pom.PomFile (MavenCoordinate (..), Pom (..), PomBuild (..))
import Test.Effect (
  expectFatal',
  expectationFailure',
  it',
  itWithTempDir',
  shouldBe',
  shouldContain',
  shouldMatchList',
  shouldSatisfy',
 )
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parseMaybe)
import Text.RawString.QQ (r)

spec :: Spec
spec = do
  textArtifactConversionSpec
  verboseGraphParsingSpec
  augmentWithDuplicateEdgesSpec
  verboseGraphCollectionSpec
  deriveVerboseGraphPathsSpec
  parsePluginOutputSpec

-- | The @graph@ goal is not an aggregator: in a multi-module build it runs once
-- per reactor module, writing into each module's own build directory.
-- 'parseVerboseGraphs' must collect every such file — from derived locations
-- when possible, via a tree-walk fallback only when derivation fails or an
-- expected file is absent (the stray decoy proves whether the walk ran).
verboseGraphCollectionSpec :: Spec
verboseGraphCollectionSpec = do
  let fossaFile = [relfile|fossa-depgraph-verbose.json|]
      coordA = MavenCoordinate "g" "a" "1.0.0"
      coordC = MavenCoordinate "g" "c" "1.0.0"
      pomEntry coord' path builds =
        ( path
        , Pom
            { pomCoord = coord'
            , pomParentCoord = Nothing
            , pomProperties = Map.empty
            , pomDependencyManagement = Map.empty
            , pomDependencies = Map.empty
            , pomLicenses = []
            , pomBuilds = builds
            }
        )
      closurePoms tmpdir =
        Map.fromList
          [ (coordA, pomEntry coordA (tmpdir </> [reldir|mod-a/|] </> [relfile|pom.xml|]) Map.empty)
          , (coordC, pomEntry coordC (tmpdir </> [reldir|mod-c/|] </> [relfile|pom.xml|]) Map.empty)
          ]
      -- Lives where no closure module could write it: only the walk fallback can find it.
      decoyFile tmpdir = tmpdir </> [reldir|stray/|] </> fossaFile
      writeJson path contents = do
        sendIO $ PIO.createDirIfMissing True (parent path)
        sendIO $ BS.writeFile (toFilePath path) contents
  describe "parseVerboseGraphs" $ do
    itWithTempDir' "collects each module's graph from derived locations without walking the tree" $ \tmpdir -> do
      writeJson (tmpdir </> [reldir|mod-a/target/|] </> fossaFile) verboseGraphJson
      writeJson (tmpdir </> [reldir|mod-c/target/|] </> fossaFile) verboseGraphJson
      writeJson (decoyFile tmpdir) decoyGraphJson
      graphs <- parseVerboseGraphs (closurePoms tmpdir) tmpdir
      shouldSatisfy' graphs ((== 2) . length)
    itWithTempDir' "falls back to a tree walk when an expected file is missing" $ \tmpdir -> do
      writeJson (tmpdir </> [reldir|mod-a/target/|] </> fossaFile) verboseGraphJson
      -- mod-c's expected file deliberately absent
      writeJson (decoyFile tmpdir) decoyGraphJson
      graphs <- parseVerboseGraphs (closurePoms tmpdir) tmpdir
      shouldSatisfy' graphs ((== 2) . length)
      -- the walk found the stray file, so it really ran
      shouldContain' graphs [decoyGraph]
    itWithTempDir' "falls back to a tree walk when derivation is impossible (interpolated build dir)" $ \tmpdir -> do
      writeJson (tmpdir </> [reldir|mod-a/target/|] </> fossaFile) verboseGraphJson
      writeJson (tmpdir </> [reldir|mod-c/target/|] </> fossaFile) verboseGraphJson
      writeJson (decoyFile tmpdir) decoyGraphJson
      let badClosure =
            Map.singleton
              coordA
              ( pomEntry
                  coordA
                  (tmpdir </> [reldir|mod-a/|] </> [relfile|pom.xml|])
                  (Map.singleton (("g", "a") :: (Text, Text)) (Just (PomBuild{pomBuildFinalName = Nothing, pomBuildOutputDirectory = Just "${build.dir}"})))
              )
      graphs <- parseVerboseGraphs badClosure tmpdir
      shouldSatisfy' graphs ((== 3) . length)
    itWithTempDir' "raises a fatal when no verbose graph files are found for a non-empty closure" $ \tmpdir -> do
      -- nothing written anywhere under tmpdir: both derived files absent,
      -- the walk finds nothing, and the closure is non-empty
      expectFatal' $ parseVerboseGraphs (closurePoms tmpdir) tmpdir

-- | The per-module verbose graph locations are derived from the closure poms:
-- a declared literal '<build><directory>' wins, otherwise Maven's default
-- 'target/' under the module; any unresolvable ('${...}') directory poisons
-- the whole derivation so callers can fall back to a tree walk.
deriveVerboseGraphPathsSpec :: Spec
deriveVerboseGraphPathsSpec = do
  let fossaJson = [relfile|fossa-depgraph-verbose.json|]
      -- Paths are built from a temp dir at runtime: compile-time absolute
      -- literals ([absfile|/proj/...|]) are not valid on every platform.
      pomAtPath path coord' builds =
        ( path
        , Pom
            { pomCoord = coord'
            , pomParentCoord = Nothing
            , pomProperties = Map.empty
            , pomDependencyManagement = Map.empty
            , pomDependencies = Map.empty
            , pomLicenses = []
            , pomBuilds = builds
            }
        )
      coord g a = MavenCoordinate g a "1.0.0"
      build :: Maybe Text -> PomBuild
      build dir = PomBuild{pomBuildFinalName = Nothing, pomBuildOutputDirectory = dir}
  describe "deriveVerboseGraphPaths" $ do
    itWithTempDir' "defaults to target/ under the module dir" $ \tmpdir -> do
      let modAPath = tmpdir </> [reldir|mod-a/|] </> [relfile|pom.xml|]
      deriveVerboseGraphPaths (Map.singleton (coord "g" "a") (pomAtPath modAPath (coord "g" "a") Map.empty))
        `shouldBe'` Just [parent modAPath </> [reldir|target/|] </> fossaJson]
    itWithTempDir' "honors a literal relative build directory" $ \tmpdir -> do
      let modAPath = tmpdir </> [reldir|mod-a/|] </> [relfile|pom.xml|]
      deriveVerboseGraphPaths
        ( Map.singleton
            (coord "g" "a")
            (pomAtPath modAPath (coord "g" "a") (Map.singleton (("g", "a") :: (Text, Text)) (Just (build (Just "out")))))
        )
        `shouldBe'` Just [parent modAPath </> [reldir|out/|] </> fossaJson]
    itWithTempDir' "honors a nested build directory" $ \tmpdir -> do
      let modAPath = tmpdir </> [reldir|mod-a/|] </> [relfile|pom.xml|]
      deriveVerboseGraphPaths
        ( Map.singleton
            (coord "g" "a")
            (pomAtPath modAPath (coord "g" "a") (Map.singleton (("g", "a") :: (Text, Text)) (Just (build (Just "build/nested")))))
        )
        `shouldBe'` Just [parent modAPath </> [reldir|build/nested/|] </> fossaJson]
    itWithTempDir' "returns Nothing when a build directory is property-interpolated" $ \tmpdir -> do
      let modAPath = tmpdir </> [reldir|mod-a/|] </> [relfile|pom.xml|]
      deriveVerboseGraphPaths
        ( Map.singleton
            (coord "g" "a")
            (pomAtPath modAPath (coord "g" "a") (Map.singleton (("g", "a") :: (Text, Text)) (Just (build (Just "${build.dir}")))))
        )
        `shouldBe'` Nothing
    itWithTempDir' "returns Nothing if any module's directory is unresolvable" $ \tmpdir -> do
      let modAPath = tmpdir </> [reldir|mod-a/|] </> [relfile|pom.xml|]
          modBPath = tmpdir </> [reldir|mod-b/|] </> [relfile|pom.xml|]
      deriveVerboseGraphPaths
        ( Map.fromList
            [ ((coord "g" "a") :: MavenCoordinate, pomAtPath modAPath (coord "g" "a") (Map.singleton (("g", "a") :: (Text, Text)) (Just (build (Just "out")))))
            , (coord "g" "b", pomAtPath modBPath (coord "g" "b") (Map.singleton (("g", "b") :: (Text, Text)) (Just (build (Just "${build.dir}")))))
            ]
        )
        `shouldBe'` Nothing
    it "handles an empty closure" $
      deriveVerboseGraphPaths (Map.empty :: Map.Map MavenCoordinate (Path Abs File, Pom)) `shouldBe` Just []

-- | 'parsePluginOutput' must keep reading exactly what the plugin's @aggregate@
-- goal writes ('dependency-graph.txt' in text format, inside @-DoutputDirectory@);
-- these tests pin
-- that writer/reader contract so a format change on either side fails loudly.
parsePluginOutputSpec :: Spec
parsePluginOutputSpec = do
  itWithTempDir' "parses the aggregate goal's text output (realistic format)" $ \tmpdir -> do
    sendIO $ BS.writeFile (toFilePath (tmpdir </> [relfile|dependency-graph.txt|])) aggregateTextFixture
    out <- parsePluginOutput tmpdir
    out `shouldBe'` expectedAggregateOutput
  itWithTempDir' "parses multi-module output with one root tree per module" $ \tmpdir -> do
    sendIO $ BS.writeFile (toFilePath (tmpdir </> [relfile|dependency-graph.txt|])) multiModuleTextFixture
    out <- parsePluginOutput tmpdir
    out `shouldBe'` expectedMultiModuleOutput
  itWithTempDir' "raises a fatal parse error on malformed output" $ \tmpdir -> do
    sendIO $ BS.writeFile (toFilePath (tmpdir </> [relfile|dependency-graph.txt|])) (BS.pack "@@not-a-dependency-graph@@")
    expectFatal' (parsePluginOutput tmpdir)

-- | Shape taken from the real plugin output documented in 'Strategy.Maven.PluginTree';
-- exercises multi-scope artifacts ('test/compile') and the '(optional)' marker.
aggregateTextFixture :: BS.ByteString
aggregateTextFixture =
  BS.pack
    [r|org.clojure:clojure:1.12.0:compile
+- org.fake:fake-pkg:1.0.0:compile (optional)
\- org.foo:bar:1.0.0:test/compile
   +- org.baz:buzz:1.0.0:test
   \- org.clojure:data.generators:1.0.0:test|]

-- | Captured verbatim from the real plugin:
-- 'mvn com.github.ferstl:depgraph-maven-plugin:4.0.1:aggregate -DgraphFormat=text ...' on a
-- 3-module reactor (identical output on 3.3.0). mod-b is folded into mod-a's tree; mod-c is a
-- second top-level root; junit and hamcrest-core repeat under both roots.
multiModuleTextFixture :: BS.ByteString
multiModuleTextFixture =
  BS.pack
    [r|org.example:mod-a:1.0.0:compile
+- org.example:mod-b:1.0.0:compile
\- junit:junit:4.13.2:compile
   \- org.hamcrest:hamcrest-core:1.3:compile
org.example:mod-c:1.0.0:compile
\- junit:junit:4.13.2:compile
   \- org.hamcrest:hamcrest-core:1.3:compile|]

expectedMultiModuleOutput :: PluginOutput
expectedMultiModuleOutput =
  PluginOutput
    { outArtifacts =
        [ Artifact
            { artifactNumericId = 4
            , artifactGroupId = "org.example"
            , artifactArtifactId = "mod-a"
            , artifactVersion = "1.0.0"
            , artifactScopes = ["compile"]
            , artifactOptional = False
            , artifactIsDirect = True
            }
        , Artifact
            { artifactNumericId = 3
            , artifactGroupId = "org.example"
            , artifactArtifactId = "mod-b"
            , artifactVersion = "1.0.0"
            , artifactScopes = ["compile"]
            , artifactOptional = False
            , artifactIsDirect = False
            }
        , Artifact
            { artifactNumericId = 1
            , artifactGroupId = "junit"
            , artifactArtifactId = "junit"
            , artifactVersion = "4.13.2"
            , artifactScopes = ["compile"]
            , artifactOptional = False
            , artifactIsDirect = False
            }
        , Artifact
            { artifactNumericId = 0
            , artifactGroupId = "org.hamcrest"
            , artifactArtifactId = "hamcrest-core"
            , artifactVersion = "1.3"
            , artifactScopes = ["compile"]
            , artifactOptional = False
            , artifactIsDirect = False
            }
        , Artifact
            { artifactNumericId = 2
            , artifactGroupId = "org.example"
            , artifactArtifactId = "mod-c"
            , artifactVersion = "1.0.0"
            , artifactScopes = ["compile"]
            , artifactOptional = False
            , artifactIsDirect = True
            }
        , -- junit and hamcrest-core are visited once per root tree; both visits
          -- resolve to the same numeric ids (per-visit emission is existing behavior)
          Artifact
            { artifactNumericId = 1
            , artifactGroupId = "junit"
            , artifactArtifactId = "junit"
            , artifactVersion = "4.13.2"
            , artifactScopes = ["compile"]
            , artifactOptional = False
            , artifactIsDirect = False
            }
        , Artifact
            { artifactNumericId = 0
            , artifactGroupId = "org.hamcrest"
            , artifactArtifactId = "hamcrest-core"
            , artifactVersion = "1.3"
            , artifactScopes = ["compile"]
            , artifactOptional = False
            , artifactIsDirect = False
            }
        ]
    , outEdges =
        [ Edge 4 3
        , Edge 4 1
        , Edge 1 0
        , Edge 2 1
        , Edge 1 0
        ]
    }

expectedAggregateOutput :: PluginOutput
expectedAggregateOutput =
  PluginOutput
    { outArtifacts =
        [ Artifact
            { artifactNumericId = 4
            , artifactGroupId = "org.clojure"
            , artifactArtifactId = "clojure"
            , artifactVersion = "1.12.0"
            , artifactScopes = ["compile"]
            , artifactOptional = False
            , artifactIsDirect = True
            }
        , Artifact
            { artifactNumericId = 3
            , artifactGroupId = "org.fake"
            , artifactArtifactId = "fake-pkg"
            , artifactVersion = "1.0.0"
            , artifactScopes = ["compile"]
            , artifactOptional = True
            , artifactIsDirect = False
            }
        , Artifact
            { artifactNumericId = 2
            , artifactGroupId = "org.foo"
            , artifactArtifactId = "bar"
            , artifactVersion = "1.0.0"
            , artifactScopes = ["test", "compile"]
            , artifactOptional = False
            , artifactIsDirect = False
            }
        , Artifact
            { artifactNumericId = 1
            , artifactGroupId = "org.baz"
            , artifactArtifactId = "buzz"
            , artifactVersion = "1.0.0"
            , artifactScopes = ["test"]
            , artifactOptional = False
            , artifactIsDirect = False
            }
        , Artifact
            { artifactNumericId = 0
            , artifactGroupId = "org.clojure"
            , artifactArtifactId = "data.generators"
            , artifactVersion = "1.0.0"
            , artifactScopes = ["test"]
            , artifactOptional = False
            , artifactIsDirect = False
            }
        ]
    , outEdges =
        [ Edge 4 3
        , Edge 4 2
        , Edge 2 1
        , Edge 2 0
        ]
    }

singleTextArtifact :: TextArtifact
singleTextArtifact =
  TextArtifact
    { artifactText = "org.clojure:clojure:1.12.0-master-SNAPSHOT"
    , groupId = "org.clojure"
    , artifactId = "clojure"
    , textArtifactVersion = "1.12.0-master-SNAPSHOT"
    , scopes = ["test"]
    , isDirect = True
    , isOptional = False
    }

complexTextArtifact :: Tree TextArtifact
complexTextArtifact =
  Node
    TextArtifact
      { artifactText = "org.clojure:test.generative:1.0.0"
      , groupId = "org.clojure"
      , artifactId = "test.generative"
      , textArtifactVersion = "1.0.0"
      , scopes = ["test"]
      , isDirect = True
      , isOptional = False
      }
    [ Node
        TextArtifact
          { artifactText = "org.fake:fake-pkg:1.0.0"
          , groupId = "org.fake"
          , artifactId = "fake-pkg"
          , textArtifactVersion = "1.0.0"
          , scopes = ["compile"]
          , isDirect = False
          , isOptional = True
          }
        []
    , Node
        TextArtifact
          { artifactText = "org.foo:bar:1.0.0"
          , groupId = "org.foo"
          , artifactId = "bar"
          , textArtifactVersion = "1.0.0"
          , isDirect = False
          , scopes = ["compile"]
          , isOptional = False
          }
        [ Node
            TextArtifact
              { artifactText = "org.baz:buzz:1.0.0"
              , groupId = "org.baz"
              , artifactId = "buzz"
              , textArtifactVersion = "1.0.0"
              , isDirect = False
              , scopes = ["test"]
              , isOptional = False
              }
            []
        ]
    , Node
        TextArtifact
          { artifactText = "org.clojure:data.generators:1.0.0"
          , groupId = "org.clojure"
          , artifactId = "data.generators"
          , textArtifactVersion = "1.0.0"
          , isDirect = False
          , scopes = ["test"]
          , isOptional = False
          }
        []
    ]

complexPluginOutputArtifacts :: PluginOutput
complexPluginOutputArtifacts =
  PluginOutput
    { outArtifacts =
        [ Artifact
            { artifactNumericId = 0
            , artifactGroupId = "org.clojure"
            , artifactArtifactId = "data.generators"
            , artifactVersion = "1.0.0"
            , artifactScopes = ["test"]
            , artifactOptional = False
            , artifactIsDirect = False
            }
        , Artifact
            { artifactNumericId = 1
            , artifactGroupId = "org.baz"
            , artifactArtifactId = "buzz"
            , artifactVersion = "1.0.0"
            , artifactScopes = ["test"]
            , artifactOptional = False
            , artifactIsDirect = False
            }
        , Artifact
            { artifactNumericId = 2
            , artifactGroupId = "org.foo"
            , artifactArtifactId = "bar"
            , artifactVersion = "1.0.0"
            , artifactScopes = ["compile"]
            , artifactOptional = False
            , artifactIsDirect = False
            }
        , Artifact
            { artifactNumericId = 3
            , artifactGroupId = "org.fake"
            , artifactArtifactId = "fake-pkg"
            , artifactVersion = "1.0.0"
            , artifactScopes = ["compile"]
            , artifactOptional = True
            , artifactIsDirect = False
            }
        , Artifact
            { artifactNumericId = 4
            , artifactGroupId = "org.clojure"
            , artifactArtifactId = "test.generative"
            , artifactVersion = "1.0.0"
            , artifactScopes = ["test"]
            , artifactOptional = False
            , artifactIsDirect = True
            }
        ]
    , outEdges =
        [ Edge 2 1
        , Edge 4 3
        , Edge 4 2
        , Edge 4 0
        ]
    }

textArtifactConversionSpec :: Spec
textArtifactConversionSpec =
  describe "Maven text artifact -> PluginOutput conversion" $ do
    it' "Converts a single TextArtifact correctly" $ do
      pluginOutput <- textArtifactToPluginOutput [Node singleTextArtifact []]
      pluginOutput
        `shouldBe'` PluginOutput
          { outArtifacts = [simpleArtifact]
          , outEdges = []
          }

    it' "Converts a more complex TextArtifact correctly" $ do
      PluginOutput{outArtifacts = resArts, outEdges = resEdges} <- textArtifactToPluginOutput [complexTextArtifact]
      resArts `shouldMatchList'` (outArtifacts complexPluginOutputArtifacts)
      resEdges `shouldMatchList'` (outEdges complexPluginOutputArtifacts)

    it' "should correctly include dependency with multiple scopes" $ do
      let maybeArtifactTree = mkTreeTextArtifact depWithMultipleScopes
      case maybeArtifactTree of
        Nothing -> expectationFailure' "could not parse raw tree output!"
        Just tree' -> do
          PluginOutput{outArtifacts = resArts} <- textArtifactToPluginOutput [tree']
          resArts `shouldContain'` [kafkaClientCompile]
          resArts `shouldContain'` [kafkaClientTest]

simpleArtifact :: Artifact
simpleArtifact =
  Artifact
    { artifactNumericId = 0
    , artifactGroupId = "org.clojure"
    , artifactArtifactId = "clojure"
    , artifactVersion = "1.12.0-master-SNAPSHOT"
    , artifactOptional = False
    , artifactScopes = ["test"]
    , artifactIsDirect = True
    }

mkTreeTextArtifact :: Text -> Maybe (Tree TextArtifact)
mkTreeTextArtifact = parseMaybe parseTextArtifact

depWithMultipleScopes :: Text
depWithMultipleScopes =
  [r|com.mycompany.app:my-app:1.0-SNAPSHOT:compile
+- junit:junit:4.11:test
|  \- org.hamcrest:hamcrest-core:1.3:test
+- org.apache.kafka:kafka-clients:3.0.2:compile
|  +- com.github.luben:zstd-jni:1.5.0-2:runtime
|  +- org.lz4:lz4-java:1.7.1:runtime
|  +- org.xerial.snappy:snappy-java:1.1.8.1:runtime
|  \- org.slf4j:slf4j-api:1.7.30:runtime
+- org.apache.kafka:kafka-clients:3.0.2:test
\- joda-time:joda-time:2.9.2:compile|]

kafkaClientCompile :: Artifact
kafkaClientCompile = Artifact 6 "org.apache.kafka" "kafka-clients" "3.0.2" ["compile"] False False

kafkaClientTest :: Artifact
kafkaClientTest = kafkaClientCompile{artifactNumericId = 1, artifactScopes = ["test"]}

-- | depgraph JSON with @showDuplicates=true@. The numeric ids deliberately do
-- not line up: depgraph numbers artifacts and edge endpoints independently, so
-- edges must join to artifacts via the string ids.
verboseGraphJson :: BS.ByteString
verboseGraphJson =
  BS.pack
    [r|{
  "graphName": "example",
  "artifacts": [
    { "id": "org.example:app:jar", "numericId": 7, "groupId": "org.example", "artifactId": "app", "version": "1.0.0", "scopes": [], "types": ["jar"] },
    { "id": "org.apache.poi:poi-ooxml:jar", "numericId": 3, "groupId": "org.apache.poi", "artifactId": "poi-ooxml", "version": "5.2.5", "scopes": ["compile"], "types": ["jar"] },
    { "id": "org.apache.logging.log4j:log4j-api:jar", "numericId": 9, "groupId": "org.apache.logging.log4j", "artifactId": "log4j-api", "version": "2.21.1", "scopes": ["compile"], "types": ["jar"] },
    { "id": "org.apache.logging.log4j:log4j-core:jar", "numericId": 1, "groupId": "org.apache.logging.log4j", "artifactId": "log4j-core", "version": "2.21.1", "scopes": ["compile"], "types": ["jar"] }
  ],
  "dependencies": [
    { "from": "org.example:app:jar", "to": "org.apache.poi:poi-ooxml:jar", "numericFrom": 1, "numericTo": 2, "resolution": "INCLUDED" },
    { "from": "org.example:app:jar", "to": "org.apache.logging.log4j:log4j-core:jar", "numericFrom": 1, "numericTo": 4, "resolution": "INCLUDED" },
    { "from": "org.apache.poi:poi-ooxml:jar", "to": "org.apache.logging.log4j:log4j-api:jar", "numericFrom": 2, "numericTo": 3, "resolution": "INCLUDED" },
    { "from": "org.apache.logging.log4j:log4j-core:jar", "to": "org.apache.logging.log4j:log4j-api:jar", "numericFrom": 4, "numericTo": 3, "resolution": "OMITTED_FOR_DUPLICATE" }
  ]
}|]

expectedVerboseGraph :: VerboseGraph
expectedVerboseGraph =
  VerboseGraph
    { verboseArtifacts =
        [ VerboseArtifact "org.example:app:jar" "org.example" "app" "1.0.0"
        , VerboseArtifact "org.apache.poi:poi-ooxml:jar" "org.apache.poi" "poi-ooxml" "5.2.5"
        , VerboseArtifact "org.apache.logging.log4j:log4j-api:jar" "org.apache.logging.log4j" "log4j-api" "2.21.1"
        , VerboseArtifact "org.apache.logging.log4j:log4j-core:jar" "org.apache.logging.log4j" "log4j-core" "2.21.1"
        ]
    , verboseEdges =
        [ VerboseEdge "org.example:app:jar" "org.apache.poi:poi-ooxml:jar" "INCLUDED"
        , VerboseEdge "org.example:app:jar" "org.apache.logging.log4j:log4j-core:jar" "INCLUDED"
        , VerboseEdge "org.apache.poi:poi-ooxml:jar" "org.apache.logging.log4j:log4j-api:jar" "INCLUDED"
        , VerboseEdge "org.apache.logging.log4j:log4j-core:jar" "org.apache.logging.log4j:log4j-api:jar" "OMITTED_FOR_DUPLICATE"
        ]
    }

-- | A graph that no closure module is expected to produce; used to detect when
-- the tree-walk fallback ran (it is the only thing that could find a file with
-- this content in an unexpected location).
decoyGraphJson :: BS.ByteString
decoyGraphJson =
  BS.pack
    [r|{
  "graphName": "decoy",
  "artifacts": [
    { "id": "org.decoy:stray:jar", "numericId": 1, "groupId": "org.decoy", "artifactId": "stray", "version": "0.0.1", "scopes": [], "types": ["jar"] }
  ],
  "dependencies": []
}|]

decoyGraph :: VerboseGraph
decoyGraph =
  VerboseGraph
    { verboseArtifacts = [VerboseArtifact "org.decoy:stray:jar" "org.decoy" "stray" "0.0.1"]
    , verboseEdges = []
    }

verboseGraphParsingSpec :: Spec
verboseGraphParsingSpec =
  describe "verbose graph parsing" $
    it "should parse the depgraph plugin's json format" $
      eitherDecode verboseGraphJson `shouldBe` Right expectedVerboseGraph

-- Aggregate output for the same build, with its own unrelated numeric ids.
mkAggregateArtifact :: Int -> Text -> Text -> Text -> Bool -> Artifact
mkAggregateArtifact numericId groupId artifactId version isDirect =
  Artifact
    { artifactNumericId = numericId
    , artifactGroupId = groupId
    , artifactArtifactId = artifactId
    , artifactVersion = version
    , artifactScopes = ["compile"]
    , artifactOptional = False
    , artifactIsDirect = isDirect
    }

aggregateOutput :: PluginOutput
aggregateOutput =
  PluginOutput
    { outArtifacts =
        [ mkAggregateArtifact 10 "org.apache.poi" "poi-ooxml" "5.2.5" True
        , mkAggregateArtifact 11 "org.apache.logging.log4j" "log4j-api" "2.21.1" False
        , mkAggregateArtifact 12 "org.apache.logging.log4j" "log4j-core" "2.21.1" True
        ]
    , outEdges = [Edge 10 11]
    }

augmentWithDuplicateEdgesSpec :: Spec
augmentWithDuplicateEdgesSpec =
  describe "augmentWithDuplicateEdges" $ do
    it "should add duplicate-resolved edges between existing artifacts" $
      outEdges (augmentWithDuplicateEdges aggregateOutput [expectedVerboseGraph])
        `shouldBe` [Edge 10 11, Edge 12 11]

    it "should not modify artifacts" $
      outArtifacts (augmentWithDuplicateEdges aggregateOutput [expectedVerboseGraph])
        `shouldBe` outArtifacts aggregateOutput

    it "should ignore included edges and artifacts absent from the aggregate output" $ do
      let onlyIncluded =
            expectedVerboseGraph
              { verboseEdges =
                  [ VerboseEdge "org.example:app:jar" "org.apache.poi:poi-ooxml:jar" "INCLUDED"
                  , VerboseEdge "org.apache.poi:poi-ooxml:jar" "org.apache.logging.log4j:log4j-api:jar" "INCLUDED"
                  ]
              }
      outEdges (augmentWithDuplicateEdges aggregateOutput [onlyIncluded])
        `shouldBe` outEdges aggregateOutput

    it "should not duplicate an edge that already exists" $ do
      let alreadyPresent = aggregateOutput{outEdges = [Edge 10 11, Edge 12 11]}
      outEdges (augmentWithDuplicateEdges alreadyPresent [expectedVerboseGraph])
        `shouldBe` [Edge 10 11, Edge 12 11]

    it "should do nothing without verbose graphs" $
      augmentWithDuplicateEdges aggregateOutput [] `shouldBe` aggregateOutput
