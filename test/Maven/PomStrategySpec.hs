{-# LANGUAGE QuasiQuotes #-}

module Maven.PomStrategySpec (
  spec,
) where

import Control.Carrier.Reader (runReader)
import Control.Effect.Lift (sendIO)
import Data.ByteString.Char8 qualified as BS
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty (nonEmpty)
import Data.Text (Text)
import DepTypes (Dependency (dependencyName))
import Discovery.Filters (MavenScopeFilters (MavenScopeIncludeFilters))
import Graphing (Graphing)
import Graphing qualified
import Path (Abs, Dir, File, Path, Rel, reldir, relfile, toFilePath, (</>))
import Path.IO qualified as PIO
import Strategy.Maven (MavenProject (MavenProject), getDepsStatically, mkProject)
import Strategy.Maven.Pom (MavenPackage (..), buildMavenPackage, interpolateProperties)
import Strategy.Maven.Pom.Closure (findProjects)
import Strategy.Maven.Pom.PomFile
import Test.Effect (EffectStack, expectationFailure', itWithTempDir', shouldBe')
import Test.Hspec
import Types (BuildTarget (BuildTarget), DependencyResults (dependencyGraph), DiscoveredProject (projectBuildTargets), FoundTargets (FoundTargets, ProjectWithoutTargets))

spec :: Spec
spec = do
  describe "interpolateProperties" $ do
    let pom = Pom (MavenCoordinate "MYGROUP" "MYARTIFACT" "MYVERSION") Nothing Map.empty Map.empty Map.empty [] Map.empty
    it "should work for built-in properties" $ do
      interpolateProperties pom "${project.groupId}" `shouldBe` "MYGROUP"
      interpolateProperties pom "${project.artifactId}" `shouldBe` "MYARTIFACT"
      interpolateProperties pom "${project.version}" `shouldBe` "MYVERSION"

    it "should prefer user-specified properties over computed ones" $ do
      let pom' = pom{pomProperties = Map.singleton "project.groupId" "OTHERGROUP"}
      interpolateProperties pom' "${project.groupId}" `shouldBe` "OTHERGROUP"

    it "should work in the middle of strings" $ do
      interpolateProperties pom "foo${project.groupId}bar" `shouldBe` "fooMYGROUPbar"

    it "should interpolate multiple properties" $ do
      interpolateProperties pom "${project.groupId}${project.artifactId}" `shouldBe` "MYGROUPMYARTIFACT"

    it "should not infinitely recurse when interpolating a property that is interpolated to itself" $ do
      let pom' = pom{pomProperties = Map.singleton "project.groupId" "${project.groupId}"}
      interpolateProperties pom' "${project.groupId}" `shouldBe` "project.groupId"

    it "should not infinitely recurse when interpolating a property that is interpolated to itself" $ do
      let pom' = pom{pomProperties = Map.singleton "project.groupId" "\\${project.groupId}"}
      interpolateProperties pom' "${project.groupId}" `shouldBe` "\\project.groupId"

  describe "buildMavenPackage" $ do
    let pom = Pom (MavenCoordinate "MYGROUP" "MYARTIFACT" "MYVERSION") Nothing Map.empty Map.empty Map.empty [] Map.empty
    it "should interpolate properties in groupId/artifactId/version" $ do
      let result =
            buildMavenPackage
              pom
              "${project.groupId}"
              "${project.artifactId}"
              ( MvnDepBody
                  { depVersion = Just "${project.version}"
                  , depClassifier = Nothing
                  , depScope = Nothing
                  , depOptional = Nothing
                  }
              )
      result `shouldBe` MavenPackage "MYGROUP" "MYARTIFACT" (Just "MYVERSION")

  -- The pom tactic builds its graph rooted at the project itself. These tests pin
  -- that the project's own packages are removed from the reported graph and the
  -- dependencies they declare are reported as direct, matching what the dynamic
  -- tactics report for the same project.
  describe "static analysis of a single-module project" $ do
    itWithTempDir' "reports the declared dependencies as direct rather than the project itself" $ \dir -> do
      writePom dir [relfile|pom.xml|] singleModulePom
      onStaticGraph dir allTargets $ \graph ->
        directNames graph `shouldBe'` ["junit:junit", "org.apache.commons:commons-lang3"]

    itWithTempDir' "does not report the project itself as a dependency" $ \dir -> do
      writePom dir [relfile|pom.xml|] singleModulePom
      onStaticGraph dir allTargets $ \graph ->
        vertexNames graph `shouldBe'` ["junit:junit", "org.apache.commons:commons-lang3"]

  describe "static analysis of a multi-module project" $ do
    -- commons-lang3 is declared by the root pom, so both submodules inherit it.
    itWithTempDir' "reports the dependencies of every module as direct" $ \dir -> do
      createMultiModuleFixture dir
      onStaticGraph dir allTargets $ \graph ->
        directNames graph `shouldBe'` ["com.google.guava:guava", "junit:junit", "org.apache.commons:commons-lang3"]

    itWithTempDir' "does not report the root pom or any submodule as a dependency" $ \dir -> do
      createMultiModuleFixture dir
      onStaticGraph dir allTargets $ \graph ->
        vertexNames graph `shouldBe'` ["com.google.guava:guava", "junit:junit", "org.apache.commons:commons-lang3"]

    -- Submodule filtering deletes the root pom's node, which is the only direct
    -- node in the graph the pom tactic builds. Unless the surviving submodules are
    -- promoted to direct before being removed, this reports nothing as direct.
    itWithTempDir' "reports the selected submodule's dependencies as direct" $ \dir -> do
      createMultiModuleFixture dir
      onStaticGraph dir (onlyTargets ["com.example:mod-a"]) $ \graph ->
        directNames graph `shouldBe'` ["junit:junit", "org.apache.commons:commons-lang3"]

directNames :: Graphing Dependency -> [Text]
directNames = sort . map dependencyName . Graphing.directList

vertexNames :: Graphing Dependency -> [Text]
vertexNames = sort . map dependencyName . Graphing.vertexList

-- | Statically analyze the maven project in @dir@ and hand its dependency graph
-- to @act@. @select@ picks which build targets to analyze.
onStaticGraph ::
  Path Abs Dir ->
  (MavenProject -> FoundTargets) ->
  (Graphing Dependency -> EffectStack ()) ->
  EffectStack ()
onStaticGraph dir select act = do
  closures <- findProjects dir
  case closures of
    [closure] -> do
      let project = MavenProject closure
      results <- runReader noScopeFilters $ getDepsStatically (select project) project
      act $ dependencyGraph results
    -- Each fixture is one project closure; a different count means discovery went
    -- wrong, and saying so is more useful than an assertion about the graph.
    _ -> expectationFailure' $ "expected one project closure, found " <> show (length closures)
  where
    noScopeFilters :: MavenScopeFilters
    noScopeFilters = MavenScopeIncludeFilters mempty

-- | Every submodule, as @fossa analyze@ selects them when given no target filter.
allTargets :: MavenProject -> FoundTargets
allTargets = projectBuildTargets . mkProject

-- | Only the named submodules.
onlyTargets :: [Text] -> MavenProject -> FoundTargets
onlyTargets targets _ =
  maybe ProjectWithoutTargets FoundTargets . nonEmpty . Set.fromList $ map BuildTarget targets

writePom :: Path Abs Dir -> Path Rel File -> BS.ByteString -> EffectStack ()
writePom dir name = sendIO . BS.writeFile (toFilePath (dir </> name))

-- | Writes a two-module project into @dir@:
--
-- @
--   pom.xml       -- com.example:root:1.0, packaging=pom, declares commons-lang3
--   mod-a/pom.xml -- declares junit
--   mod-b/pom.xml -- declares guava
-- @
createMultiModuleFixture :: Path Abs Dir -> EffectStack ()
createMultiModuleFixture dir = do
  sendIO $ PIO.createDirIfMissing True (dir </> [reldir|mod-a|])
  sendIO $ PIO.createDirIfMissing True (dir </> [reldir|mod-b|])
  writePom dir [relfile|pom.xml|] multiModuleRootPom
  writePom (dir </> [reldir|mod-a|]) [relfile|pom.xml|] (modulePom "mod-a" "junit" "junit" "4.13.2")
  writePom (dir </> [reldir|mod-b|]) [relfile|pom.xml|] (modulePom "mod-b" "com.google.guava" "guava" "31.1-jre")

-- | Concatenates lines of an XML document.
packLines :: [String] -> BS.ByteString
packLines = BS.concat . map BS.pack

singleModulePom :: BS.ByteString
singleModulePom =
  packLines
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    , "<project xmlns=\"http://maven.apache.org/POM/4.0.0\">\n"
    , "  <modelVersion>4.0.0</modelVersion>\n"
    , "  <groupId>com.example</groupId>\n"
    , "  <artifactId>demo</artifactId>\n"
    , "  <version>1.0</version>\n"
    , "  <dependencies>\n"
    , "    <dependency>\n"
    , "      <groupId>junit</groupId>\n"
    , "      <artifactId>junit</artifactId>\n"
    , "      <version>4.13.2</version>\n"
    , "    </dependency>\n"
    , "    <dependency>\n"
    , "      <groupId>org.apache.commons</groupId>\n"
    , "      <artifactId>commons-lang3</artifactId>\n"
    , "      <version>3.12.0</version>\n"
    , "    </dependency>\n"
    , "  </dependencies>\n"
    , "</project>\n"
    ]

multiModuleRootPom :: BS.ByteString
multiModuleRootPom =
  packLines
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    , "<project xmlns=\"http://maven.apache.org/POM/4.0.0\">\n"
    , "  <modelVersion>4.0.0</modelVersion>\n"
    , "  <groupId>com.example</groupId>\n"
    , "  <artifactId>root</artifactId>\n"
    , "  <version>1.0</version>\n"
    , "  <packaging>pom</packaging>\n"
    , "  <modules>\n"
    , "    <module>mod-a</module>\n"
    , "    <module>mod-b</module>\n"
    , "  </modules>\n"
    , "  <dependencies>\n"
    , "    <dependency>\n"
    , "      <groupId>org.apache.commons</groupId>\n"
    , "      <artifactId>commons-lang3</artifactId>\n"
    , "      <version>3.12.0</version>\n"
    , "    </dependency>\n"
    , "  </dependencies>\n"
    , "</project>\n"
    ]

modulePom :: String -> String -> String -> String -> BS.ByteString
modulePom artifactId depGroup depArtifact depVersion' =
  packLines
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    , "<project xmlns=\"http://maven.apache.org/POM/4.0.0\">\n"
    , "  <modelVersion>4.0.0</modelVersion>\n"
    , "  <parent>\n"
    , "    <groupId>com.example</groupId>\n"
    , "    <artifactId>root</artifactId>\n"
    , "    <version>1.0</version>\n"
    , "    <relativePath>../pom.xml</relativePath>\n"
    , "  </parent>\n"
    , "  <artifactId>" <> artifactId <> "</artifactId>\n"
    , "  <dependencies>\n"
    , "    <dependency>\n"
    , "      <groupId>" <> depGroup <> "</groupId>\n"
    , "      <artifactId>" <> depArtifact <> "</artifactId>\n"
    , "      <version>" <> depVersion' <> "</version>\n"
    , "    </dependency>\n"
    , "  </dependencies>\n"
    , "</project>\n"
    ]
