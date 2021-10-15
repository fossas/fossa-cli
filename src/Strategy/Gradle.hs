{-# LANGUAGE TemplateHaskell #-}

-- Strategy.Gradle provides dependency analysis for Gradle projects. It works by
-- looking for `build.gradle` files and shelling out to `gradle` using a bundled
-- Gradle init script (`jsondeps.gradle`). The init script does most of the
-- thinking, and the analyzer just invokes it and parses the JSON output.
--
-- If you're debugging this analyzer, you'll want to start by getting the output
-- of invoking the Gradle script. You should be able to find this output from
-- the replay logs of the analysis.
--
-- Useful links:
-- - Gradle subprojects: https://docs.gradle.org/current/userguide/multi_project_builds.html
-- - Gradle tasks: https://docs.gradle.org/current/userguide/tutorial_using_tasks.html
-- - Gradle init scripts: https://docs.gradle.org/current/userguide/init_scripts.html
module Strategy.Gradle (
  discover,
  buildGraph,
  JsonDep (..),
  PackageName (..),
  ConfigName (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject, analyzeProject)
import Control.Algebra (Has, run)
import Control.Effect.Diagnostics (
  Diagnostics,
  context,
  errorBoundary,
  fatal,
  renderFailureBundle,
  (<||>),
 )
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Path (withSystemTempDir)
import Data.Aeson (FromJSON (..), ToJSON, Value (..), decodeStrict, withObject, (.:))
import Data.Aeson.Types (Parser, unexpected)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.FileEmbed (embedFile)
import Data.Foldable (find, for_)
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Set.NonEmpty (nonEmpty, toSet)
import Data.String.Conversion (decodeUtf8, encodeUtf8, toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (
  DepEnvironment (..),
  DepType (MavenType, SubprojectType),
  Dependency (..),
  VerConstraint (CEq),
  insertEnvironment,
 )
import Discovery.Walk (WalkStep (..), fileName, walk')
import Effect.Exec (AllowErr (..), Command (..), Exec, execThrow)
import Effect.Grapher (LabeledGrapher, direct, edge, label, withLabeling)
import Effect.Logger (Logger, logWarn)
import Effect.ReadFS (ReadFS, doesFileExist)
import GHC.Generics (Generic)
import Graphing (Graphing)
import Path (Abs, Dir, File, Path, fromAbsDir, parent, parseRelFile, (</>))
import Strategy.Android.Util (isDefaultAndroidDevConfig, isDefaultAndroidTestConfig)
import System.FilePath qualified as FilePath
import Types (BuildTarget (..), DependencyResults (..), DiscoveredProject (..), FoundTargets (..), GraphBreadth (..))

newtype ConfigName = ConfigName {unConfigName :: Text} deriving (Eq, Ord, Show, FromJSON)
newtype GradleLabel = Env DepEnvironment deriving (Eq, Ord, Show)
newtype PackageName = PackageName {unPackageName :: Text} deriving (Eq, Ord, Show, FromJSON)

-- Run the init script on a set of subprojects. Note that this runs the
-- `:jsonDeps` task on every subproject in one command. This is helpful for
-- performance reasons, because Gradle has a slow startup on each invocation.
gradleJsonDepsCmdTargets :: FilePath -> Set BuildTarget -> Text -> Command
gradleJsonDepsCmdTargets initScriptFilepath targets baseCmd =
  Command
    { cmdName = baseCmd
    , cmdArgs = ["-I", toText initScriptFilepath] ++ map (\target -> unBuildTarget target <> ":jsonDeps") (Set.toList targets)
    , cmdAllowErr = Never
    }

-- Run the init script on a root project.
gradleJsonDepsCmd :: FilePath -> Text -> Command
gradleJsonDepsCmd initScriptFilepath baseCmd =
  Command
    { cmdName = baseCmd
    , cmdArgs = ["-I", toText initScriptFilepath, "jsonDeps"]
    , cmdAllowErr = Never
    }

discover ::
  ( Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Exec sig m
  , Has Logger sig m
  ) =>
  Path Abs Dir ->
  m [DiscoveredProject GradleProject]
discover dir = context "Gradle" $ do
  found <- context "Finding projects" $ findProjects dir
  pure $ mkProject <$> found

-- Run a Gradle command in a specific working directory, while correctly trying
-- Gradle wrappers.
runGradle :: (Has ReadFS sig m, Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> (Text -> Command) -> m BL.ByteString
runGradle dir cmd =
  do
    walkUpDir dir "gradlew" >>= execThrow dir . cmd . toText
    <||> (walkUpDir dir "gradlew.bat" >>= execThrow dir . cmd . toText)
    <||> execThrow dir (cmd "gradle")

-- Search upwards in a directory for the existence of the supplied file.
walkUpDir :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> Text -> m (Path Abs File)
walkUpDir dir filename = do
  relFile <- case parseRelFile $ toString filename of
    Nothing -> fatal $ "invalid file name: " <> filename
    Just path -> pure path
  let absFile = dir </> relFile
  exists <- doesFileExist absFile
  if exists
    then pure absFile
    else do
      let parentDir = parent dir
      if parentDir /= dir
        then walkUpDir parentDir filename
        else fatal $ "invalid file name: " <> filename

-- Search for a `build.gradle`. Each `build.gradle` is its own analysis target.
--
-- In the user documentation, we say that each Gradle subproject is its own
-- analysis target. This is morally true, but in our implementation we treat
-- each `build.gradle` as an analysis target.
--
-- This is to avoid invoking Gradle again for each subproject, which would be
-- slow (because of Gradle's startup time) and possibly wrong (because
-- subprojects need to resolve dependency constraints together).
findProjects :: (Has Exec sig m, Has Logger sig m, Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [GradleProject]
findProjects = walk' $ \dir _ files -> do
  case find (\f -> "build.gradle" `isPrefixOf` fileName f) files of
    Nothing -> pure ([], WalkContinue)
    Just buildFile -> do
      projectsStdout <-
        errorBoundary
          . context ("Listing gradle projects at '" <> toText dir <> "'")
          $ runGradle dir gradleProjectsCmd

      case projectsStdout of
        Left err -> do
          logWarn $ renderFailureBundle err
          -- Nearly all Gradle projects have a multi-module structure with a
          -- top-level root project, and subdirectories contain subprojects of
          -- the top-level root project.
          --
          -- If Gradle exits non-zero when invoked in the root project, it will
          -- almost certainly exit non-zero in subprojects, so there's no point
          -- in looking for subprojects in this subtree.
          pure ([], WalkSkipAll)
        Right result -> do
          let subprojects = parseProjects result

          let project =
                GradleProject
                  { gradleDir = dir
                  , gradleBuildFile = buildFile
                  , gradleProjects = subprojects
                  }

          pure ([project], WalkSkipAll)

data GradleProject = GradleProject
  { gradleDir :: Path Abs Dir
  , gradleBuildFile :: Path Abs File
  , gradleProjects :: Set Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON GradleProject

instance AnalyzeProject GradleProject where
  analyzeProject = getDeps

gradleProjectsCmd :: Text -> Command
gradleProjectsCmd baseCmd =
  Command
    { cmdName = baseCmd
    , cmdArgs = ["projects"]
    , cmdAllowErr = Never
    }

-- We use a single empty-string target when no subprojects exist. Gradle uses an
-- empty string to denote the root project when invoking tasks, e.g., ":task"
-- instead of ":subproject:task".
parseProjects :: BL.ByteString -> Set Text
parseProjects outBL = if Set.null subprojects then Set.singleton "" else subprojects
  where
    subprojects = Set.fromList $ mapMaybe parseSubproject outLines

    outText = decodeUtf8 $ BL.toStrict outBL
    outLines = Text.lines outText

-- | Parse a subproject line from the gradle output, e.g.,
--
-- >>> parseSubproject "+--- Project ':foo'"
-- Just ":foo"
--
-- >>> parseSubproject "    +--- Project ':foo'"
-- Just ":foo"
--
-- >>> parseSubproject "\\--- Project ':foo'"
-- Just ":foo"
--
-- >>> parseSubproject "|   +--- Project ':foo'"
-- Just ":foo"
--
-- >>> parseSubproject "anyprefix +--- Project ':foo'"
-- Just ":foo"
--
-- >>> parseSubproject "anything else"
-- Nothing
parseSubproject :: Text -> Maybe Text
parseSubproject line =
  case Text.breakOnEnd "--- Project '" (Text.strip line) of
    ("", _) -> Nothing -- no match
    (_, rest) -> Just $ Text.takeWhile (/= '\'') rest

mkProject :: GradleProject -> DiscoveredProject GradleProject
mkProject project =
  DiscoveredProject
    { projectType = "gradle"
    , projectBuildTargets = maybe ProjectWithoutTargets FoundTargets $ nonEmpty $ Set.map BuildTarget $ gradleProjects project
    , projectPath = gradleDir project
    , projectData = project
    }

getDeps :: (Has (Lift IO) sig m, Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => FoundTargets -> GradleProject -> m DependencyResults
getDeps targets project = context "Gradle" $ do
  graph <- analyze targets (gradleDir project)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [gradleBuildFile project]
      }

-- See the release process to see how this script gets vendored.
initScript :: ByteString
initScript = $(embedFile "scripts/jsondeps.gradle")

-- During analysis, we unpack the Gradle init script, run it, and parse the
-- output.
analyze ::
  ( Has (Lift IO) sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  FoundTargets ->
  Path Abs Dir ->
  m (Graphing Dependency)
analyze foundTargets dir = withSystemTempDir "fossa-gradle" $ \tmpDir -> do
  let initScriptFilepath = fromAbsDir tmpDir FilePath.</> "jsondeps.gradle"
  context "Writing gradle script" $ sendIO (BS.writeFile initScriptFilepath initScript)

  let cmd :: Text -> Command
      cmd = case foundTargets of
        FoundTargets targets -> gradleJsonDepsCmdTargets initScriptFilepath (toSet targets)
        ProjectWithoutTargets -> gradleJsonDepsCmd initScriptFilepath

  stdout <- context "running gradle script" $ runGradle dir cmd

  let text = decodeUtf8 $ BL.toStrict stdout

      textLines :: [Text]
      textLines = Text.lines (Text.filter (/= '\r') text)

      -- Output lines from the init script are of the format:
      -- JSONDEPS_:project-path_{"configName":[{"type":"package", ...}, ...], ...}
      --
      -- See the init script's implementation for details.
      jsonDepsLines :: [Text]
      jsonDepsLines = mapMaybe (Text.stripPrefix "JSONDEPS_") textLines

      packagePathsWithJson :: [(PackageName, Text)]
      packagePathsWithJson = map (\line -> let (x, y) = Text.breakOn "_" line in (PackageName x, Text.drop 1 y {- drop the underscore; break doesn't remove it -})) jsonDepsLines

      packagePathsWithDecoded :: [((PackageName, ConfigName), [JsonDep])]
      packagePathsWithDecoded = do
        (name, outJson) <- packagePathsWithJson
        let configMap = fromMaybe mempty . decodeStrict $ encodeUtf8 outJson
        (configName, deps) <- Map.toList configMap
        pure ((name, ConfigName configName), deps)

      packagesToOutput :: Map (PackageName, ConfigName) [JsonDep]
      packagesToOutput = Map.fromList packagePathsWithDecoded

  context "Building dependency graph" $ pure (buildGraph packagesToOutput)

-- TODO: use LabeledGraphing to add labels for environments
buildGraph :: Map (PackageName, ConfigName) [JsonDep] -> Graphing Dependency
buildGraph projectsAndDeps = run . withLabeling toDependency $ Map.traverseWithKey addProject projectsAndDeps
  where
    -- add top-level projects from the output
    addProject :: Has (LabeledGrapher JsonDep GradleLabel) sig m => (PackageName, ConfigName) -> [JsonDep] -> m ()
    addProject (projName, configName) projDeps = do
      let projAsDep = ProjectDep $ unPackageName projName
          envLabel = configNameToLabel configName
      direct projAsDep
      label projAsDep envLabel
      for_ projDeps $ \dep -> do
        edge projAsDep dep
        mkRecursiveEdges dep envLabel

    -- Infers environment label based on the name of configuration.
    -- Ref: https://docs.gradle.org/current/userguide/java_library_plugin.html#sec:java_library_configurations_graph
    configNameToLabel :: ConfigName -> GradleLabel
    configNameToLabel conf = case unConfigName conf of
      "compileOnly" -> Env EnvDevelopment
      x | x `elem` ["testImplementation", "testCompileOnly", "testRuntimeOnly", "testCompileClasspath", "testRuntimeClasspath"] -> Env EnvTesting
      x | isDefaultAndroidDevConfig x -> Env EnvDevelopment
      x | isDefaultAndroidTestConfig x -> Env EnvTesting
      x -> Env $ EnvOther x

    toDependency :: JsonDep -> Set GradleLabel -> Dependency
    toDependency dep = foldr applyLabel $ jsonDepToDep dep

    applyLabel :: GradleLabel -> Dependency -> Dependency
    applyLabel lbl dep = case lbl of
      Env env -> insertEnvironment env dep

    -- build edges between deps, recursively
    mkRecursiveEdges :: Has (LabeledGrapher JsonDep GradleLabel) sig m => JsonDep -> GradleLabel -> m ()
    mkRecursiveEdges (ProjectDep x) envLabel = label (ProjectDep x) envLabel
    mkRecursiveEdges jsondep@(PackageDep _ _ deps) envLabel = do
      label jsondep envLabel
      for_ deps $ \child -> do
        edge jsondep child
        mkRecursiveEdges child envLabel

    jsonDepToDep :: JsonDep -> Dependency
    jsonDepToDep (ProjectDep name) = projectToDep name
    jsonDepToDep (PackageDep name version _) =
      Dependency
        { dependencyType = MavenType
        , dependencyName = name
        , dependencyVersion = Just (CEq version)
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }

    projectToDep name =
      Dependency
        { dependencyType = SubprojectType
        , dependencyName = name
        , dependencyVersion = Nothing
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }

data JsonDep
  = ProjectDep Text -- name
  | PackageDep Text Text [JsonDep] -- name version deps
  deriving (Eq, Ord, Show)

instance FromJSON JsonDep where
  parseJSON = withObject "JsonDep" $ \obj -> do
    ty <- obj .: "type" :: Parser Text
    case ty of
      "project" -> ProjectDep <$> obj .: "name"
      "package" -> PackageDep <$> obj .: "name" <*> obj .: "version" <*> obj .: "dependencies"
      _ -> unexpected (String ty)
