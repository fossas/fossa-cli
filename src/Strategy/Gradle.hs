{-# LANGUAGE TemplateHaskell #-}

module Strategy.Gradle (
  discover,
  buildGraph,
  JsonDep (..),
  PackageName (..),
  ConfigName (..),
) where

import Control.Carrier.Diagnostics hiding (fromMaybe)
import Control.Effect.Exception
import Control.Effect.Lift (sendIO)
import Control.Effect.Path (withSystemTempDir)
import Data.Aeson
import Data.Aeson.Types (Parser, unexpected)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.FileEmbed (embedFile)
import Data.Foldable (find, for_)
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Set.NonEmpty
import Data.String.Conversion (decodeUtf8, encodeUtf8)
import Data.Text (Text)
import Data.Text qualified as T
import DepTypes
import Discovery.Walk
import Effect.Exec
import Effect.Grapher
import Effect.Logger (Logger, logWarn)
import Effect.ReadFS (ReadFS)
import Graphing (Graphing)
import Path
import System.FilePath qualified as FP
import Types

newtype ConfigName = ConfigName {unConfigName :: Text} deriving (Eq, Ord, Show, FromJSON)
newtype GradleLabel = Env DepEnvironment deriving (Eq, Ord, Show)
newtype PackageName = PackageName {unPackageName :: Text} deriving (Eq, Ord, Show, FromJSON)

gradleJsonDepsCmdTargets :: FP.FilePath -> Set BuildTarget -> Text -> Command
gradleJsonDepsCmdTargets initScriptFilepath targets baseCmd =
  Command
    { cmdName = baseCmd
    , cmdArgs = ["-I", T.pack initScriptFilepath] ++ map (\target -> unBuildTarget target <> ":jsonDeps") (S.toList targets)
    , cmdAllowErr = Never
    }

gradleJsonDepsCmd :: FP.FilePath -> Text -> Command
gradleJsonDepsCmd initScriptFilepath baseCmd =
  Command
    { cmdName = baseCmd
    , cmdArgs = ["-I", T.pack initScriptFilepath, "jsonDeps"]
    , cmdAllowErr = Never
    }

discover ::
  ( Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Exec sig m
  , Has Logger sig m
  , Has (Lift IO) rsig run
  , Has Exec rsig run
  , Has Diagnostics rsig run
  ) =>
  Path Abs Dir ->
  m [DiscoveredProject run]
discover dir = context "Gradle" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

pathToText :: Path ar fd -> Text
pathToText = T.pack . toFilePath

findProjects :: (Has Exec sig m, Has Logger sig m, Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [GradleProject]
findProjects = walk' $ \dir _ files -> do
  case find (\f -> "build.gradle" `isPrefixOf` fileName f) files of
    Nothing -> pure ([], WalkContinue)
    Just _ -> do
      projectsStdout <-
        errorBoundary
          . context ("Listing gradle projects at '" <> pathToText dir <> "'")
          $ execThrow dir (gradleProjectsCmd (pathToText dir <> "gradlew"))
            <||> execThrow dir (gradleProjectsCmd (pathToText dir <> "gradlew.bat"))
            <||> execThrow dir (gradleProjectsCmd "gradle")

      case projectsStdout of
        Left err -> do
          logWarn $ renderFailureBundle err
          -- Nearly all gradle projects have a multi-module structure with a
          -- top-level root project, and subdirectories contain subprojects of
          -- the top-level root project
          --
          -- If we're not able to scan the top-level root project, there's no
          -- reason to recurse and try the lower-level subprojects -- it only
          -- adds noise
          pure ([], WalkSkipAll)
        Right result -> do
          let subprojects = parseProjects result

          let project =
                GradleProject
                  { gradleDir = dir
                  , gradleProjects = subprojects
                  }

          pure ([project], WalkSkipAll)

data GradleProject = GradleProject
  { gradleDir :: Path Abs Dir
  , gradleProjects :: Set Text
  }
  deriving (Eq, Ord, Show)

gradleProjectsCmd :: Text -> Command
gradleProjectsCmd baseCmd =
  Command
    { cmdName = baseCmd
    , cmdArgs = ["projects"]
    , cmdAllowErr = Never
    }

-- we use a single empty-string target when no subprojects exist. gradle uses an
-- empty string to denote the root project when invoking tasks, e.g., ":task"
-- instead of ":subproject:task"
parseProjects :: BL.ByteString -> Set Text
parseProjects outBL = if S.null subprojects then S.singleton "" else subprojects
  where
    subprojects = S.fromList $ mapMaybe parseSubproject outLines

    outText = decodeUtf8 $ BL.toStrict outBL
    outLines = T.lines outText

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
-- Just ":foo'
--
-- >>> parseSubproject "anything else"
-- Nothing
parseSubproject :: Text -> Maybe Text
parseSubproject line =
  case T.breakOnEnd "--- Project '" (T.strip line) of
    ("", _) -> Nothing -- no match
    (_, rest) -> Just $ T.takeWhile (/= '\'') rest

mkProject :: (Has Exec sig n, Has (Lift IO) sig n, Has Diagnostics sig n) => GradleProject -> DiscoveredProject n
mkProject project =
  DiscoveredProject
    { projectType = "gradle"
    , projectBuildTargets = maybe ProjectWithoutTargets FoundTargets $ nonEmpty $ S.map BuildTarget $ gradleProjects project
    , projectDependencyGraph = getDeps project
    , projectPath = gradleDir project
    , projectLicenses = pure []
    }

getDeps :: (Has (Lift IO) sig m, Has Exec sig m, Has Diagnostics sig m) => GradleProject -> FoundTargets -> m (Graphing Dependency, GraphBreadth)
getDeps project targets = context "Gradle" $ analyze targets (gradleDir project)

initScript :: ByteString
initScript = $(embedFile "scripts/jsondeps.gradle")

analyze ::
  ( Has (Lift IO) sig m
  , Has Exec sig m
  , Has Diagnostics sig m
  ) =>
  FoundTargets ->
  Path Abs Dir ->
  m (Graphing Dependency, GraphBreadth)
analyze foundTargets dir = do
  graph <- withSystemTempDir "fossa-gradle" $ \tmpDir -> do
    let initScriptFilepath = fromAbsDir tmpDir FP.</> "jsondeps.gradle"
    context "Writing gradle script" $ sendIO (BS.writeFile initScriptFilepath initScript)

    let cmd :: Text -> Command
        cmd = case foundTargets of
          FoundTargets targets -> gradleJsonDepsCmdTargets initScriptFilepath (toSet targets)
          ProjectWithoutTargets -> gradleJsonDepsCmd initScriptFilepath

    stdout <-
      context "Running gradle script" $
        execThrow dir (cmd (pathToText dir <> "gradlew"))
          <||> execThrow dir (cmd (pathToText dir <> "gradlew.bat"))
          <||> execThrow dir (cmd "gradle")

    let text = decodeUtf8 $ BL.toStrict stdout
        textLines :: [Text]
        textLines = T.lines (T.filter (/= '\r') text)
        -- jsonDeps lines look like:
        -- JSONDEPS_:project-path_{"configName":[{"type":"package", ...}, ...], ...}
        jsonDepsLines :: [Text]
        jsonDepsLines = mapMaybe (T.stripPrefix "JSONDEPS_") textLines

        packagePathsWithJson :: [(PackageName, Text)]
        packagePathsWithJson = map (\line -> let (x, y) = T.breakOn "_" line in (PackageName x, T.drop 1 y {- drop the underscore; break doesn't remove it -})) jsonDepsLines

        packagePathsWithDecoded :: [((PackageName, ConfigName), [JsonDep])]
        packagePathsWithDecoded = do
          (name, outJson) <- packagePathsWithJson
          let configMap = fromMaybe mempty . decodeStrict $ encodeUtf8 outJson
          (configName, deps) <- M.toList configMap
          pure ((name, ConfigName configName), deps)

        packagesToOutput :: Map (PackageName, ConfigName) [JsonDep]
        packagesToOutput = M.fromList packagePathsWithDecoded

    context "Building dependency graph" $ pure (buildGraph packagesToOutput)
  pure (graph, Complete)

-- TODO: use LabeledGraphing to add labels for environments
buildGraph :: Map (PackageName, ConfigName) [JsonDep] -> Graphing Dependency
buildGraph projectsAndDeps = run . withLabeling toDependency $ M.traverseWithKey addProject projectsAndDeps
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

    configNameToLabel :: ConfigName -> GradleLabel
    configNameToLabel conf = case unConfigName conf of
      "compileOnly" -> Env EnvDevelopment
      x | x `elem` ["testImplementation", "testCompileOnly", "testRuntimeOnly"] -> Env EnvTesting
      x -> Env $ EnvOther x

    toDependency :: JsonDep -> S.Set GradleLabel -> Dependency
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
        , dependencyEnvironments = []
        , dependencyTags = M.empty
        }

    projectToDep name =
      Dependency
        { dependencyType = SubprojectType
        , dependencyName = name
        , dependencyVersion = Nothing
        , dependencyLocations = []
        , dependencyEnvironments = []
        , dependencyTags = M.empty
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
