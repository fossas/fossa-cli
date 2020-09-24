{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Strategy.Gradle
  ( discover

  , buildGraph
  , JsonDep(..)
  , PackageName (..)
  , ConfigName (..)
  ) where

import Control.Carrier.Error.Either
import Control.Effect.Diagnostics
import Control.Effect.Exception
import Control.Effect.Lift (sendIO)
import Control.Effect.Path (withSystemTempDir)
import Data.Aeson
import Data.Aeson.Types (Parser, unexpected)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.FileEmbed (embedFile)
import Data.Foldable (find, for_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import DepTypes
import Discovery.Walk
import Effect.Exec
import Effect.Grapher
import Graphing (Graphing)
import Path
import qualified System.FilePath as FP
import Types

newtype ConfigName = ConfigName { unConfigName :: Text } deriving (Eq, Ord, Show, FromJSON)
newtype GradleLabel = Env DepEnvironment deriving (Eq, Ord, Show)
newtype PackageName = PackageName { unPackageName :: Text } deriving (Eq, Ord, Show, FromJSON)

gradleJsonDepsCmd :: Text -> FP.FilePath -> Command
gradleJsonDepsCmd baseCmd initScriptFilepath = Command
  { cmdName = baseCmd
  , cmdArgs = ["jsonDeps", "-I", T.pack initScriptFilepath]
  , cmdAllowErr = Never
  }

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \_ _ files -> do
  case find (\f -> fileName f == "build.gradle") files of
    Nothing -> pure WalkContinue
    Just file -> do
      runSimpleStrategy "gradle-cli" GradleGroup $ analyze (parent file)
      pure WalkSkipAll

initScript :: ByteString
initScript = $(embedFile "scripts/jsondeps.gradle")

analyze ::
  ( Has (Lift IO) sig m
  , Has Exec sig m
  , Has Diagnostics sig m
  )
  => Path Abs Dir -> m ProjectClosureBody
analyze dir = withSystemTempDir "fossa-gradle" $ \tmpDir -> do
  let initScriptFilepath = fromAbsDir tmpDir FP.</> "jsondeps.gradle"
  sendIO (BS.writeFile initScriptFilepath initScript)
  stdout <- execThrow dir (gradleJsonDepsCmd "./gradlew" initScriptFilepath)
              <||> execThrow dir (gradleJsonDepsCmd "gradlew.bat" initScriptFilepath)
              <||> execThrow dir (gradleJsonDepsCmd "gradle" initScriptFilepath)

  let text = decodeUtf8 $ BL.toStrict stdout
      textLines :: [Text]
      textLines = T.lines (T.filter (/= '\r') text)
      -- jsonDeps lines look like:
      -- JSONDEPS_:project-path_{"configName":[{"type":"package", ...}, ...], ...}
      jsonDepsLines :: [Text]
      jsonDepsLines = mapMaybe (T.stripPrefix "JSONDEPS_") textLines

      packagePathsWithJson :: [(PackageName, Text)]
      packagePathsWithJson = map (\line -> let (x,y) = T.breakOn "_" line in (PackageName x, T.drop 1 y {- drop the underscore; break doesn't remove it -})) jsonDepsLines

      packagePathsWithDecoded :: [((PackageName, ConfigName), [JsonDep])]
      packagePathsWithDecoded = do
        (name, outJson) <- packagePathsWithJson
        let configMap = fromMaybe mempty . decodeStrict $ encodeUtf8 outJson
        (configName, deps) <- M.toList configMap
        pure ((name, ConfigName configName), deps)

      packagesToOutput :: Map (PackageName, ConfigName) [JsonDep]
      packagesToOutput = M.fromList packagePathsWithDecoded

  pure (mkProjectClosure dir packagesToOutput)

mkProjectClosure :: Path Abs Dir -> Map (PackageName, ConfigName) [JsonDep] -> ProjectClosureBody
mkProjectClosure dir deps = ProjectClosureBody
  { bodyModuleDir    = dir
  , bodyDependencies = dependencies
  , bodyLicenses     = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph deps
    , dependenciesOptimal  = Optimal
    , dependenciesComplete = Complete
    }

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

  projectToDep name = Dependency
    { dependencyType = SubprojectType
    , dependencyName = name
    , dependencyVersion = Nothing
    , dependencyLocations = []
    , dependencyEnvironments = []
    , dependencyTags = M.empty
    }

data JsonDep =
    ProjectDep Text -- name
  | PackageDep Text Text [JsonDep] -- name version deps
  deriving (Eq, Ord, Show)

instance FromJSON JsonDep where
  parseJSON = withObject "JsonDep" $ \obj -> do
    ty <- obj .: "type" :: Parser Text
    case ty of
      "project" -> ProjectDep <$> obj .: "name"
      "package" -> PackageDep <$> obj .: "name" <*> obj .: "version" <*> obj .: "dependencies"
      _         -> unexpected (String ty)
