{-# language TemplateHaskell #-}

module Strategy.Gradle
  ( discover

  , buildGraph
  , JsonDep(..)
  ) where

import Prologue hiding (json)

import Data.Aeson.Types (Parser, unexpected)
import Control.Carrier.Error.Either
import Control.Effect.Diagnostics
import Control.Effect.Exception
import Control.Effect.Path (withSystemTempDir)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.FileEmbed (embedFile)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified System.FilePath as FP

import DepTypes
import Discovery.Walk
import Effect.Exec
import Effect.Grapher
import Graphing (Graphing)
import Types

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
  , MonadIO m
  )
  => Path Abs Dir -> m ProjectClosureBody
analyze dir = withSystemTempDir "fossa-gradle" $ \tmpDir -> do
  let initScriptFilepath = fromAbsDir tmpDir FP.</> "jsondeps.gradle"
  liftIO (BS.writeFile initScriptFilepath initScript)
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

      packagePathsWithJson :: [(Text,Text)]
      packagePathsWithJson = map (\line -> let (x,y) = T.breakOn "_" line in (x, T.drop 1 y {- drop the underscore; break doesn't remove it -})) jsonDepsLines

      packagePathsWithDecoded :: [(Text, [JsonDep])]
      packagePathsWithDecoded = [(name, deps) | (name, json) <- packagePathsWithJson
                                              , Just configs <- [decodeStrict (encodeUtf8 json) :: Maybe (Map Text [JsonDep])]
                                              , Just deps <- [M.lookup "default" configs]] -- FUTURE: use more than default?

      packagesToOutput :: Map Text [JsonDep]
      packagesToOutput = M.fromList packagePathsWithDecoded

  pure (mkProjectClosure dir packagesToOutput)

mkProjectClosure :: Path Abs Dir -> Map Text [JsonDep] -> ProjectClosureBody
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
buildGraph :: Map Text [JsonDep] -> Graphing Dependency
buildGraph mapping = run . evalGrapher $ M.traverseWithKey addProject mapping
  where
  -- add top-level projects from the output
  addProject :: Has (Grapher Dependency) sig m => Text -> [JsonDep] -> m ()
  addProject projName projDeps = do
    let projAsDep = projectToDep projName
    direct projAsDep
    for_ projDeps $ \dep -> do
      edge projAsDep (jsonDepToDep dep)
      mkRecursiveEdges dep

  -- build edges between deps, recursively
  mkRecursiveEdges :: Has (Grapher Dependency) sig m => JsonDep -> m ()
  mkRecursiveEdges (ProjectDep _) = pure ()
  mkRecursiveEdges jsondep@(PackageDep _ _ deps) = do
    let packageAsDep = jsonDepToDep jsondep
    for_ deps $ \child -> do
      edge packageAsDep (jsonDepToDep child)
      mkRecursiveEdges child

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
  deriving (Eq, Ord, Show, Generic)

instance FromJSON JsonDep where
  parseJSON = withObject "JsonDep" $ \obj -> do
    ty <- obj .: "type" :: Parser Text
    case ty of
      "project" -> ProjectDep <$> obj .: "name"
      "package" -> PackageDep <$> obj .: "name" <*> obj .: "version" <*> obj .: "dependencies"
      _         -> unexpected (String ty)
