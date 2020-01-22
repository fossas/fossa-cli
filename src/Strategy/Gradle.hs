{-# language TemplateHaskell #-}

module Strategy.Gradle
  ( discover
  , strategy

  , buildGraph
  , JsonDep(..)
  ) where

import Prologue hiding (json)

import Data.Aeson.Types (Parser, unexpected)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.FileEmbed (embedFile)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Path.IO (createTempDir, getTempDir, removeDirRecur)
import Polysemy
import Polysemy.Error
import Polysemy.Output
import Polysemy.Resource
import qualified System.FilePath as FP

import DepTypes
import Diagnostics
import Discovery.Walk
import Effect.Exec
import Effect.Grapher
import Graphing (Graphing)
import Types

gradleTasksCmd :: Command
gradleTasksCmd = Command
  { cmdNames = ["./gradlew", "gradlew.bat", "gradle"]
  , cmdBaseArgs = ["tasks"]
  , cmdAllowErr = Never
  }

gradleJsonDepsCmd :: Command
gradleJsonDepsCmd = Command
  { cmdNames = ["./gradlew", "gradlew.bat", "gradle"]
  , cmdBaseArgs = ["jsonDeps", "-I"]
  , cmdAllowErr = Never
  }

discover :: Discover
discover = Discover
  { discoverName = "gradle"
  , discoverFunc = discover'
  }

discover' :: forall r. Members '[Embed IO, Exec, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \dir subdirs files -> do
    let buildscripts = filter (\f -> "build.gradle" `isPrefixOf` fileName f) files

    if null buildscripts
      then walkContinue
      else do
        res <- exec dir gradleTasksCmd []
        case res of
          Left _ -> walkContinue
          Right _ -> do
            output (configure dir)
            walkSkipAll subdirs

strategy :: Strategy BasicDirOpts
strategy = Strategy
  { strategyName = "gradle-cli"
  , strategyAnalyze = analyze
  , strategyModule = targetDir
  , strategyOptimal = Optimal
  , strategyComplete = Complete
  }

initScript :: ByteString
initScript = $(embedFile "scripts/jsondeps.gradle")

analyze :: Members '[Embed IO, Resource, Exec, Error ExecErr] r => BasicDirOpts -> Sem r (Graphing Dependency)
analyze BasicDirOpts{..} =
  bracket (embed @IO (getTempDir >>= \tmp -> createTempDir tmp "fossa-gradle"))
          (embed @IO . removeDirRecur)
          act

  where

  act tmpDir = do
    let initScriptFilepath = fromAbsDir tmpDir FP.</> "jsondeps.gradle"
    embed (BS.writeFile initScriptFilepath initScript)
    stdout <- execThrow targetDir gradleJsonDepsCmd [initScriptFilepath]

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

    pure (buildGraph packagesToOutput)

-- TODO: use LabeledGraphing to add labels for environments
buildGraph :: Map Text [JsonDep] -> Graphing Dependency
buildGraph mapping = run . evalGrapher $ M.traverseWithKey addProject mapping
  where
  -- add top-level projects from the output
  addProject :: Member (Grapher Dependency) r => Text -> [JsonDep] -> Sem r ()
  addProject projName projDeps = do
    let projAsDep = projectToDep projName
    direct projAsDep
    for_ projDeps $ \dep -> do
      edge projAsDep (jsonDepToDep dep)
      mkRecursiveEdges dep

  -- build edges between deps, recursively
  mkRecursiveEdges :: Member (Grapher Dependency) r => JsonDep -> Sem r ()
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
      , dependencyTags = M.empty
      }

  projectToDep name = Dependency
    { dependencyType = SubprojectType
    , dependencyName = name
    , dependencyVersion = Nothing
    , dependencyLocations = []
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

configure :: Path Rel Dir -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicDirOpts
