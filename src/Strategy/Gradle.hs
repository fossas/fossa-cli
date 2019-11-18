
{-# language TemplateHaskell #-}

module Strategy.Gradle
  ( discover
  , strategy

  , buildGraph
  , JsonDep(..)
  ) where

import Prologue hiding (json)

import           Data.Aeson.Types (Parser, unexpected)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.FileEmbed (embedFile)
import qualified Data.Map.Strict as M
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Path.IO (createTempDir, getTempDir, removeDirRecur)
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output
import           Polysemy.Resource
import qualified System.FilePath as FP

import           Diagnostics
import           Discovery.Walk
import           Effect.Exec
import           Effect.GraphBuilder
import qualified Graph as G
import           Types

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

analyze :: Members '[Embed IO, Resource, Exec, Error ExecErr] r => BasicDirOpts -> Sem r G.Graph
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
        textLines = T.lines text
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

    buildGraph packagesToOutput
      & evalGraphBuilder G.empty

buildGraph :: (Member (Error ExecErr) r, Member GraphBuilder r) => Map Text [JsonDep] -> Sem r ()
buildGraph mapping = do
  mappingWithRefs <- M.traverseWithKey addShallowProject mapping
  traverse_ (\(ref,_) -> addDirect ref) (M.elems mappingWithRefs)
  traverse_ (addProject mappingWithRefs) mappingWithRefs

  where
  projectToDep name = G.Dependency
    { dependencyType = G.SubprojectType
    , dependencyName = name
    , dependencyVersion = Nothing
    , dependencyLocations = []
    , dependencyTags = M.empty
    }

  addShallowProject :: Member GraphBuilder r => Text -> [JsonDep] -> Sem r (G.DepRef, [JsonDep])
  addShallowProject projName projDeps = do
    ref <- addNode (projectToDep projName)
    pure (ref, projDeps)

  addProject :: (Member (Error ExecErr) r, Member GraphBuilder r) => Map Text (G.DepRef, [JsonDep]) -> (G.DepRef, [JsonDep]) -> Sem r ()
  addProject projectToRef (projRef, projDeps) = do
    children <- traverse (addJsonDep projectToRef) projDeps
    traverse_ (addEdge projRef) children

  addJsonDep :: (Member (Error ExecErr) r, Member GraphBuilder r) => Map Text (G.DepRef, [JsonDep]) -> JsonDep -> Sem r G.DepRef
  addJsonDep projectToRef = \case
    ProjectDep name ->
      case M.lookup name projectToRef of
        Nothing -> throw (CommandParseError "" ("couldn't find project " <> name <> " when building graph")) -- TODO: better error or failure mode
        Just (ref, _) -> pure ref
    PackageDep name version deps -> do
      children <- traverse (addJsonDep projectToRef) deps
      ref <- addNode $ G.Dependency
        { dependencyType = G.MavenType
        , dependencyName = name
        , dependencyVersion = Just (G.CEq version)
        , dependencyLocations = []
        , dependencyTags = M.empty
        }
      traverse_ (addEdge ref) children
      pure ref

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
