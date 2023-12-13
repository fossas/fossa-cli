{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Strategy.Maven.Plugin (
  withUnpackedPlugin,
  installPlugin,
  parsePluginOutput,
  depGraphPlugin,
  depGraphPluginLegacy,
  Artifact (..),
  DepGraphPlugin (..),
  Edge (..),
  PluginOutput (..),
  ReactorOutput (..),
  ReactorArtifact (..),
  parseReactorOutput,
  textArtifactToPluginOutput,
  execPluginAggregate,
  execPluginReactor,
  mavenCmdCandidates,
) where

import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, recover, warn)
import Control.Effect.Exception (Lift, bracket)
import Control.Effect.Lift (sendIO)
import Control.Monad (when)
import Data.Aeson (FromJSON, parseJSON, withObject, (.!=), (.:), (.:?))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.FileEmbed.Extra (embedFile')
import Data.Foldable (Foldable (fold), foldl')
import Data.Functor (void)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, isNothing)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Traversable (for)
import Data.Tree (Tree (..))
import DepTypes (DepType (MavenType))
import Discovery.Walk (findFileInAncestor)
import Effect.Exec (
  AllowErr (Never),
  CandidateAnalysisCommands (..),
  CandidateCommandEffs,
  Command (..),
  Exec,
  execThrow,
  mkAnalysisCommand,
 )
import Effect.ReadFS (
  ReadFS,
  readContentsJson,
  readContentsParser,
 )
import Path (
  Abs,
  Dir,
  File,
  Path,
  Rel,
  fromAbsDir,
  mkRelFile,
  (</>),
 )
import Path.IO (createTempDir, getTempDir, removeDirRecur)
import Strategy.Maven.PluginTree (TextArtifact (..), parseTextArtifact)
import System.FilePath qualified as FP
import System.Info qualified as SysInfo

data DepGraphPlugin = DepGraphPlugin
  { group :: Text
  , artifact :: Text
  , version :: Text
  , jar :: ByteString
  }
  deriving (Eq, Ord, Show)

depGraphPlugin :: DepGraphPlugin
depGraphPlugin =
  DepGraphPlugin
    { group = "com.github.ferstl"
    , artifact = "depgraph-maven-plugin"
    , version = "4.0.1"
    , jar = $(embedFile' "scripts/depgraph-maven-plugin-4.0.1.jar")
    }

depGraphPluginLegacy :: DepGraphPlugin
depGraphPluginLegacy =
  DepGraphPlugin
    { group = "com.github.ferstl"
    , artifact = "depgraph-maven-plugin"
    , version = "3.3.0"
    , jar = $(embedFile' "scripts/depgraph-maven-plugin-3.3.0.jar")
    }

withUnpackedPlugin ::
  ( Has (Lift IO) sig m
  ) =>
  DepGraphPlugin ->
  (FP.FilePath -> m a) ->
  m a
withUnpackedPlugin plugin act =
  bracket
    (sendIO (getTempDir >>= \tmp -> createTempDir tmp "fossa-maven"))
    (sendIO . removeDirRecur)
    go
  where
    go tmpDir = do
      let pluginJarFilepath = fromAbsDir tmpDir FP.</> "plugin.jar"
      sendIO (BS.writeFile pluginJarFilepath $ jar plugin)

      act pluginJarFilepath

installPlugin :: (CandidateCommandEffs sig m, Has ReadFS sig m) => Path Abs Dir -> FP.FilePath -> DepGraphPlugin -> m ()
installPlugin dir path plugin = do
  cmd <- mavenInstallPluginCmd dir path plugin
  void $ execThrow dir cmd

execPlugin :: (Has Exec sig m, Has Diagnostics sig m) => (DepGraphPlugin -> Command) -> Path Abs Dir -> DepGraphPlugin -> m ()
execPlugin pluginToCmd dir plugin = void $ execThrow dir $ pluginToCmd plugin

execPluginAggregate :: (CandidateCommandEffs sig m, Has ReadFS sig m) => Path Abs Dir -> DepGraphPlugin -> m ()
execPluginAggregate dir plugin = do
  cmd <- mavenPluginDependenciesCmd dir plugin
  execPlugin (const cmd) dir plugin

execPluginReactor :: (CandidateCommandEffs sig m, Has ReadFS sig m) => Path Abs Dir -> Path Abs Dir -> DepGraphPlugin -> m ()
execPluginReactor projectdir outputdir plugin = do
  cmd <- mavenPluginReactorCmd projectdir outputdir plugin
  execPlugin (const cmd) projectdir plugin

outputFile :: Path Rel File
outputFile = $(mkRelFile "target/dependency-graph.txt")

parsePluginOutput :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m PluginOutput
parsePluginOutput dir =
  readContentsParser parseTextArtifact (dir </> outputFile) >>= textArtifactToPluginOutput

reactorOutputFilename :: Path Rel File
reactorOutputFilename = $(mkRelFile "fossa-reactor-graph.json")

parseReactorOutput :: (Has ReadFS sig m, Has Diagnostics sig m) => (Path Abs Dir) -> m ReactorOutput
parseReactorOutput dir = readContentsJson $ dir </> reactorOutputFilename

textArtifactToPluginOutput :: Has Diagnostics sig m => Tree TextArtifact -> m PluginOutput
textArtifactToPluginOutput
  ta = buildPluginOutput ta
    where
      artifacts :: [TextArtifact]
      artifacts = nub $ foldl' (flip (:)) mempty ta

      artifactToIds :: Map TextArtifact Int
      artifactToIds = Map.fromList . (\ns -> zip ns [0 ..]) $ artifacts

      textArtifactToArtifact :: Int -> TextArtifact -> Artifact
      textArtifactToArtifact numericId TextArtifact{..} =
        Artifact
          { artifactNumericId = numericId
          , artifactGroupId = groupId
          , artifactArtifactId = artifactId
          , artifactVersion = textArtifactVersion
          , artifactScopes = scopes
          , artifactOptional = isOptional
          , artifactIsDirect = isDirect
          }

      lookupArtifact :: Has Diagnostics sig m => TextArtifact -> m (Maybe Int)
      lookupArtifact artifactToLook = do
        let res = Map.lookup artifactToLook artifactToIds
        when (isNothing res) $
          warn $
            "Could not find artifact: " <> show artifactToLook
        pure res

      buildEdges :: Has Diagnostics sig m => Int -> [Tree TextArtifact] -> m [Edge]
      buildEdges parentId children =
        catMaybes
          <$> for
            (map rootLabel children)
            (fmap (fmap $ Edge parentId) . lookupArtifact)

      buildPluginOutput :: Has Diagnostics sig m => Tree TextArtifact -> m PluginOutput
      buildPluginOutput
        (Node t aChildren) = do
          maybeId <- lookupArtifact t
          childInfo@PluginOutput
            { outArtifacts = cArtifacts
            , outEdges = cEdges
            } <-
            fold <$> traverse buildPluginOutput aChildren
          case maybeId of
            Nothing -> do
              warn ("Could not find id for artifact: " <> show t)
              pure childInfo
            Just numericId -> do
              let artifact = textArtifactToArtifact numericId t
              newEdges <- buildEdges numericId aChildren
              pure
                childInfo
                  { outArtifacts = artifact : cArtifacts
                  , outEdges = newEdges <> cEdges
                  }

-- | Search for maven wrappers in the closest parent directory and if found use them as a candidate command.
mavenCmdCandidates :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m CandidateAnalysisCommands
mavenCmdCandidates dir =
  mvnWrapperPath >>= \case
    Just wrapper -> pure . mkCmd $ (toText wrapper) :| ["mvn"]
    Nothing -> pure . mkCmd $ NE.singleton "mvn"
  where
    runningWindows :: Bool
    runningWindows = SysInfo.os == "mingw32"
    mkCmd :: NonEmpty Text -> CandidateAnalysisCommands
    mkCmd cmds = CandidateAnalysisCommands cmds ["-v"] $ Just MavenType
    -- Unlike with the gradle wrapper, it's not _expected_ for maven projects to use the maven wrapper.
    -- Given that, don't warn on failure to find a wrapper; only warn if we find a wrapper and it fails to execute.
    mvnWrapperPath :: (Has ReadFS sig m, Has Diagnostics sig m) => m (Maybe (Path Abs File))
    mvnWrapperPath =
      recover $
        if runningWindows
          then findFileInAncestor dir "mvnw.bat"
          else findFileInAncestor dir "mvnw"

mavenInstallPluginCmd :: (CandidateCommandEffs sig m, Has ReadFS sig m) => Path Abs Dir -> FP.FilePath -> DepGraphPlugin -> m Command
mavenInstallPluginCmd workdir pluginFilePath plugin = do
  candidates <- mavenCmdCandidates workdir
  mkAnalysisCommand candidates workdir args Never
  where
    args =
      [ "org.apache.maven.plugins:maven-install-plugin:3.0.0-M1:install-file"
      , "-DgroupId=" <> group plugin
      , "-DartifactId=" <> artifact plugin
      , "-Dversion=" <> version plugin
      , "-Dpackaging=jar"
      , "-Dfile=" <> toText pluginFilePath
      ]

-- | The aggregate command is documented
--  [here.](https://ferstl.github.io/depgraph-maven-plugin/aggregate-mojo.html)
mavenPluginDependenciesCmd :: (CandidateCommandEffs sig m, Has ReadFS sig m) => Path Abs Dir -> DepGraphPlugin -> m Command
mavenPluginDependenciesCmd workdir plugin = do
  candidates <- mavenCmdCandidates workdir
  mkAnalysisCommand candidates workdir args Never
  where
    args =
      [ group plugin <> ":" <> artifact plugin <> ":" <> version plugin <> ":aggregate"
      , "-DgraphFormat=text"
      , -- display deps that appear multiple times in different scopes as a single node
        "-DmergeScopes"
      , -- Don't omit edges for deps appearing in multiple places in the graph
        "-DreduceEdges=false"
      , "-DshowVersions=true"
      , "-DshowGroupIds=true"
      , -- Deps that are optional in the graph will be tagged optional in the cli's output
        -- this does not exclude them from sourceUnits.
        "-DshowOptional=true"
      , -- Repeat transitive deps for packages that appear multiple times
        "-DrepeatTransitiveDependenciesInTextGraph=true"
      ]

-- | The reactor command is documented
--  [here.](https://ferstl.github.io/depgraph-maven-plugin/reactor-mojo.html)
--  We set outputDirectory explicitly so that the file is written to a known spot even if the pom file
--  overrides the build directory (See FDN-82 for more details)
mavenPluginReactorCmd :: (CandidateCommandEffs sig m, Has ReadFS sig m) => Path Abs Dir -> Path Abs Dir -> DepGraphPlugin -> m Command
mavenPluginReactorCmd workdir outputdir plugin = do
  candidates <- mavenCmdCandidates workdir
  mkAnalysisCommand candidates workdir args Never
  where
    args =
      [ group plugin <> ":" <> artifact plugin <> ":" <> version plugin <> ":reactor"
      , "-DgraphFormat=json"
      , "-DoutputFileName=" <> toText reactorOutputFilename
      , "-DoutputDirectory=" <> toText outputdir
      ]

newtype ReactorArtifact = ReactorArtifact
  { reactorArtifactName :: Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON ReactorArtifact where
  parseJSON = withObject "Reactor artifact" $
    \o -> ReactorArtifact <$> o .: "artifactId"

newtype ReactorOutput = ReactorOutput {reactorArtifacts :: [ReactorArtifact]}
  deriving (Eq, Ord, Show)

instance FromJSON ReactorOutput where
  parseJSON = withObject "Reactor output" $
    \o -> ReactorOutput <$> (o .:? "artifacts" .!= [])

data PluginOutput = PluginOutput
  { outArtifacts :: [Artifact]
  , outEdges :: [Edge]
  }
  deriving (Eq, Ord, Show)

instance Semigroup PluginOutput where
  PluginOutput
    { outArtifacts = p1Arts
    , outEdges = p1Edges
    }
    <> PluginOutput
      { outArtifacts = p2Arts
      , outEdges = p2Edges
      } = PluginOutput (p1Arts <> p2Arts) (p1Edges <> p2Edges)

instance Monoid PluginOutput where
  mempty = PluginOutput [] []

data Artifact = Artifact
  { artifactNumericId :: Int
  , artifactGroupId :: Text
  , artifactArtifactId :: Text
  , artifactVersion :: Text
  , artifactScopes :: [Text]
  , artifactOptional :: Bool
  , artifactIsDirect :: Bool
  }
  deriving (Eq, Ord, Show)

data Edge = Edge
  { edgeFrom :: Int
  , edgeTo :: Int
  }
  deriving (Eq, Ord, Show)
