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
) where

import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, warn)
import Control.Effect.Exception (Lift, bracket)
import Control.Effect.Lift (sendIO)
import Control.Monad (when)
import Data.Aeson (FromJSON, parseJSON, withObject, (.!=), (.:), (.:?))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.FileEmbed.Extra (embedFile')
import Data.Foldable (Foldable (fold), foldl')
import Data.Functor (void)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, isNothing)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Traversable (for)
import Data.Tree (Tree (..))
import Effect.Exec (
  AllowErr (Never),
  Command (..),
  Exec,
  execThrow,
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
  mkRelDir,
  mkRelFile,
  (</>),
 )
import Path.IO (createTempDir, getTempDir, removeDirRecur)
import Strategy.Maven.PluginTree (TextArtifact (..), parseTextArtifact)
import System.FilePath qualified as FP

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

installPlugin :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> FP.FilePath -> DepGraphPlugin -> m ()
installPlugin dir path plugin = void $ execThrow dir (mavenInstallPluginCmd path plugin)

execPlugin :: (Has Exec sig m, Has Diagnostics sig m) => (DepGraphPlugin -> Command) -> Path Abs Dir -> DepGraphPlugin -> m ()
execPlugin pluginToCmd dir plugin = void $ execThrow dir $ pluginToCmd plugin

execPluginAggregate :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> DepGraphPlugin -> m ()
execPluginAggregate = execPlugin mavenPluginDependenciesCmd

execPluginReactor :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> DepGraphPlugin -> m ()
execPluginReactor = execPlugin mavenPluginReactorCmd

outputFile :: Path Rel File
outputFile = $(mkRelFile "target/dependency-graph.txt")

parsePluginOutput :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m PluginOutput
parsePluginOutput dir =
  readContentsParser parseTextArtifact (dir </> outputFile) >>= textArtifactToPluginOutput

reactorOutputFilename :: Path Rel File
reactorOutputFilename = $(mkRelFile "fossa-reactor-graph.json")

parseReactorOutput :: (Has ReadFS sig m, Has Diagnostics sig m) => (Path Abs Dir) -> m ReactorOutput
parseReactorOutput dir = readContentsJson $ dir </> $(mkRelDir "target/") </> reactorOutputFilename

textArtifactToPluginOutput :: Has Diagnostics sig m => Tree TextArtifact -> m PluginOutput
textArtifactToPluginOutput
  ta = buildPluginOutput ta
    where
      artifactNames :: [Text]
      artifactNames = foldl' (\a c -> (artifactText c : a)) mempty ta

      namesToIds :: Map Text Int
      namesToIds = Map.fromList . (\ns -> zip ns [0 ..]) $ artifactNames

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

      lookupArtifactByName :: Has Diagnostics sig m => Text -> m (Maybe Int)
      lookupArtifactByName aText = do
        let res = Map.lookup aText namesToIds
        when (isNothing res) $
          warn $ "Could not find artifact with name " <> aText
        pure res

      buildEdges :: Has Diagnostics sig m => Int -> [Tree TextArtifact] -> m [Edge]
      buildEdges parentId children =
        catMaybes
          <$> for
            (map rootLabel children)
            (\c -> fmap (Edge parentId) <$> lookupArtifactByName (artifactText c))

      buildPluginOutput :: Has Diagnostics sig m => Tree TextArtifact -> m PluginOutput
      buildPluginOutput
        (Node t@(TextArtifact{artifactText = aText}) aChildren) = do
          maybeId <- lookupArtifactByName aText
          childInfo@PluginOutput
            { outArtifacts = cArtifacts
            , outEdges = cEdges
            } <-
            fold <$> traverse buildPluginOutput aChildren
          case maybeId of
            Nothing -> do
              warn ("Could not find id for artifact " <> aText)
              pure childInfo
            Just numericId -> do
              let artifact = textArtifactToArtifact numericId t
              newEdges <- buildEdges numericId aChildren
              pure
                childInfo
                  { outArtifacts = artifact : cArtifacts
                  , outEdges = newEdges <> cEdges
                  }

mavenInstallPluginCmd :: FP.FilePath -> DepGraphPlugin -> Command
mavenInstallPluginCmd pluginFilePath plugin =
  Command
    { cmdName = "mvn"
    , cmdArgs =
        [ "org.apache.maven.plugins:maven-install-plugin:3.0.0-M1:install-file"
        , "-DgroupId=" <> group plugin
        , "-DartifactId=" <> artifact plugin
        , "-Dversion=" <> version plugin
        , "-Dpackaging=jar"
        , "-Dfile=" <> toText pluginFilePath
        ]
    , cmdAllowErr = Never
    }

-- |The aggregate command is documented
-- [here.](https://ferstl.github.io/depgraph-maven-plugin/aggregate-mojo.html)
mavenPluginDependenciesCmd :: DepGraphPlugin -> Command
mavenPluginDependenciesCmd plugin =
  Command
    { cmdName = "mvn"
    , cmdArgs =
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
    , cmdAllowErr = Never
    }

-- |The reactor command is documented
-- [here.](https://ferstl.github.io/depgraph-maven-plugin/reactor-mojo.html)
mavenPluginReactorCmd :: DepGraphPlugin -> Command
mavenPluginReactorCmd plugin =
  Command
    { cmdName = "mvn"
    , cmdArgs =
        [ group plugin <> ":" <> artifact plugin <> ":" <> version plugin <> ":reactor"
        , "-DgraphFormat=json"
        , "-DoutputFileName=" <> toText reactorOutputFilename
        ]
    , cmdAllowErr = Never
    }

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
