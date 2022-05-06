{-# LANGUAGE TemplateHaskell #-}

module Strategy.Maven.Plugin (
  withUnpackedPlugin,
  installPlugin,
  execPlugin,
  parsePluginOutput,
  depGraphPlugin,
  depGraphPluginLegacy,
  Artifact (..),
  DepGraphPlugin (..),
  Edge (..),
  PluginOutput (..),
) where

import Control.Algebra
import Control.Effect.Diagnostics
import Control.Effect.Exception
import Control.Effect.Lift (sendIO)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.FileEmbed (embedFile)
import Data.Functor (void)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Effect.Exec
import Effect.ReadFS
import Path
import Path.IO (createTempDir, getTempDir, removeDirRecur)
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
    , jar = $(embedFile "scripts/depgraph-maven-plugin-4.0.1.jar")
    }

depGraphPluginLegacy :: DepGraphPlugin
depGraphPluginLegacy =
  DepGraphPlugin
    { group = "com.github.ferstl"
    , artifact = "depgraph-maven-plugin"
    , version = "3.3.0"
    , jar = $(embedFile "scripts/depgraph-maven-plugin-3.3.0.jar")
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

execPlugin :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> DepGraphPlugin -> m ()
execPlugin dir plugin = void $ execThrow dir $ mavenPluginDependenciesCmd plugin

outputFile :: Path Rel File
outputFile = $(mkRelFile "target/dependency-graph.json")

parsePluginOutput :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m PluginOutput
parsePluginOutput dir = readContentsJson (dir </> outputFile)

mavenInstallPluginCmd :: FP.FilePath -> DepGraphPlugin -> Command
mavenInstallPluginCmd pluginFilePath plugin =
  Command
    { cmdName = "mvn"
    , cmdArgs =
        [ "install:install-file"
        , "-DgroupId=" <> group plugin
        , "-DartifactId=" <> artifact plugin
        , "-Dversion=" <> version plugin
        , "-Dpackaging=jar"
        , "-Dfile=" <> toText pluginFilePath
        ]
    , cmdAllowErr = Never
    }

mavenPluginDependenciesCmd :: DepGraphPlugin -> Command
mavenPluginDependenciesCmd plugin =
  Command
    { cmdName = "mvn"
    , cmdArgs =
        [ group plugin <> ":" <> artifact plugin <> ":" <> version plugin <> ":aggregate"
        , "-DgraphFormat=json"
        , "-DmergeScopes"
        , "-DreduceEdges=false"
        ]
    , cmdAllowErr = Never
    }

data PluginOutput = PluginOutput
  { outArtifacts :: [Artifact]
  , outEdges :: [Edge]
  }
  deriving (Eq, Ord, Show)

-- NOTE: artifact numeric IDs are 1-indexed, whereas edge numeric references are 0-indexed.
-- the json parser for artifacts converts them to be 0-indexed.
data Artifact = Artifact
  { artifactNumericId :: Int
  , artifactGroupId :: Text
  , artifactArtifactId :: Text
  , artifactVersion :: Text
  , artifactOptional :: Bool
  , artifactScopes :: [Text]
  }
  deriving (Eq, Ord, Show)

data Edge = Edge
  { edgeFrom :: Int
  , edgeTo :: Int
  }
  deriving (Eq, Ord, Show)

instance FromJSON PluginOutput where
  parseJSON = withObject "PluginOutput" $ \obj ->
    PluginOutput <$> obj .:? "artifacts" .!= []
      <*> obj .:? "dependencies" .!= []

instance FromJSON Artifact where
  parseJSON = withObject "Artifact" $ \obj ->
    Artifact <$> (offset <$> obj .: "numericId")
      <*> obj .: "groupId"
      <*> obj .: "artifactId"
      <*> obj .: "version"
      <*> obj .: "optional"
      <*> obj .: "scopes"
    where
      offset = subtract 1

instance FromJSON Edge where
  parseJSON = withObject "Edge" $ \obj ->
    Edge <$> obj .: "numericFrom"
      <*> obj .: "numericTo"
