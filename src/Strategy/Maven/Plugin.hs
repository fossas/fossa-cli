{-# LANGUAGE RecordWildCards #-}
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
  textArtifactToPluginOutput,
) where

import Control.Algebra
import Control.Effect.Diagnostics
import Control.Effect.Exception
import Control.Effect.Lift (sendIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.FileEmbed (embedFile)
import Data.Functor (void)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String.Conversion (toText)
import Data.Text (Text)
import Effect.Exec
import Effect.ReadFS
import Path
import Path.IO (createTempDir, getTempDir, removeDirRecur)
import Strategy.Maven.PluginTree (TextArtifact (..), foldTextArtifactl, parseTextArtifact, foldTextArtifactM)
import System.FilePath qualified as FP
import qualified Data.Text as Text

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
outputFile = $(mkRelFile "target/dependency-graph.txt")

parsePluginOutput :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m PluginOutput
parsePluginOutput dir =
  do contents <- readContentsParser parseTextArtifact (dir </> outputFile)
     case textArtifactToPluginOutput contents of
       Left (MissingArtifact t) -> fatal $ "Could not find an id for artifact " <> t
       Left (UnparseableName t) -> fatal $ "Could not parse name for artifact " <> t
       Right po -> pure po

newtype ArtifactInfo = ArtifactInfo
  { artifacts :: [Artifact] }
  deriving (Eq, Ord, Show)

emptyArtifactInfo :: ArtifactInfo
emptyArtifactInfo = ArtifactInfo []

data ConversionError
  = MissingArtifact Text
  | UnparseableName Text
  deriving (Eq, Show)

textArtifactToPluginOutput :: TextArtifact -> Either ConversionError PluginOutput
textArtifactToPluginOutput
  ta = do ArtifactInfo {..} <- foldTextArtifactM foldFn emptyArtifactInfo ta
          Right PluginOutput {outArtifacts = artifacts, outEdges = []}
  where
      artifactNames :: [Text]
      artifactNames = foldTextArtifactl (\a c -> (artifactText c : a)) mempty ta

      namesToIds :: Map Text Int
      namesToIds = Map.fromList . (\ns -> zip ns [0 ..]) $ artifactNames

      textArtifactToArtifact :: Int -> TextArtifact -> Either ConversionError Artifact
      textArtifactToArtifact numericId TextArtifact{..} = 
        case Text.splitOn ":" artifactText of
          [groupId, artifactId, version] -> Right $
            Artifact {
            artifactNumericId = numericId
            , artifactGroupId = groupId
            , artifactArtifactId = artifactId
            , artifactVersion = version
            , artifactScopes = scopes
            , artifactOptional = isOptional
            }
          _ -> Left . UnparseableName $ artifactText 
          
      foldFn :: ArtifactInfo ->  TextArtifact -> Either ConversionError ArtifactInfo
      foldFn acc@(ArtifactInfo{..}) t@TextArtifact{artifactText = aText} =
        case Map.lookup aText namesToIds of
          Nothing -> Left . MissingArtifact $ "Could not find artifact with name " <> aText
          Just numericId -> do artifact <- textArtifactToArtifact numericId t 
                               Right $ acc{artifacts = artifact : artifacts}
                               

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
        , "-DgraphFormat=text"
        , "-DmergeScopes"
        , "-DreduceEdges=false"
        , "-DshowVersions=true"
        , "-DshowGroupIds=true"
        , "-DshowOptional=true"
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
  , artifactScopes :: [Text]
  , artifactOptional :: Bool
  }
  deriving (Eq, Ord, Show)

data Edge = Edge
  { edgeFrom :: Int
  , edgeTo :: Int
  }
  deriving (Eq, Ord, Show)
