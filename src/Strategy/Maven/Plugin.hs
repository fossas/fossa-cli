{-# language TemplateHaskell #-}

module Strategy.Maven.Plugin
  ( withUnpackedPlugin
  , installPlugin
  , execPlugin
  , parsePluginOutput

  , PluginOutput(..)
  , Artifact(..)
  , Edge(..)
  ) where

import Prologue

import Control.Algebra
import Control.Effect.Diagnostics
import Control.Effect.Exception
import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile)
import Path.IO (createTempDir, getTempDir, removeDirRecur)
import qualified System.FilePath as FP

import Effect.Exec
import Effect.ReadFS

pluginGroup :: String
pluginGroup = "com.github.ferstl"

pluginArtifact :: String
pluginArtifact = "depgraph-maven-plugin"

pluginVersion :: String
pluginVersion = "3.3.0"

pluginJar :: ByteString
pluginJar = $(embedFile "scripts/depgraph-maven-plugin-3.3.0.jar")

withUnpackedPlugin ::
  ( Has (Lift IO) sig m
  , MonadIO m
  )
  => (FP.FilePath -> m a) -> m a
withUnpackedPlugin act =
  bracket (liftIO (getTempDir >>= \tmp -> createTempDir tmp "fossa-maven"))
          (liftIO . removeDirRecur)
          go

  where

  go tmpDir = do
    let pluginJarFilepath = fromAbsDir tmpDir FP.</> "plugin.jar"
    liftIO (BS.writeFile pluginJarFilepath pluginJar)

    act pluginJarFilepath

installPlugin :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> FP.FilePath -> m ()
installPlugin dir path = void $ execThrow dir (mavenInstallPluginCmd path)

execPlugin :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> m ()
execPlugin dir = void $ execThrow dir mavenPluginDependenciesCmd

outputFile :: Path Rel File
outputFile = $(mkRelFile "target/dependency-graph.json")

parsePluginOutput :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m PluginOutput
parsePluginOutput dir = readContentsJson (dir </> outputFile)

mavenInstallPluginCmd :: FP.FilePath -> Command
mavenInstallPluginCmd pluginFilePath = Command
  { cmdName = "mvn"
  , cmdArgs =
    [ "install:install-file"
    , "-DgroupId=" <> pluginGroup
    , "-DartifactId=" <> pluginArtifact
    , "-Dversion=" <> pluginVersion
    , "-Dpackaging=jar"
    , "-Dfile=" <> pluginFilePath
    ]
  , cmdAllowErr = Never
  }

mavenPluginDependenciesCmd :: Command
mavenPluginDependenciesCmd = Command
  { cmdName = "mvn"
  , cmdArgs =
    [ pluginGroup <> ":" <> pluginArtifact <> ":" <> pluginVersion <> ":aggregate"
    , "-DgraphFormat=json"
    , "-DmergeScopes"
    , "-DreduceEdges=false"
    ]
  , cmdAllowErr = Never
  }

data PluginOutput = PluginOutput
  { outArtifacts :: [Artifact]
  , outEdges     :: [Edge]
  } deriving (Eq, Ord, Show, Generic)

-- NOTE: artifact numeric IDs are 1-indexed, whereas edge numeric references are 0-indexed.
-- the json parser for artifacts converts them to be 0-indexed.
data Artifact = Artifact
  { artifactNumericId  :: Int
  , artifactGroupId    :: Text
  , artifactArtifactId :: Text
  , artifactVersion    :: Text
  , artifactOptional   :: Bool
  , artifactScopes     :: [Text]
  } deriving (Eq, Ord, Show, Generic)

data Edge = Edge
  { edgeFrom :: Int
  , edgeTo   :: Int
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON PluginOutput where
  parseJSON = withObject "PluginOutput" $ \obj ->
    PluginOutput <$> obj .:? "artifacts"    .!= []
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
