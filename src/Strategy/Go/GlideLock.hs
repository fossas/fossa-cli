module Strategy.Go.GlideLock
  ( discover
  , strategy
  , analyze
  , configure

  , GlideLockfile(..)
  , GlideDep(..)

  , buildGraph
  )
  where

import Prologue hiding ((.=))

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Polysemy
import           Polysemy.Input
import           Polysemy.Output

import           DepTypes
import           Discovery.Walk
import           Effect.ReadFS
import           Graphing (Graphing, unfold)
import           Types

discover :: Discover
discover = Discover
  { discoverName = "gopkglock"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files ->
  case find (\f -> fileName f == "glide.lock") files of
    Nothing -> walkContinue
    Just file  -> do
      output (configure file)
      walkContinue

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "golang-glidelock"
  , strategyAnalyze = \opts -> analyze & fileInputYaml @GlideLockfile (targetFile opts)
  , strategyModule = parent . targetFile
  , strategyOptimal = Optimal
  , strategyComplete = NotComplete
  }

analyze :: Member (Input GlideLockfile) r => Sem r (Graphing Dependency)
analyze = buildGraph <$> input

buildGraph :: GlideLockfile -> Graphing Dependency
buildGraph lockfile = unfold direct (const []) toDependency
  where
  direct = imports lockfile
  toDependency GlideDep{..}  =
    Dependency { dependencyType = GoType
               , dependencyName = depName
               , dependencyVersion = Just (CEq $ T.pack (show depVersion))
               , dependencyLocations = []
               , dependencyTags = M.empty
               }

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts

data GlideLockfile = GlideLockfile
  { hash    :: Integer
  , updated :: Text
  , imports :: [GlideDep]
  } deriving (Eq, Ord, Show, Generic)

data GlideDep = GlideDep
  { depName    :: Text
  , depVersion :: Integer
  , depRepo    :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON GlideLockfile where
  parseJSON = withObject "GlideLockfile" $ \obj ->
    GlideLockfile <$> obj .: "hash"
                  <*> obj .: "updated"
                  <*> obj .: "imports"

instance FromJSON GlideDep where
  parseJSON = withObject "GlideDep" $ \obj ->
    GlideDep <$> obj .:  "name"
               <*> obj .: "version"
               <*> obj .:? "repo"
