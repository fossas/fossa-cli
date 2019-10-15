
{-# language QuasiQuotes #-}

module Strategy.Python.PipList
  ( discover
  , strategy
  , analyze
  , configure

  , PipListDep(..)
  , buildGraph
  )
  where

import Prologue

import qualified Data.Map.Strict as M
import           Polysemy
import           Polysemy.Input
import           Polysemy.Output

import           Discovery.Walk
import           Effect.Exec
import           Effect.GraphBuilder
import qualified Graph as G
import           Types

discover :: Discover
discover = Discover
  { discoverName = "piplist"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \dir _ files ->
  case find (\f -> fileName f `elem` ["setup.py", "requirements.txt"]) files of
    Nothing -> walkContinue
    Just _  -> do
      output (configure dir)
      walkContinue

pipListCmd :: Command
pipListCmd = Command
  { cmdNames = [ [relfile|pip3|]
               , [relfile|pip|]
               ]
  , cmdArgs = ["list", "--format=json"]
  }

strategy :: Strategy BasicDirOpts
strategy = Strategy
  { strategyName = "python-piplist"
  , strategyAnalyze = \opts -> analyze & execInputJson (targetDir opts) pipListCmd
  , strategyModule = targetDir
  , strategyComplete = False
  , strategyOptimal = False
  }

analyze :: Member (Input [PipListDep]) r => Sem r G.Graph
analyze = buildGraph <$> input

buildGraph :: [PipListDep] -> G.Graph
buildGraph xs = unfold xs (const []) toDependency
  where
  toDependency PipListDep{..} =
    G.Dependency { dependencyType = G.PipType
                 , dependencyName = depName
                 , dependencyVersion = Just (G.CEq depVersion)
                 , dependencyLocations = []
                 , dependencyTags = M.empty
                 }

configure :: Path Rel Dir -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicDirOpts

data PipListDep = PipListDep
  { depName    :: Text
  , depVersion :: Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON PipListDep where
  parseJSON = withObject "PipListDep" $ \obj ->
    PipListDep <$> obj .: "name"
               <*> obj .: "version"
