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
import Polysemy
import Polysemy.Input
import Polysemy.Output

import DepTypes
import Discovery.Walk
import Effect.Exec
import Graphing
import Types

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
  { cmdNames = ["pip3", "pip"]
  , cmdBaseArgs = ["list", "--format=json"]
  , cmdAllowErr = Never
  }

strategy :: Strategy BasicDirOpts
strategy = Strategy
  { strategyName = "python-piplist"
  , strategyAnalyze = \opts -> analyze & execInputJson @[PipListDep] (targetDir opts) pipListCmd []
  , strategyModule = targetDir
  , strategyOptimal = NotOptimal
  , strategyComplete = NotComplete
  }

analyze :: Member (Input [PipListDep]) r => Sem r (Graphing Dependency)
analyze = buildGraph <$> input

buildGraph :: [PipListDep] -> Graphing Dependency
buildGraph xs = unfold xs (const []) toDependency
  where
  toDependency PipListDep{..} =
    Dependency { dependencyType = PipType
               , dependencyName = depName
               , dependencyVersion = Just (CEq depVersion)
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
