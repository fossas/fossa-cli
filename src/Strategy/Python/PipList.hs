
module Strategy.Python.PipList
  ( discover
  , strategy
  , analyze
  , configure
  )
  where

import Prologue

import qualified Data.Map.Strict as M
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output

import           Diagnostics
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
discover' = walk $ \dir _ files -> do
  case find (\f -> fileName f `elem` ["setup.py", "requirements.txt"]) files of
    Nothing -> walkContinue
    Just _  -> do
      output (configure dir)
      walkContinue

strategy :: Strategy BasicDirOpts
strategy = Strategy
  { strategyName = "python-piplist"
  , strategyAnalyze = analyze
  , strategyModule = targetDir
  }

analyze :: Members '[Exec, Error CLIErr] r => BasicDirOpts -> Sem r G.Graph
analyze BasicDirOpts{..} = do
  stdout <- execThrow targetDir "pip3" ["list", "--format=json"]
  case eitherDecode stdout of
    Left err -> throw $ CommandParseError "pip" err
    Right a -> pure $ buildGraph a

buildGraph :: [PipListDep] -> G.Graph
buildGraph xs = unfold xs (const []) toDependency
  where
  toDependency PipListDep{..} =
    G.Dependency { dependencyType = G.PipType
                 , dependencyName = depName
                 , dependencyVersion = Just depVersion
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
  parseJSON = withObject "PipListDep" $ \obj -> do
    PipListDep <$> obj .: "name"
               <*> obj .: "version"
