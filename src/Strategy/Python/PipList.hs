
module Strategy.Python.PipList
  ( discover
  , strategy
  , analyze
  , configure
  )
  where

import Prologue

import qualified Data.ByteString.Lazy.Char8 as BL8
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output

import qualified Graph as G
import           Discovery.Walk
import           Effect.Exec
import           Effect.GraphBuilder
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

-- TODO: pip effect
analyze :: Members '[Exec, Error CLIErr] r => BasicDirOpts -> Sem r G.Graph
analyze BasicDirOpts{..} = do
  (exitcode, stdout, stderr) <- exec targetDir "pip3" ["list", "--format=json"]
  when (exitcode /= ExitSuccess) (throw $ StrategyFailed $ "`pip list` returned an error: " <> BL8.unpack stderr)
  case eitherDecode stdout of
    Left err -> throw $ StrategyFailed err -- TODO: better error
    Right a -> pure $ buildGraph a

buildGraph :: [PipListDep] -> G.Graph
buildGraph xs = unfold xs (const []) toDependency
  where
  toDependency PipListDep{..} =
    G.Dependency { dependencyType = G.PipType
                 , dependencyName = depName
                 , dependencyVersion = Just depVersion
                 , dependencyLocations = []
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
