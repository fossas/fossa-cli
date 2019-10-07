
module Strategy.Python.Pipenv
  ( discover
  , strategy
  , analyze
  , configure
  )
  where

import Prologue

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
  { discoverName = "pipenv"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \dir _ files -> do
  case find (\f -> fileName f == "Pipfile.lock") files of
    Nothing -> walkContinue
    Just _  -> do
      output (configure dir)
      walkContinue

strategy :: Strategy BasicDirOpts
strategy = Strategy
  { strategyName = "python-pipenv"
  , strategyAnalyze = analyze
  , strategyModule = targetDir
  }

analyze :: Members '[Exec, Error CLIErr] r => BasicDirOpts -> Sem r G.Graph
analyze BasicDirOpts{..} = do
  stdout <- execThrow targetDir "pipenv" ["graph", "--json-tree"]
  case eitherDecode stdout of
    Left err -> throw $ CommandParseError "pipenv" err
    Right a -> pure $ buildGraph a

buildGraph :: [PipenvDep] -> G.Graph
buildGraph xs = unfold xs getDeps toDependency
  where
  getDeps dep = depDependencies dep

  toDependency PipenvDep{..} =
    G.Dependency { dependencyType = G.PipType
                 , dependencyName = depName
                 , dependencyVersion = Just depInstalled
                 , dependencyLocations = []
                 }

configure :: Path Rel Dir -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicDirOpts

data PipenvDep = PipenvDep
  { depName         :: Text
  , depInstalled    :: Text
  , depRequired     :: Text
  , depDependencies :: [PipenvDep]
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON PipenvDep where
  parseJSON = withObject "PipenvDep" $ \obj -> do
    PipenvDep <$> obj .: "package_name"
              <*> obj .: "installed_version"
              <*> obj .: "required_version"
              <*> obj .: "dependencies"
