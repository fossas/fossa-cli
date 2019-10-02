
module Strategy.Python.Pipenv
  ( PipenvOpts(..)

  , discover
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

import           Config
import qualified Graph as G
import           Discovery.Walk
import           Effect.Exec
import           Effect.GraphBuilder
import           Strategy
import           Types

discover :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover = walk $ \dir _ files -> do
  case find (\f -> fileName f == "Pipfile.lock") files of
    Nothing -> walkContinue
    Just _  -> do
      output (configure dir)
      walkContinue

data PipenvOpts = PipenvOpts
  { pipenvOptsDir :: Path Rel Dir
  } deriving Show

instance FromJSON PipenvOpts where
  parseJSON = withObject "PipenvOpts" $ \obj ->
    PipenvOpts <$> obj .: "dir"

instance ToJSON PipenvOpts where
  toJSON PipenvOpts{..} = object ["dir" .= pipenvOptsDir]

strategy :: Strategy PipenvOpts
strategy = Strategy
  { strategyName = "python-piplist"
  , strategyAnalyze = analyze
  }

analyze :: Members '[Exec, Error CLIErr] r => PipenvOpts -> Sem r G.Graph
analyze PipenvOpts{..} = do
  (exitcode, stdout, stderr) <- exec pipenvOptsDir "pipenv" ["graph", "--json-tree"]
  when (exitcode /= ExitSuccess) (throw $ StrategyFailed $ "pipenv returned an error: " <> BL8.unpack stderr)
  case eitherDecode stdout of
    Left err -> throw $ StrategyFailed err -- TODO: better error
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
configure = ConfiguredStrategy strategy . PipenvOpts

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
