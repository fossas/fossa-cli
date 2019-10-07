
{-# language QuasiQuotes #-}

module Discovery.Config
  ( ConfiguredStrategy(..)
  , loadConfig
  ) where

import Prologue

import qualified Data.Map as M
import qualified Data.Yaml as Yaml
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output

import Diagnostics
import Effect.ReadFS
import Types

configPath :: Path Rel File
configPath = [relfile|.strategies.yml|]

loadConfig :: [StrategyGroup] -> Discover
loadConfig groups = Discover
  { discoverName = "config"
  , discoverFunc = loadConfig' groups
  }

loadConfig' :: forall r. Members '[Error CLIErr, Output ConfiguredStrategy, ReadFS] r => [StrategyGroup] -> Path Abs Dir -> Sem r ()
loadConfig' strategies dir = do
  exists <- doesFileExist (dir </> configPath)
  when exists $ do
    contents <- readContentsBS configPath

    -- code is disgusting, but it's using three different failure types:
    -- - Either ParseException (yaml)
    -- - Maybe
    -- - Result (aeson)
    case Yaml.decodeEither' contents of
      Left err -> throw (ConfigParseFailed (Yaml.prettyPrintParseException err))
      Right Config{configStrategies} -> do
        for_ configStrategies $ \ConfigStrategy{..} -> do
          case M.lookup configStrategyName strategiesByName of
            Nothing -> throw (UnknownStrategyName configStrategyName)
            Just (SomeStrategy strat) -> case fromJSON configStrategyOptions of
              Error err -> throw (StrategyOptionsParseFailed configStrategyName err)
              Success a -> output (ConfiguredStrategy strat a)
  where
  strategiesByName :: Map String SomeStrategy
  strategiesByName = M.fromList (map (\strategy@(SomeStrategy Strategy{strategyName}) -> (strategyName, strategy)) (groupStrategies =<< strategies))


data Config = Config
  { configStrategies :: [ConfigStrategy]
  } deriving (Generic)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \obj -> Config <$> obj .: "strategies"

instance FromJSON ConfigStrategy where
  parseJSON = withObject "ConfigStrategy" $ \obj ->
    ConfigStrategy <$> obj .: "name"
                   <*> obj .:? "options"
                           .!= object [] -- default to empty object

data ConfigStrategy = ConfigStrategy
  { configStrategyName    :: String
  , configStrategyOptions :: Value
  } deriving (Show, Generic)
