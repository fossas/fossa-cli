
{-# language QuasiQuotes #-}

module Config
  ( ConfiguredStrategy(..)
  , ConfigErr(..)
  , loadConfig
  ) where

import Prologue

import qualified Data.Map as M
import qualified Data.Yaml as Yaml
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output

import           Effect.ReadFS
import           Strategy

data ConfigErr =
    ConfigParseFailed String
  | UnknownStrategyName String
  | StrategyOptionsParseFailed String String -- name of strategy, err
  deriving (Eq, Ord, Show, Generic, Typeable)

configPath :: Path Rel File
configPath = [relfile|.strategies.yml|]

loadConfig :: Members '[Error ConfigErr, Output ConfiguredStrategy, ReadFS] r => Map String SomeStrategy -> Path Abs Dir -> Sem r ()
loadConfig strategiesByName dir = do
  exists <- doesFileExist (dir </> configPath)
  when exists $ do
    contents <- readContents configPath

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
              Success a -> output @ConfiguredStrategy (ConfiguredStrategy strat a)

data ConfiguredStrategy where
  ConfiguredStrategy :: (FromJSON options, ToJSON options) => Strategy options -> options -> ConfiguredStrategy

instance ToJSON ConfiguredStrategy where
  toJSON (ConfiguredStrategy strategy options) = object ["name" .= strategyName strategy, "options" .= options]

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
