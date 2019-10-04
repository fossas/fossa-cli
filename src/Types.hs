{-# language TemplateHaskell #-}

module Types
  ( CLIErr(..)

  , DiscoverEffs
  , Discover(..)
  , StrategyEffs
  , Strategy(..)
  , StrategyGroup(..)

  , ConfiguredStrategy(..)
  , SomeStrategy(..)

  , BasicDirOpts(..)
  , BasicFileOpts(..)
  ) where

import Prologue

import Polysemy
import Polysemy.Error
import Polysemy.Output

import Effect.Exec
import Effect.ReadFS
import Graph

---------- Errors

data CLIErr =
    -- Configuration errors
    ConfigParseFailed String
  | UnknownStrategyName String
  | StrategyOptionsParseFailed String String -- name of strategy, err

    -- Strategy execution errors
    -- TODO: new-cli-error-style reporting
  | StrategyFailed String -- name of strategy, err
  deriving (Eq, Ord, Show, Generic, Typeable)

---------- Discovery

type DiscoverEffs r = Members '[Embed IO, Error CLIErr, Exec, ReadFS, Output ConfiguredStrategy] r

data Discover = Discover
  { discoverName :: String
  , discoverFunc :: forall r. DiscoverEffs r => Path Abs Dir -> Sem r ()
  }


---------- Strategies

type StrategyEffs r = Members '[Embed IO, Error CLIErr, Exec, ReadFS] r

data Strategy options = Strategy
  { strategyName    :: String
  , strategyAnalyze :: forall r. StrategyEffs r => options -> Sem r Graph
  , strategyModule  :: options -> Path Rel Dir
  }

data StrategyGroup = StrategyGroup
  { groupName       :: String
  , groupStrategies :: [SomeStrategy]
  }

data SomeStrategy where
  SomeStrategy :: (FromJSON options, ToJSON options) => Strategy options -> SomeStrategy

---------- Configured Strategies
data ConfiguredStrategy where
  ConfiguredStrategy :: (FromJSON options, ToJSON options) => Strategy options -> options -> ConfiguredStrategy

instance ToJSON ConfiguredStrategy where
  toJSON (ConfiguredStrategy strategy options) = object ["name" .= strategyName strategy, "options" .= options]

data BasicDirOpts = BasicDirOpts
  { targetDir :: Path Rel Dir
  } deriving (Eq, Ord, Show, Generic)

---------- Basic Opts
instance FromJSON BasicDirOpts where
  parseJSON = withObject "BasicDirOpts" $ \obj ->
    BasicDirOpts <$> obj .: "dir"

instance ToJSON BasicDirOpts where
  toJSON BasicDirOpts{..} = object ["dir" .= targetDir]

data BasicFileOpts = BasicFileOpts
  { targetFile :: Path Rel File
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON BasicFileOpts where
  parseJSON = withObject "BasicFileOpts" $ \obj ->
    BasicFileOpts <$> obj .: "file"

instance ToJSON BasicFileOpts where
  toJSON BasicFileOpts{..} = object ["file" .= targetFile]
