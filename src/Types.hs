module Types
  ( DiscoverEffs
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
import Diagnostics
import Graph

---------- Discovery

-- | The effects available for use in 'Discover'
type DiscoverEffs r = Members '[Embed IO, Error CLIErr, Exec, ReadFS, Output ConfiguredStrategy] r

-- | Discover functions produce 'ConfiguredStrategy's, given a base directory
-- to search
data Discover = Discover
  { discoverName :: String
  , discoverFunc :: forall r. DiscoverEffs r => Path Abs Dir -> Sem r ()
  }


---------- Strategies

-- | The effects available for use in 'Strategy'
type StrategyEffs r = Members '[Embed IO, Error CLIErr, Exec, ReadFS] r

-- | Strategies produce dependency graphs
--
-- @options@ must have 'ToJSON' and 'FromJSON' instances -- these are used to
-- serialize\/deserialize a strategy's options to/from disk
data Strategy options = Strategy
  { strategyName    :: String -- ^ e.g., "python-pipenv"
  , strategyAnalyze :: forall r. StrategyEffs r => options -> Sem r Graph
  , strategyModule  :: options -> Path Rel Dir -- ^ Determine the module directory for grouping with other strategies
  }

-- | 'Strategy' outputs are grouped and sorted based on the provided @StrategyGroup@s
--
-- For example, @"python"@ is a @StrategyGroup@ that has pipenv, piplist, ... as strategies
data StrategyGroup = StrategyGroup
  { groupName       :: String -- ^ e.g., "python"
  , groupStrategies :: [SomeStrategy]
  }

-- | An arbitrary 'Strategy', suitable for use in lists, map values, ...
-- Used to construct 'StrategyGroup's
data SomeStrategy where
  SomeStrategy :: (FromJSON options, ToJSON options) => Strategy options -> SomeStrategy

---------- Configured Strategies

-- | A strategy paired with its options. Produced by 'Discover' functions
data ConfiguredStrategy where
  ConfiguredStrategy :: (FromJSON options, ToJSON options) => Strategy options -> options -> ConfiguredStrategy

instance ToJSON ConfiguredStrategy where
  toJSON (ConfiguredStrategy strategy options) = object ["name" .= strategyName strategy, "options" .= options]

---------- Basic Opts

-- | A basic set of options, containing just a target directory
newtype BasicDirOpts = BasicDirOpts
  { targetDir :: Path Rel Dir
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON BasicDirOpts where
  parseJSON = withObject "BasicDirOpts" $ \obj ->
    BasicDirOpts <$> obj .: "dir"

instance ToJSON BasicDirOpts where
  toJSON BasicDirOpts{..} = object ["dir" .= targetDir]

-- | A basic set of options, containing just a target file
newtype BasicFileOpts = BasicFileOpts
  { targetFile :: Path Rel File
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON BasicFileOpts where
  parseJSON = withObject "BasicFileOpts" $ \obj ->
    BasicFileOpts <$> obj .: "file"

instance ToJSON BasicFileOpts where
  toJSON BasicFileOpts{..} = object ["file" .= targetFile]
