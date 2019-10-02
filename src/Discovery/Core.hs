module Discovery.Core
  ( DiscoverEffs
  , ConfiguredStrategy(..)

  , BasicDirOpts(..)
  , BasicFileOpts(..)
  ) where

import Prologue

import Polysemy
import Polysemy.Error
import Polysemy.Output

import Effect.ReadFS
import Strategy
import Types

type DiscoverEffs r = Members '[Embed IO, Error CLIErr, Output ConfiguredStrategy, ReadFS] r

data ConfiguredStrategy where
  ConfiguredStrategy :: (FromJSON options, ToJSON options) => Strategy options -> options -> ConfiguredStrategy

instance ToJSON ConfiguredStrategy where
  toJSON (ConfiguredStrategy strategy options) = object ["name" .= strategyName strategy, "options" .= options]

data BasicDirOpts = BasicDirOpts
  { targetDir :: Path Rel Dir
  } deriving (Eq, Ord, Show, Generic)

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

