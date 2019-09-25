module Discovery
  ( DiscoverErr(..)
  , filterFilename
  , filterDirname
  ) where

import Prologue

data DiscoverErr =
    ConfigParseFailed String
  | UnknownStrategyName String
  | StrategyOptionsParseFailed String String -- name of strategy, err
  deriving (Eq, Ord, Show, Generic, Typeable)

filterFilename :: String -> [Path b File] -> [Path b File]
filterFilename name = filter ((== name) . toFilePath . filename)

filterDirname :: String -> [Path b Dir] -> [Path b Dir]
filterDirname name = filter ((== name) . toFilePath . dirname)
