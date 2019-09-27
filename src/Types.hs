module Types
  ( CLIErr(..)
  ) where

import Prologue

data CLIErr =
    -- Configuration errors
    ConfigParseFailed String
  | UnknownStrategyName String
  | StrategyOptionsParseFailed String String -- name of strategy, err

    -- Strategy execution errors
    -- TODO: new-cli-error-style reporting
  | StrategyFailed String -- name of strategy, err
  deriving (Eq, Ord, Show, Generic, Typeable)
