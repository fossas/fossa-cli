{-# language TemplateHaskell #-}

module Diagnostics
  ( CLIErr(..)
  ) where

import Prologue

-- | Errors that can be produced by 'Discover' and 'Strategy'

data CLIErr =
    -- Configuration errors
    ConfigParseFailed String -- ^ Configuration parsing failed
  | UnknownStrategyName String -- ^ An unknown strategy name was found in the configuration file
  | StrategyOptionsParseFailed String String -- ^ name of strategy, err.

    -- Strategy execution errors
    -- TODO: new-cli-error-style reporting
  | StrategyFailed String
  | CommandFailed String String -- command, stderr
  | FileReadError String
  deriving (Eq, Ord, Show, Generic, Typeable)
