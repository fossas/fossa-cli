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
  | CommandFailed String String -- ^ Command execution failed, usually from a non-zero exit. command, stderr
  | CommandParseError String String -- ^ Command output couldn't be parsed. TODO: ask user to help with this. command, err
  | FileReadError FilePath String -- ^ A file couldn't be read. file, err
  | FileParseError FilePath String -- ^ A file's contents couldn't be parsed. TODO: ask user to help with this. file, err
  deriving (Eq, Ord, Show, Generic, Typeable)
