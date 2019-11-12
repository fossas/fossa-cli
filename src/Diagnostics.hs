module Diagnostics
  ( CLIErr(..)
  ) where

import Prologue

import Control.Exception (SomeException)

-- | Errors that can be produced by 'Discover' and 'Strategy'

data CLIErr =
    -- Configuration errors
    ConfigParseFailed Text -- ^ Configuration parsing failed
  | UnknownStrategyName Text -- ^ An unknown strategy name was found in the configuration file
  | StrategyOptionsParseFailed Text Text -- ^ name of strategy, err.

    -- Strategy execution errors
    -- TODO: new-cli-error-style reporting
  | CommandFailed Text Text -- ^ Command execution failed, usually from a non-zero exit. command, stderr
  | CommandParseError Text Text -- ^ Command output couldn't be parsed. TODO: ask user to help with this. command, err
  | FileReadError FilePath Text -- ^ A file couldn't be read. file, err
  | FileParseError FilePath Text -- ^ A file's contents couldn't be parsed. TODO: ask user to help with this. file, err

  | UncaughtException SomeException
  deriving (Show, Generic, Typeable)
