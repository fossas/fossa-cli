module Diagnostics
  ( CLIErr(..)
  , ExecErr(..)
  , ReadFSErr(..)

  , execErrToCLIErr
  , readFSErrToCLIErr
  ) where

import Prologue

import Polysemy
import Polysemy.Error

-- | Errors that can be produced by 'Discover' and 'Strategy'

data CLIErr =
    -- Configuration errors
    ConfigParseFailed Text -- ^ Configuration parsing failed
  | UnknownStrategyName Text -- ^ An unknown strategy name was found in the configuration file
  | StrategyOptionsParseFailed Text Text -- ^ name of strategy, err.

  | CLIErrExec ExecErr -- ^ Command execution error
  | CLIErrReadFS ReadFSErr -- ^ Filesystem error
  deriving (Eq, Ord, Show, Generic, Typeable)

data ExecErr =
    CommandFailed Text Text -- ^ Command execution failed, usually from a non-zero exit. command, stderr
  | CommandParseError Text Text -- ^ Command output couldn't be parsed. TODO: ask user to help with this. command, err
  deriving (Eq, Ord, Show, Generic, Typeable)

data ReadFSErr =
    FileReadError FilePath Text -- ^ A file couldn't be read. file, err
  | FileParseError FilePath Text -- ^ A file's contents couldn't be parsed. TODO: ask user to help with this. file, err
  deriving (Eq, Ord, Show, Generic, Typeable)

execErrToCLIErr :: Member (Error CLIErr) r => Sem (Error ExecErr ': r) a -> Sem r a
execErrToCLIErr = mapError CLIErrExec

readFSErrToCLIErr :: Member (Error CLIErr) r => Sem (Error ReadFSErr ': r) a -> Sem r a
readFSErrToCLIErr = mapError CLIErrReadFS
