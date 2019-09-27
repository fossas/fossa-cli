
{-# language TemplateHaskell #-}

module Effect.ErrorTrace
  ( CLIErr(..)
  , ErrorTrace(..)
  , traceErr
  , errorTraceToOutput
  )
  where

import Prologue

import Polysemy
import Polysemy.Output

import Types

data ErrorTrace m a where
  -- trace an error but continue execution
  TraceErr     :: CLIErr -> ErrorTrace m ()

makeSem ''ErrorTrace

errorTraceToOutput :: Members '[Output CLIErr] r => InterpreterFor ErrorTrace r
errorTraceToOutput = interpret $ \case
  TraceErr e -> output e
{-# INLINE errorTraceToOutput #-}
