
module Strategy
  ( Strategy(..)
  , SomeStrategy(..)
  , StrategyMems
  ) where

import Prologue

import Polysemy
import Polysemy.Error

import Effect.ErrorTrace
import Effect.Exec
import Graph

type StrategyMems r = Members '[Embed IO, Exec, Error CLIErr] r

data Strategy options = Strategy
  { strategyName    :: String
  , strategyAnalyze :: forall r. StrategyMems r => options -> Sem r Graph
  }

data SomeStrategy where
  SomeStrategy :: (FromJSON options, ToJSON options) => Strategy options -> SomeStrategy
