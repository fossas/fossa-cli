
module Strategy
  ( Strategy(..)
  , SomeStrategy(..)
  ) where

import Prologue

import Graph

data Strategy options = Strategy
  { strategyName    :: String
  , strategyAnalyze :: options -> IO (Either String Graph)
  }

data SomeStrategy where
  SomeStrategy :: (FromJSON options, ToJSON options) => Strategy options -> SomeStrategy
