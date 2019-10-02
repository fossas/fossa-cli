module Discovery.Types
  ( DiscoverEffs
  ) where

import Prologue

import Polysemy
import Polysemy.Error
import Polysemy.Output

import Config
import Effect.ReadFS
import Types

type DiscoverEffs r = Members '[Embed IO, Error CLIErr, Output ConfiguredStrategy, ReadFS] r
