-- | An effect for "logging" to a sticky region in the console
module Control.Effect.StickyLogger (
  StickyLogger,
  StickyLoggerF (..),
  logSticky,
  logSticky',
  module X,
) where

import Control.Algebra as X
import Control.Carrier.Simple
import Data.Text (Text)
import Prettyprinter (Doc, pretty)
import Prettyprinter.Render.Terminal (AnsiStyle)

data StickyLoggerF a where
  LogSticky' :: Doc AnsiStyle -> StickyLoggerF ()

type StickyLogger = Simple StickyLoggerF

-- | Set the contents of the sticky region to the provided message
logSticky :: Has StickyLogger sig m => Text -> m ()
logSticky = logSticky' . pretty

logSticky' :: Has StickyLogger sig m => Doc AnsiStyle -> m ()
logSticky' = sendSimple . LogSticky'
