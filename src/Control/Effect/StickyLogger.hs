-- | An effect for "logging" to a sticky region in the console
module Control.Effect.StickyLogger (
  StickyLogger (..),
  logSticky,
  logSticky',
  module X,
) where

import Data.Text (Text)
import Control.Algebra as X
import Prettyprinter (Doc, pretty)
import Prettyprinter.Render.Terminal (AnsiStyle)

data StickyLogger m a where
  LogSticky' :: Doc AnsiStyle -> StickyLogger m ()

-- | Set the contents of the sticky region to the provided message
logSticky :: Has StickyLogger sig m => Text -> m ()
logSticky = logSticky' . pretty

logSticky' :: Has StickyLogger sig m => Doc AnsiStyle -> m ()
logSticky' = send . LogSticky'
