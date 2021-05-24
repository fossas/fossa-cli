module Console.Sticky
  ( withStickyRegion,
    StickyRegion, -- constructors intentionally not exported
    setSticky,
    setSticky',
  )
where

import Control.Effect.ConsoleRegion qualified as R
import Control.Effect.Lift
import Data.Text (Text)
import Effect.Logger
import System.Console.ANSI (hSupportsANSI)
import System.IO (stdout)

withStickyRegion :: Has (Lift IO) sig m => (StickyRegion -> m a) -> m a
withStickyRegion f = do
  ansiSupported <- sendIO $ hSupportsANSI stdout
  if ansiSupported
    then R.withConsoleRegion R.Linear (f . Sticky)
    else f NonSticky

data StickyRegion
  = -- | Show messages via Logger
    NonSticky
  | -- | Show messages in console region
    Sticky R.ConsoleRegion

setSticky :: (Has (Lift IO) sig m, Has Logger sig m) => StickyRegion -> Text -> m ()
setSticky region = setSticky' region . pretty

setSticky' :: (Has (Lift IO) sig m, Has Logger sig m) => StickyRegion -> Doc AnsiStyle -> m ()
setSticky' NonSticky msg = logDebug msg
setSticky' (Sticky region) msg = logDebug msg *> renderToConsoleRegion region msg

renderToConsoleRegion :: Has (Lift IO) sig m => R.ConsoleRegion -> Doc AnsiStyle -> m ()
renderToConsoleRegion region = sendIO . R.setConsoleRegion region . renderStrict . layoutPretty defaultLayoutOptions
