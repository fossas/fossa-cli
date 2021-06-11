module Console.Sticky (
  withStickyRegion,
  StickyRegion, -- constructors intentionally not exported
  setSticky,
  setSticky',
) where

import Control.Effect.ConsoleRegion qualified as R
import Control.Effect.Lift
import Data.Text (Text)
import Effect.Logger
import System.Console.ANSI (hSupportsANSI)
import System.IO (stdout)

withStickyRegion :: Has (Lift IO) sig m => Severity -> (StickyRegion -> m a) -> m a
withStickyRegion sev f = do
  ansiSupported <- sendIO $ hSupportsANSI stdout
  if ansiSupported
    then R.withConsoleRegion R.Linear (f . Sticky)
    else f (NonSticky sev)

data StickyRegion
  = -- | Show messages via Logger
    NonSticky Severity
  | -- | Show messages in console region
    Sticky R.ConsoleRegion

setSticky :: (Has (Lift IO) sig m, Has Logger sig m) => StickyRegion -> Text -> m ()
setSticky region = setSticky' region . pretty

setSticky' :: (Has (Lift IO) sig m, Has Logger sig m) => StickyRegion -> Doc AnsiStyle -> m ()
setSticky' (NonSticky sev) msg = Effect.Logger.log sev msg
setSticky' (Sticky region) msg = logDebug msg *> renderToConsoleRegion region msg

renderToConsoleRegion :: Has (Lift IO) sig m => R.ConsoleRegion -> Doc AnsiStyle -> m ()
renderToConsoleRegion region = sendIO . R.setConsoleRegion region . renderStrict . layoutPretty defaultLayoutOptions
