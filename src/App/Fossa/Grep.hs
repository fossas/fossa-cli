module App.Fossa.Grep (
  analyzeWithGrep,
) where
import Data.List.NonEmpty (NonEmpty)
import Control.Carrier.Diagnostics (Diagnostics)
import Control.Effect.Lift (Has, Lift)
import Control.Effect.StickyLogger (StickyLogger)
import Control.Effect.Debug (Debug)

import Fossa.API.Types (ApiOpts)
import Path (Abs, Dir, Path)
import Effect.Exec (Exec)
import Effect.Logger (Logger)
import Srclib.Types (SourceUnit (..))
import App.Fossa.Config.Analyze (GrepEntry, GrepOptions)

analyzeWithGrep ::
  ( Has Diagnostics sig m
  , Has StickyLogger sig m
  , Has Logger sig m
  , Has Debug sig m
  , Has Exec sig m
  ) =>
  Path Abs Dir ->
  Maybe ApiOpts ->
  GrepOptions ->
  m (Maybe SourceUnit) -- TODO: make the types that we actually want to return
analyzeWithGrep root maybeApiOpts grepOptions = do
  pure Nothing
