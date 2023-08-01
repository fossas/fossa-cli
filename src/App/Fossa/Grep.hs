module App.Fossa.Grep (
  analyzeWithGrep,
) where

import Control.Carrier.Diagnostics (Diagnostics)
import Control.Effect.Debug (Debug)
import Control.Effect.Lift (Has, Lift)
import Control.Effect.StickyLogger (StickyLogger)
import Data.List.NonEmpty (NonEmpty)

import App.Fossa.Config.Analyze (GrepEntry, GrepOptions)
import Effect.Exec (Exec)
import Effect.Logger (Logger)
import Fossa.API.Types (ApiOpts)
import Path (Abs, Dir, Path)
import Srclib.Types (SourceUnit (..))

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
