module App.Fossa.Analyze.Types (
  AnalyzeProject (..),
  AnalyzeTaskEffs,
) where

import Control.Carrier.Diagnostics
import Control.Effect.Debug (Debug)
import Control.Effect.Lift
import Control.Monad.IO.Class (MonadIO)
import Effect.Exec (Exec)
import Effect.Logger (Logger)
import Effect.ReadFS (ReadFS)
import Types

type AnalyzeTaskEffs sig m =
  ( Has (Lift IO) sig m
  , MonadIO m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has Logger sig m
  , Has Diagnostics sig m
  , Has Debug sig m
  )

class AnalyzeProject a where
  analyzeProject :: AnalyzeTaskEffs sig m => FoundTargets -> a -> m DependencyResults
