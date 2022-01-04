module App.Fossa.Analyze.Types (
  AnalyzeProject (..),
  AnalyzeTaskEffs,
  AnalyzeExperimentalPreferences (..),
) where

import App.Fossa.Config.Analyze (ExperimentalAnalyzeConfig)
import Control.Effect.Debug (Debug)
import Control.Effect.Diagnostics (Diagnostics, Has)
import Control.Effect.Lift (Lift)
import Control.Effect.Reader (Reader)
import Data.Set (Set)
import Data.Text (Text)
import Effect.Exec (Exec)
import Effect.Logger (Logger)
import Effect.ReadFS (ReadFS)
import Types (DependencyResults, FoundTargets)

newtype AnalyzeExperimentalPreferences = AnalyzeExperimentalPreferences
  {gradleOnlyConfigsAllowed :: Maybe (Set Text)}
  deriving (Show, Eq, Ord)

type AnalyzeTaskEffs sig m =
  ( Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has Logger sig m
  , Has Diagnostics sig m
  , Has Debug sig m
  , Has (Reader ExperimentalAnalyzeConfig) sig m
  )

class AnalyzeProject a where
  analyzeProject :: AnalyzeTaskEffs sig m => FoundTargets -> a -> m DependencyResults
