module App.Fossa.Analyze.Types (
  AnalyzeProject (..),
  AnalyzeTaskEffs,
  AnalyzeExperimentalPreferences (..),
) where

import Control.Carrier.Diagnostics
import Control.Effect.Debug (Debug)
import Control.Effect.Lift
import Control.Effect.Reader (Reader)
import Control.Monad.IO.Class (MonadIO)
import Data.Set (Set)
import Data.Text (Text)
import Effect.Exec (Exec)
import Effect.Logger (Logger)
import Effect.ReadFS (ReadFS)
import Types
import Control.Effect.DiagWarn

newtype AnalyzeExperimentalPreferences = AnalyzeExperimentalPreferences
  {gradleOnlyConfigsAllowed :: Maybe (Set Text)}
  deriving (Show, Eq, Ord)

type AnalyzeTaskEffs sig m =
  ( Has (Lift IO) sig m
  , MonadIO m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has Logger sig m
  , Has Diagnostics sig m
  , Has DiagWarn sig m
  , Has Debug sig m
  , Has (Reader AnalyzeExperimentalPreferences) sig m
  )

class AnalyzeProject a where
  analyzeProject :: AnalyzeTaskEffs sig m => FoundTargets -> a -> m DependencyResults
