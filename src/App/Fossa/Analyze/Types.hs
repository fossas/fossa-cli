module App.Fossa.Analyze.Types (
  AnalyzeProject (..),
  AnalyzeTaskEffs,
  AnalyzeExperimentalPreferences (..),
  DiscoveredProjectScan (..),
  DiscoveredProjectIdentifier (..),
) where

import App.Fossa.Analyze.Project (ProjectResult)
import App.Fossa.Config.Analyze (ExperimentalAnalyzeConfig)
import Control.Effect.Debug (Debug)
import Control.Effect.Diagnostics (Diagnostics, Has)
import Control.Effect.Lift (Lift)
import Control.Effect.Reader (Reader)
import Data.Set (Set)
import Data.Text (Text)
import Diag.Result (Result (Failure, Success))
import Effect.Exec (Exec)
import Effect.Logger (Logger)
import Effect.ReadFS (ReadFS)
import Path
import Types (DependencyResults, DiscoveredProjectType, FoundTargets)

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

data DiscoveredProjectScan
  = SkippedDueToProvidedFilter DiscoveredProjectIdentifier
  | SkippedDueToDefaultProductionFilter DiscoveredProjectIdentifier
  | Scanned DiscoveredProjectIdentifier (Result ProjectResult)

instance Ord DiscoveredProjectScan where
  a `compare` b = orderByScanStatusAndType a b

instance Eq DiscoveredProjectScan where
  a == b = compare a b == EQ

orderByScanStatusAndType :: DiscoveredProjectScan -> DiscoveredProjectScan -> Ordering
orderByScanStatusAndType (SkippedDueToProvidedFilter lhs) (SkippedDueToProvidedFilter rhs) = compare lhs rhs
orderByScanStatusAndType (SkippedDueToProvidedFilter lhs) (SkippedDueToDefaultProductionFilter rhs) = compare lhs rhs
orderByScanStatusAndType (SkippedDueToDefaultProductionFilter lhs) (SkippedDueToProvidedFilter rhs) = compare lhs rhs
orderByScanStatusAndType (SkippedDueToDefaultProductionFilter lhs) (SkippedDueToDefaultProductionFilter rhs) = compare lhs rhs
orderByScanStatusAndType (SkippedDueToDefaultProductionFilter _) (Scanned _ _) = GT
orderByScanStatusAndType (SkippedDueToProvidedFilter _) (Scanned _ _) = GT
orderByScanStatusAndType (Scanned lhs (Success lhsEw _)) (Scanned rhs (Success rhsEw _)) =
  if length rhsEw == length lhsEw
    then compare lhs rhs
    else compare (length rhsEw) (length lhsEw)
orderByScanStatusAndType (Scanned lhs (Failure _ _)) (Scanned rhs (Failure _ _)) = compare lhs rhs
orderByScanStatusAndType (Scanned _ (Success _ _)) (Scanned _ (Failure _ _)) = GT
orderByScanStatusAndType (Scanned _ _) _ = LT

data DiscoveredProjectIdentifier = DiscoveredProjectIdentifier
  { dpiProjectPath :: Path Abs Dir
  , dpiProjectType :: DiscoveredProjectType
  }
  deriving (Eq, Ord)

class AnalyzeProject a where
  analyzeProject :: AnalyzeTaskEffs sig m => FoundTargets -> a -> m DependencyResults
