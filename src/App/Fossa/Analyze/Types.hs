module App.Fossa.Analyze.Types (
  AnalyzeProject (..),
  AnalysisScanResult (..),
  DiscoverTaskEffs,
  AnalyzeTaskEffs,
  AnalyzeStaticTaskEffs,
  AnalyzeExperimentalPreferences (..),
  DiscoveredProjectScan (..),
  DiscoveredProjectIdentifier (..),
) where

import App.Fossa.Analyze.Project (ProjectResult)
import App.Fossa.Config.Analyze (ExperimentalAnalyzeConfig)
import App.Fossa.Lernie.Types (LernieResults)
import Control.Effect.Debug (Debug)
import Control.Effect.Diagnostics (Diagnostics, Has)
import Control.Effect.Lift (Lift)
import Control.Effect.Reader (Reader)
import Control.Effect.Telemetry (Telemetry)
import Data.Set (Set)
import Data.Text (Text)
import Diag.Result (Result (Failure, Success))
import Discovery.Filters (AllFilters, MavenScopeFilters)
import Effect.Exec (CandidateCommandEffs, Exec)
import Effect.Logger (Logger)
import Effect.ReadFS (ReadFS)
import Path (Abs, Dir, Path)
import Srclib.Types (SourceUnit)
import Types (DependencyResults, DiscoveredProjectType, FoundTargets)

newtype AnalyzeExperimentalPreferences = AnalyzeExperimentalPreferences
  {gradleOnlyConfigsAllowed :: Maybe (Set Text)}
  deriving (Show, Eq, Ord)

-- | Effects needed to discover projects.
type DiscoverTaskEffs sig m =
  ( Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has Logger sig m
  , Has Diagnostics sig m
  , Has Debug sig m
  , Has (Reader ExperimentalAnalyzeConfig) sig m
  , Has (Reader MavenScopeFilters) sig m
  , Has (Reader AllFilters) sig m
  , Has Telemetry sig m
  )

-- | Effects needed to analyze projects dynamically.
type AnalyzeTaskEffs sig m =
  ( CandidateCommandEffs sig m
  , DiscoverTaskEffs sig m
  )

-- | Effects needed to analyze projects statically.
--
-- Currently a copy of @DiscoverTaskEffs@, but ideally wouldn't have @Exec@:
-- @Exec@ is only there because we need to execute a binary to statically analyze BerkeleyDB,
-- so ideally we'd either create a different effect to disambiguate this case or make the BDB analysis not a separate binary
-- (either via FFI, WASM embedding, or rewriting it to Haskell).
type AnalyzeStaticTaskEffs sig m =
  ( Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has Logger sig m
  , Has Diagnostics sig m
  , Has Debug sig m
  , Has (Reader ExperimentalAnalyzeConfig) sig m
  , Has (Reader MavenScopeFilters) sig m
  , Has (Reader AllFilters) sig m
  , Has Telemetry sig m
  )

data AnalysisScanResult = AnalysisScanResult
  { analyzersScanResult :: [DiscoveredProjectScan]
  , vsiScanResult :: Result (Maybe [SourceUnit])
  , binaryDepsScanResult :: Result (Maybe SourceUnit)
  , fossaDepsScanResult :: Result (Maybe SourceUnit)
  , dynamicLinkingResult :: Result (Maybe SourceUnit)
  , lernieResult :: Result (Maybe LernieResults)
  }

data DiscoveredProjectScan
  = SkippedDueToProvidedFilter DiscoveredProjectIdentifier
  | SkippedDueToDefaultProductionFilter DiscoveredProjectIdentifier
  | Scanned DiscoveredProjectIdentifier (Result ProjectResult)
  deriving (Show)

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
  case compare (length rhsEw) (length lhsEw) of
    EQ -> compare lhs rhs
    comp -> comp
orderByScanStatusAndType (Scanned lhs (Failure _ _)) (Scanned rhs (Failure _ _)) = compare lhs rhs
orderByScanStatusAndType (Scanned _ (Success _ _)) (Scanned _ (Failure _ _)) = GT
orderByScanStatusAndType (Scanned _ _) _ = LT

data DiscoveredProjectIdentifier = DiscoveredProjectIdentifier
  { dpiProjectPath :: Path Abs Dir
  , dpiProjectType :: DiscoveredProjectType
  }
  deriving (Eq, Ord, Show)

class AnalyzeProject a where
  -- | Analyze a project with any tactic.
  analyzeProject :: AnalyzeTaskEffs sig m => FoundTargets -> a -> m DependencyResults

  -- | Analyze a project with only static tactics.
  analyzeProject' :: AnalyzeStaticTaskEffs sig m => FoundTargets -> a -> m DependencyResults
