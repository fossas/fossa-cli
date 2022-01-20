module App.Fossa.VSIDeps (
  analyzeVSIDeps,
) where

import App.Fossa.Analyze.Project (ProjectResult (ProjectResult))
import App.Fossa.VSI.Analyze (runVsiAnalysis)
import App.Fossa.VSI.IAT.Resolve (resolveGraph, resolveUserDefined)
import App.Fossa.VSI.Types qualified as VSI
import App.Types (ProjectRevision)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, fromEither)
import Control.Effect.Finally (Finally)
import Control.Effect.Lift (Lift)
import Control.Effect.StickyLogger (StickyLogger)
import DepTypes (Dependency)
import Discovery.Filters (AllFilters)
import Effect.Logger (Logger)
import Effect.ReadFS (ReadFS)
import Fossa.API.Types (ApiOpts)
import Graphing (Graphing)
import Graphing qualified
import Path (Abs, Dir, Path)
import Srclib.Converter qualified as Srclib
import Srclib.Types (AdditionalDepData (..), SourceUnit (..), SourceUserDefDep)
import Types (DiscoveredProjectType (VsiProjectType), GraphBreadth (Complete))

-- | VSI analysis is sufficiently different from other analysis types that it cannot be just another strategy.
-- Instead, VSI analysis is run separately over the entire scan directory, outputting its own source unit.
analyzeVSIDeps ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has StickyLogger sig m
  , Has ReadFS sig m
  , Has Finally sig m
  ) =>
  Path Abs Dir ->
  ProjectRevision ->
  ApiOpts ->
  AllFilters ->
  VSI.SkipResolution ->
  m SourceUnit
analyzeVSIDeps dir projectRevision apiOpts filters skipResolving = do
  (direct, userDeps) <- runVsiAnalysis dir apiOpts projectRevision filters

  resolvedUserDeps <- resolveUserDefined apiOpts userDeps
  resolvedGraph <- resolveGraph apiOpts direct skipResolving
  dependencies <- fromEither $ Graphing.gtraverse VSI.toDependency resolvedGraph

  pure $ toSourceUnit (toProject dir dependencies) resolvedUserDeps

toProject :: Path Abs Dir -> Graphing Dependency -> ProjectResult
toProject dir graph = ProjectResult VsiProjectType dir graph Complete []

toSourceUnit :: ProjectResult -> Maybe [SourceUserDefDep] -> SourceUnit
toSourceUnit project deps = do
  let unit = Srclib.toSourceUnit False project
  unit{additionalData = fmap toDepData deps}
  where
    toDepData d = AdditionalDepData (Just d) Nothing
