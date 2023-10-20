{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VSIDeps (
  analyzeVSIDeps,
  -- exported for testing
  ruleToSourceUnit,
) where

import App.Fossa.VSI.Analyze (runVsiAnalysis)
import App.Fossa.VSI.IAT.Resolve (resolveGraph, resolveUserDefined)
import App.Fossa.VSI.IAT.Types qualified as IAT
import App.Fossa.VSI.Types (VsiRule (..))
import App.Fossa.VSI.Types qualified as VSI
import App.Types (ProjectRevision)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, fromEither)
import Control.Effect.Finally (Finally)
import Control.Effect.FossaApiClient (FossaApiClient)
import Control.Effect.Lift (Lift)
import Control.Effect.StickyLogger (StickyLogger)
import Data.Bifunctor (first)
import Data.List (partition)
import Data.String.Conversion (toText)
import Data.Text (Text)
import DepTypes (Dependency)
import Discovery.Filters (AllFilters)
import Effect.Logger (Logger)
import Effect.ReadFS (ReadFS)
import Graphing (Graphing)
import Graphing qualified
import Path (Abs, Dir, Path, toFilePath)
import Srclib.Converter qualified as Srclib
import Srclib.Types (AdditionalDepData (..), SourceUnit (..), SourceUserDefDep, textToOriginPath)
import Types (DiscoveredProjectType (..), GraphBreadth (Complete))

-- | VSI analysis is sufficiently different from other analysis types that it cannot be just another strategy.
-- Instead, VSI analysis is run separately over the entire scan directory, outputting its own source unit.
analyzeVSIDeps ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has StickyLogger sig m
  , Has ReadFS sig m
  , Has Finally sig m
  , Has FossaApiClient sig m
  ) =>
  Path Abs Dir ->
  ProjectRevision ->
  AllFilters ->
  VSI.SkipResolution ->
  m (Maybe [SourceUnit])
analyzeVSIDeps dir projectRevision filters skipResolving = do
  rules <- runVsiAnalysis dir projectRevision filters

  let (userDeps, directRules) =
        first (map (IAT.toUserDep . vsiRuleLocator))
          . partition (VSI.isUserDefined . vsiRuleLocator)
          $ rules

  resolvedUserDeps <- resolveUserDefined userDeps
  directSrcUnits <- traverse (ruleToSourceUnit skipResolving) directRules

  -- These deps have to get up to the backend somehow on a 'SourceUnit's 'additionalData'.
  -- This generates an empty-graph source unit and puts the userdeps on it.
  let renderedPath = toText (toFilePath dir)
      userDepSrcUnits = toSourceUnit renderedPath mempty resolvedUserDeps

  pure . Just $ userDepSrcUnits : directSrcUnits

ruleToSourceUnit :: (Has Diagnostics sig m, Has FossaApiClient sig m) => VSI.SkipResolution -> VSI.VsiRule -> m (SourceUnit)
ruleToSourceUnit skipResolving VSI.VsiRule{..} = do
  resolvedGraph <- resolveGraph [vsiRuleLocator] skipResolving
  dependencies <- fromEither $ Graphing.gtraverse VSI.toDependency resolvedGraph
  pure $ toSourceUnit (VSI.unVsiRulePath vsiRulePath) dependencies Nothing

toSourceUnit :: Text -> Graphing Dependency -> Maybe [SourceUserDefDep] -> SourceUnit
toSourceUnit path deps additionalDeps = do
  let unit = Srclib.toSourceUnit False path deps VsiProjectType Complete [textToOriginPath path]
  unit{additionalData = fmap toDepData additionalDeps}
  where
    toDepData d = AdditionalDepData (Just d) Nothing
