{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VSIDeps (
  analyzeVSIDeps,
  userEnabledMsb,
  userEnabledMsbErrorMsg,
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
import Control.Carrier.Diagnostics (recover)
import Control.Effect.Diagnostics (Diagnostics, fromEither)
import Control.Effect.Finally (Finally)
import Control.Effect.FossaApiClient (FossaApiClient)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.StickyLogger (StickyLogger)
import Data.Bifunctor (first)
import Data.List (find, partition)
import Data.Maybe (catMaybes, fromMaybe)
import Data.String.Conversion (toString, toText)
import Data.Text (Text)
import DepTypes (Dependency)
import Discovery.Filters (AllFilters)
import Effect.Logger (Logger, logDebug, pretty, renderIt)
import Effect.ReadFS (ReadFS)
import Graphing (Graphing)
import Graphing qualified
import Path (Abs, Dir, Path, toFilePath)
import Prettyprinter (vsep)
import Srclib.Converter qualified as Srclib
import Srclib.Types (AdditionalDepData (..), SourceUnit (..), SourceUserDefDep, textToOriginPath)
import System.Environment (getEnvironment)
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

  -- MSB is still active server side, so MSB results can still be returned.
  let (userDeps, directRules) =
        first (map (IAT.toUserDep . vsiRuleLocator))
          . partition (VSI.isUserDefined . vsiRuleLocator)
          $ rules

  -- But if MSB is disabled client side, just drop the user-defined results.
  msbEnabled <- userEnabledMsb
  resolvedUserDeps <- resolveUserDefined userDeps
  directSrcUnits <- catMaybes <$> traverse (ruleToSourceUnit msbEnabled skipResolving) directRules

  -- These deps have to get up to the backend somehow on a 'SourceUnit's 'additionalData'.
  -- This generates an empty-graph source unit and puts the userdeps on it.
  let renderedPath = toText (toFilePath dir)
      userDepSrcUnits = toSourceUnit renderedPath mempty resolvedUserDeps

  pure . Just $ userDepSrcUnits : directSrcUnits

ruleToSourceUnit :: (Has Diagnostics sig m, Has FossaApiClient sig m, Has Logger sig m) => Bool -> VSI.SkipResolution -> VSI.VsiRule -> m (Maybe SourceUnit)
ruleToSourceUnit msbEnabled skipResolving VSI.VsiRule{..} = do
  if not msbEnabled && VSI.isTopLevelProject vsiRuleLocator
    then do
      logDebug . pretty $ "MSB disabled, skipping " <> VSI.renderLocator vsiRuleLocator
      pure Nothing
    else do
      logDebug . pretty $ "MSB enabled, resolving " <> VSI.renderLocator vsiRuleLocator
      resolvedGraph <- recover $ resolveGraph [vsiRuleLocator] skipResolving
      dependencies <- fromEither $ Graphing.gtraverse VSI.toDependency (fromMaybe mempty resolvedGraph)
      pure . Just $ toSourceUnit (VSI.unVsiRulePath vsiRulePath) dependencies Nothing

toSourceUnit :: Text -> Graphing Dependency -> Maybe [SourceUserDefDep] -> SourceUnit
toSourceUnit path deps additionalDeps = do
  let unit = Srclib.toSourceUnit False path deps VsiProjectType Complete [textToOriginPath path]
  unit{additionalData = fmap toDepData additionalDeps}
  where
    toDepData d = AdditionalDepData (Just d) Nothing

-- | Whether MSB is enabled.
-- This is a temporary solution to allow users to opt-in to MSB during its deprecation phase.
-- Once MSB is fully removed, this function will be removed.
userEnabledMsb :: (Has (Lift IO) sig m) => m Bool
userEnabledMsb = do
  -- Uses @getEnvironment@ to get all vars, then searches for the one we care about,
  -- instead of @getEnv@ to get the one we care about because the latter can throw an exception.
  vars <- sendIO getEnvironment
  let userEnableMsbVar' = toString userEnableMsbVar
  case find (\(k, _) -> k == userEnableMsbVar') vars of
    Just (_, "true") -> pure True
    _ -> pure False

-- | The environment variable that controls whether MSB is enabled.
-- This is a temporary solution to allow users to opt-in to MSB during its deprecation phase.
-- Once MSB is fully removed, this variable will be removed.
userEnableMsbVar :: Text
userEnableMsbVar = "FOSSA_MSB_ENABLED"

-- | Error message to use when the user has enabled MSB functionality.
userEnabledMsbErrorMsg :: Text
userEnabledMsbErrorMsg =
  renderIt . vsep $
    [ "MSB functionality is deprecated and will be removed in a future release."
    , ""
    , "For now you can set the '" <> pretty userEnableMsbVar <> "' environment variable to 'true' to re-enable support."
    , ""
    , "Warning: This workaround will be removed in a future release."
    , "If you rely on this behavior please contact FOSSA support to let us know."
    ]
