module App.Fossa.VSIDeps (
  analyzeVSIDeps,
) where

import App.Fossa.Analyze.Project (ProjectResult (ProjectResult))
import App.Fossa.EmbeddedBinary (withWigginsBinary)
import App.Fossa.VPS.Scan.RunWiggins (WigginsOpts, execWigginsJson, generateVSIStandaloneOpts, toPathFilters)
import App.Fossa.VSI.IAT.Resolve (resolveGraph, resolveUserDefined)
import App.Fossa.VSI.IAT.Types qualified as IAT
import App.Fossa.VSI.Types qualified as VSI
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, context, fromEither)
import Control.Effect.Lift (Lift)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import DepTypes (Dependency)
import Discovery.Filters (AllFilters)
import Effect.Exec (Exec)
import Fossa.API.Types (ApiOpts)
import Graphing (Graphing)
import Graphing qualified
import Path (Abs, Dir, Path)
import Srclib.Converter qualified as Srclib
import Srclib.Types (AdditionalDepData (..), SourceUnit (..), SourceUserDefDep)
import Types (GraphBreadth (Complete))

-- | VSI analysis is sufficiently different from other analysis types that it cannot be just another strategy.
-- Instead, VSI analysis is run separately over the entire scan directory, outputting its own source unit.
analyzeVSIDeps :: (MonadIO m, Has Diagnostics sig m, Has Exec sig m, Has (Lift IO) sig m) => Path Abs Dir -> ApiOpts -> AllFilters -> m SourceUnit
analyzeVSIDeps dir apiOpts filters = do
  (direct, userDeps) <- pluginAnalyze $ generateVSIStandaloneOpts dir (toPathFilters filters) apiOpts

  resolvedUserDeps <- resolveUserDefined apiOpts userDeps
  resolvedGraph <- resolveGraph apiOpts direct
  dependencies <- fromEither $ Graphing.gtraverse VSI.toDependency resolvedGraph

  pure $ toSourceUnit (toProject dir dependencies) resolvedUserDeps

-- | The VSI plugin results in a shallow graph of direct discovered dependencies and a list of discovered user defined dependencies.
pluginAnalyze :: (MonadIO m, Has (Lift IO) sig m, Has Exec sig m, Has Diagnostics sig m) => WigginsOpts -> m ([VSI.Locator], [IAT.UserDep])
pluginAnalyze opts = context "VSI" $ do
  (discoveredRawLocators :: [Text]) <- context "Running VSI binary" $ withWigginsBinary (execWigginsJson opts)
  parsedLocators <- fromEither $ traverse VSI.parseLocator discoveredRawLocators

  let userDefinedDeps = map IAT.toUserDep $ filter VSI.isUserDefined parsedLocators
  let allOtherDeps = filter (not . VSI.isUserDefined) parsedLocators
  pure (allOtherDeps, userDefinedDeps)

toProject :: Path Abs Dir -> Graphing Dependency -> ProjectResult
toProject dir graph = ProjectResult "vsi" dir graph Complete []

toSourceUnit :: ProjectResult -> Maybe [SourceUserDefDep] -> SourceUnit
toSourceUnit project deps = do
  let unit = Srclib.toSourceUnit False project
  unit{additionalData = fmap toDepData deps}
  where
    toDepData d = AdditionalDepData (Just d) Nothing
