{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VSI.IAT.Resolve (
  resolveUserDefined,
  resolveRevision,
  resolveGraph,
) where

import App.Fossa.FossaAPIV1 qualified as Fossa
import App.Fossa.VSI.IAT.Types (
  UserDefinedAssertionMeta (..),
  UserDep,
 )
import App.Fossa.VSI.IAT.Types qualified as IAT
import App.Fossa.VSI.Types qualified as VSI
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, context)
import Control.Effect.Lift (Lift)
import Data.String.Conversion (toText)
import Fossa.API.Types (ApiOpts)
import Graphing (Graphing, direct, edges)
import Srclib.Types (
  SourceUserDefDep (..),
 )

resolveUserDefined :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> [UserDep] -> m (Maybe [SourceUserDefDep])
resolveUserDefined apiOpts deps = context ("Resolving user defined dependencies " <> toText (show $ map IAT.renderUserDep deps)) $ do
  assertions <- traverse (Fossa.resolveUserDefinedBinary apiOpts) deps
  if null assertions
    then pure Nothing
    else pure . Just $ map toUserDefDep assertions

resolveRevision :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> VSI.Locator -> m [VSI.Locator]
resolveRevision apiOpts locator =
  context ("Resolving dependencies for " <> VSI.renderLocator locator) $
    -- In the future we plan to support either returning a partial graph to the server for resolution there,
    -- or expanding this into a full fledged graph resolver for any dependency using dedicated endpoints.
    -- For the initial version of IAT however we're just resolving dependencies for top level projects.
    -- Since users may only register revision assertions as a byproduct of running CLI analysis,
    -- revision assertions are always for "custom" projects.
    if VSI.isTopLevelProject locator
      then Fossa.resolveProjectDependencies apiOpts locator
      else pure []

resolveGraph :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> [VSI.Locator] -> m (Graphing VSI.Locator)
resolveGraph apiOpts locators = context ("Resolving graph for " <> toText (show $ fmap VSI.renderLocator locators)) $ do
  subgraphs <- traverse (resolveSubgraph apiOpts) locators
  pure $ mconcat subgraphs

resolveSubgraph :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> VSI.Locator -> m (Graphing VSI.Locator)
resolveSubgraph apiOpts locator = do
  resolved <- resolveRevision apiOpts locator
  pure $ direct locator <> edges (map edge resolved)
  where
    edge dep = (locator, dep)

toUserDefDep :: UserDefinedAssertionMeta -> SourceUserDefDep
toUserDefDep UserDefinedAssertionMeta{..} =
  SourceUserDefDep
    { srcUserDepName = assertedName
    , srcUserDepVersion = assertedVersion
    , srcUserDepLicense = assertedLicense
    , srcUserDepDescription = assertedDescription
    , srcUserDepHomepage = assertedUrl
    }
