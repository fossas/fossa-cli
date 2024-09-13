{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VSI.IAT.Resolve (
  resolveUserDefined,
  resolveRevision,
  resolveGraph,
) where

import App.Fossa.VSI.IAT.Types (
  UserDefinedAssertionMeta (..),
  UserDep,
 )
import App.Fossa.VSI.IAT.Types qualified as IAT
import App.Fossa.VSI.Types qualified as VSI
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, context, recover, warn)
import Control.Effect.FossaApiClient (FossaApiClient, resolveProjectDependencies, resolveUserDefinedBinary)
import Control.Monad (unless)
import Data.Either (partitionEithers)
import Data.String.Conversion (toText)
import Data.Text (Text, intercalate)
import Graphing (Graphing, direct, edges)
import Srclib.Types (
  SourceUserDefDep (..),
 )

resolveUserDefined :: (Has FossaApiClient sig m, Has Diagnostics sig m) => [UserDep] -> m (Maybe [SourceUserDefDep])
resolveUserDefined deps = context ("Resolving user defined dependencies " <> toText (show $ map IAT.renderUserDep deps)) $ do
  assertions <- traverse (resolveUserDefinedBinary) deps
  if null assertions
    then pure Nothing
    else pure . Just $ map toUserDefDep assertions

resolveRevision :: (Has FossaApiClient sig m, Has Diagnostics sig m) => VSI.Locator -> m (Maybe [VSI.Locator])
resolveRevision locator =
  context ("Resolving dependencies for " <> VSI.renderLocator locator) $
    -- In the future we plan to support either returning a partial graph to the server for resolution there,
    -- or expanding this into a full fledged graph resolver for any dependency using dedicated endpoints.
    -- For the initial version of IAT however we're just resolving dependencies for top level projects.
    -- Since users may only register revision assertions as a byproduct of running CLI analysis,
    -- revision assertions are always for "custom" projects.
    if VSI.isTopLevelProject locator
      then recover $ resolveProjectDependencies locator
      else pure . Just $ []

resolveGraph :: (Has FossaApiClient sig m, Has Diagnostics sig m) => [VSI.Locator] -> VSI.SkipResolution -> m (Graphing VSI.Locator)
resolveGraph locators skipResolving = context ("Resolving graph for " <> toText (show $ fmap VSI.renderLocator locators)) $ do
  -- If any of the resulting graphs are Nothing, we failed to fetch the graph from the server.
  -- This typically means that the user doesn't have access to the project, or the project doesn't exist.
  -- Collect failed locators and report them to the user, along with mitigation suggestions.
  subgraphs <- traverseZipM (resolveSubgraph skipResolving) locators
  let (warned, success) = partitionMap partitionSubgraph subgraphs
  renderWarnings warned
  pure $ mconcat success
  where
    renderWarnings subgraphs = unless (null subgraphs) . warn $ resolveGraphFailureBundle subgraphs
    partitionSubgraph (a, Nothing) = Left a
    partitionSubgraph (_, Just b) = Right b

resolveGraphFailureBundle :: [VSI.Locator] -> Text
resolveGraphFailureBundle subgraphs =
  "Failed to resolve dependencies for the following FOSSA projects:\n\t"
    <> intercalate "\n\t" (fmap VSI.renderLocator subgraphs)
    <> "\n\n"
    <> "You may not have access to the projects, or they may not exist.\n"

-- | Given a traverseable list and a monadic function that resolves them to b, traverse and zip the list into a pair of (a, b)
traverseZipM :: (Traversable t, Applicative m) => (a -> m b) -> t a -> m (t (a, b))
traverseZipM f = traverse (\a -> (a,) <$> f a)

-- | Split the list into two mapped lists based on the predicate.
partitionMap :: (a -> Either b c) -> [a] -> ([b], [c])
partitionMap split items = partitionEithers $ map split items

-- Pass through the list of skipped locators all the way here:
-- we want to still record the direct dependency, we just don't want to resolve it.
resolveSubgraph :: (Has FossaApiClient sig m, Has Diagnostics sig m) => VSI.SkipResolution -> VSI.Locator -> m (Maybe (Graphing VSI.Locator))
resolveSubgraph skip locator | VSI.shouldSkipResolving skip locator = pure . Just $ direct locator
resolveSubgraph _ locator = do
  resolved <- resolveRevision locator
  case resolved of
    Just deps -> pure . Just $ direct locator <> edges (map edge deps)
    Nothing -> pure Nothing
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
    , srcUserDepOrigin = Nothing
    }
