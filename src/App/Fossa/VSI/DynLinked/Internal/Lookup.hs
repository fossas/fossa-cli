module App.Fossa.VSI.DynLinked.Internal.Lookup (
  dynamicDependencies,
) where

import App.Fossa.VSI.DynLinked.Internal.Lookup.APK (APKLookupTable, apkTactic, buildLookupTable)
import App.Fossa.VSI.DynLinked.Internal.Lookup.DEB (debTactic)
import App.Fossa.VSI.DynLinked.Internal.Lookup.RPM (rpmTactic)
import App.Fossa.VSI.DynLinked.Types (DynamicDependency (..))
import Control.Algebra (Has)
import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, (<||>))
import Data.Set (Set)
import Data.Set qualified as Set
import Effect.Exec (Exec)
import Path (Abs, Dir, File, Path)

-- | Resolve the provided file paths, which represent dynamic dependencies of a binary, into a set of @DynamicDependency@.
dynamicDependencies ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  ) =>
  -- The scan root.
  Path Abs Dir ->
  -- The files to resolve.
  Set (Path Abs File) ->
  -- Resulting dependencies.
  m (Set DynamicDependency)
dynamicDependencies root files = do
  apkLookupTable <- buildLookupTable root
  resolved <- traverse (resolveFile apkLookupTable root) $ Set.toList files
  pure $ Set.fromList resolved

resolveFile :: (Has Diagnostics sig m, Has Exec sig m) => Maybe APKLookupTable -> Path Abs Dir -> Path Abs File -> m DynamicDependency
resolveFile table root file = do
  resolved <- rpmTactic root file <||> debTactic root file
  case resolved <|> apkTactic table file of
    Nothing -> pure $ fallbackTactic file
    Just result -> pure result

fallbackTactic :: Path Abs File -> DynamicDependency
fallbackTactic file = DynamicDependency file Nothing
