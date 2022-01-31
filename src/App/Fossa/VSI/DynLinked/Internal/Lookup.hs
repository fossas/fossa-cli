module App.Fossa.VSI.DynLinked.Internal.Lookup (
  dynamicDependencies,
) where

import App.Fossa.VSI.DynLinked.Internal.Lookup.APK qualified as APK
import App.Fossa.VSI.DynLinked.Internal.Lookup.DEB qualified as DEB
import App.Fossa.VSI.DynLinked.Internal.Lookup.RPM qualified as RPM
import App.Fossa.VSI.DynLinked.Types (DynamicDependency (..))
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
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
dynamicDependencies root files = Set.fromList <$> resolveFiles root strategies (Set.toList files) []

-- | Resolve all the files into dynamic dependencies by recursively applying strategies until all files are resolved.
resolveFiles ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  ) =>
  -- The scan root.
  Path Abs Dir ->
  -- The list of strategies which resolve files.
  [(Path Abs Dir -> [Path Abs File] -> m ([Path Abs File], [DynamicDependency]))] ->
  -- The files to resolve.
  [Path Abs File] ->
  -- The working set of resolved dependencies by previous strategies.
  [DynamicDependency] ->
  -- Resulting dependencies.
  m [DynamicDependency]
-- We're out of files to resolve.
-- They were reversed during the function for performance, so put them back in the correct order.
resolveFiles _ _ [] resolved = pure $ reverse resolved
-- We're out of resolvers, but still have files since the above pattern didn't match.
-- Resolve remaining files with @strategyRawFingerprint@.
resolveFiles root [] files resolved = resolveFiles root [] [] $ fmap strategyRawFingerprint files ++ resolved
-- We have a strategy! Resolve as many files as we can with it and move on.
resolveFiles root (resolve : remainingStrategies) files resolved = do
  (remainingFiles, newlyResolved) <- resolve root files
  resolveFiles root remainingStrategies remainingFiles $ newlyResolved ++ resolved

-- | Functions which may be able to resolve a binary to a dependency.
strategies :: (Has Diagnostics sig m, Has Exec sig m) => [(Path Abs Dir -> [Path Abs File] -> m ([Path Abs File], [DynamicDependency]))]
strategies =
  [ RPM.lookupDependencies
  , DEB.lookupDependencies
  , APK.lookupDependencies
  ]

-- | Fallback strategy: resolve to a binary dependency for the binary.
-- This strategy is used if no other strategy succeeds at resolving the path.
strategyRawFingerprint :: Path Abs File -> DynamicDependency
strategyRawFingerprint file = DynamicDependency file Nothing
