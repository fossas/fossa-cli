module App.Fossa.VSI.DynLinked.Internal.Lookup (
  dynamicDependencies,
) where

import App.Fossa.VSI.DynLinked.Internal.Lookup.DEB (debTactic)
import App.Fossa.VSI.DynLinked.Internal.Lookup.RPM (rpmTactic)
import App.Fossa.VSI.DynLinked.Types (DynamicDependency (..))
import Control.Algebra (Has)
import Control.Effect.Diagnostics (
  Diagnostics,
  ToDiagnostic (renderDiagnostic),
  recover,
  warn,
  (<||>),
 )
import Control.Monad (join)
import Data.Set (Set)
import Data.Set qualified as Set
import Effect.Exec (Exec)
import Effect.Logger (Logger, logDebug)
import Path (Abs, Dir, File, Path)
import Prettyprinter (pretty, vsep)

-- | Resolve the provided file paths, which represent dynamic dependencies of a binary, into a set of @DynamicDependency@.
dynamicDependencies ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has Logger sig m
  ) =>
  -- The scan root.
  Path Abs Dir ->
  -- The files to resolve.
  Set (Path Abs File) ->
  -- Resulting dependencies.
  m (Set DynamicDependency)
dynamicDependencies root files = do
  let targets = Set.toList files
  logDebug . pretty $ "Resolving files: " <> show targets
  resolved <- traverse (resolveFile root) targets
  pure $ Set.fromList resolved

resolveFile ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has Logger sig m
  ) =>
  Path Abs Dir ->
  Path Abs File ->
  m DynamicDependency
resolveFile root file = do
  -- When adding new tactics in the future, ensure that they fail (through diagnostics) if they cannot identify a dependency.
  -- <||> selects the first item to succeed without a diagnostic error.
  resolved <- recover $ rpmTactic root file <||> debTactic root file
  case join resolved of
    Just result -> pure result
    Nothing -> do
      warn $ MissingLinuxMetadata file
      pure $ fallbackTactic file

fallbackTactic :: Path Abs File -> DynamicDependency
fallbackTactic file = DynamicDependency file Nothing

newtype MissingLinuxMetadata = MissingLinuxMetadata (Path Abs File)

instance ToDiagnostic MissingLinuxMetadata where
  renderDiagnostic (MissingLinuxMetadata path) =
    vsep
      [ "Could not determine owning system package for file:"
      , pretty . show $ path
      ]
