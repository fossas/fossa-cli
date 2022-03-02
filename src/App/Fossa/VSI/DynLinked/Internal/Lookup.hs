module App.Fossa.VSI.DynLinked.Internal.Lookup (
  dynamicDependencies,
) where

import App.Fossa.VSI.DynLinked.Internal.Lookup.APK (APKLookupTable, apkTactic, buildLookupTable)
import App.Fossa.VSI.DynLinked.Internal.Lookup.DEB (debTactic)
import App.Fossa.VSI.DynLinked.Internal.Lookup.RPM (rpmTactic)
import App.Fossa.VSI.DynLinked.Types (DynamicDependency (..))
import Control.Algebra (Has)
import Control.Effect.Diagnostics (
  Diagnostics,
  ToDiagnostic (renderDiagnostic),
  context,
  warn,
  (<||>),
 )
import Control.Effect.Lift (Lift)
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
  , Has (Lift IO) sig m
  ) =>
  -- The scan root.
  Path Abs Dir ->
  -- The files to resolve.
  Set (Path Abs File) ->
  -- Resulting dependencies.
  m (Set DynamicDependency)
dynamicDependencies root files = do
  apkLookupTable <- context "build APK lookup table" $ buildLookupTable root
  let targets = Set.toList files
  logDebug . pretty $ "Resolving files: " <> show targets
  resolved <- traverse (resolveFile apkLookupTable root) targets
  pure $ Set.fromList resolved

resolveFile ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has Logger sig m
  ) =>
  Maybe APKLookupTable ->
  Path Abs Dir ->
  Path Abs File ->
  m DynamicDependency
resolveFile table root file = do
  -- When adding new tactics in the future, ensure that they fail (through diagnostics) if they're not the last link in the chain.
  -- <||> selects the first item to succeed without a diagnostic error.
  resolved <- rpmTactic root file <||> debTactic root file <||> apkTactic table file
  case resolved of
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
      [ "Could not infer linux package manager, and its metadata for:"
      , pretty . show $ path
      ]
