module App.Fossa.VSI.DynLinked (
  analyzeDynamicLinkedDeps,
) where

import App.Fossa.VSI.DynLinked.Internal.Binary (dynamicLinkedDependencies)
import App.Fossa.VSI.DynLinked.Internal.Lookup (dynamicDependencies)
import App.Fossa.VSI.DynLinked.Internal.Resolve (environmentDistro, toSourceUnit)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, context)
import Control.Effect.Lift (Lift)
import Data.String.Conversion (toText)
import Effect.Exec (Exec)
import Effect.Logger (Logger, logDebug)
import Effect.ReadFS (ReadFS)
import Path (Abs, Dir, File, Path)
import Srclib.Types (SourceUnit (..))

-- | Dynamic linking analysis is sufficiently different from other analysis types that it cannot be just another strategy.
analyzeDynamicLinkedDeps ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Exec sig m
  ) =>
  Path Abs Dir ->
  Path Abs File ->
  m (Maybe SourceUnit)
analyzeDynamicLinkedDeps root target = context "analyze dynamic deps" $ do
  environment <- context "inspect distro" environmentDistro
  case environment of
    Nothing -> do
      logDebug "Skipping dynamic linking analysis: not a supported linux distro"
      pure Nothing
    Just distro -> do
      linkedFiles <- context ("analyze target: " <> toText target) $ dynamicLinkedDependencies target
      if null linkedFiles
        then do
          logDebug "Dynamic linking analysis: no dependencies found in target"
          pure Nothing
        else do
          linkedDeps <- context ("resolve linked dependencies: " <> (toText . show $ linkedFiles)) $ dynamicDependencies root linkedFiles
          Just <$> toSourceUnit root distro linkedDeps
