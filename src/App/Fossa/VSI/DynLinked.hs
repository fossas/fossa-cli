module App.Fossa.VSI.DynLinked (
  analyzeDynamicLinkedDeps,
) where

import App.Fossa.VSI.DynLinked.Internal.Binary (dynamicLinkedDependencies)
import App.Fossa.VSI.DynLinked.Internal.Lookup (dynamicDependencies)
import App.Fossa.VSI.DynLinked.Internal.Resolve (environmentDistro, toSourceUnit)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, ToDiagnostic, context, errCtx, fatalText, recover, renderDiagnostic, warnOnErr)
import Control.Effect.Lift (Lift)
import Data.String.Conversion (toText)
import Effect.Exec (Exec)
import Effect.Logger (Logger, logDebug, pretty)
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
analyzeDynamicLinkedDeps root target = context "analyze dynamic deps" . recover . warnOnErr (SkippingDynamicDep target) $ do
  environment <- context "inspect distro" environmentDistro
  case environment of
    Nothing -> do
      errCtx NotSupportedDistro $ fatalText "Unsupported operating system"
    Just distro -> do
      linkedFiles <- context ("analyze target: " <> toText target) $ dynamicLinkedDependencies target
      if null linkedFiles
        then do
          errCtx NoDependenciesFound $ fatalText "Unsupported operating system"
        else do
          logDebug . pretty $ "Dynamic linking analysis: resolving linked dependencies: " <> toText (show linkedFiles)
          linkedDeps <- context ("resolve linked dependencies: " <> toText (show linkedFiles)) $ dynamicDependencies root linkedFiles
          toSourceUnit root distro linkedDeps

newtype SkippingDynamicDep = SkippingDynamicDep (Path Abs File)
instance ToDiagnostic SkippingDynamicDep where
  renderDiagnostic (SkippingDynamicDep path) = pretty $ "Skipping dynamic analysis for target: " <> show path

data NotSupportedDistro = NotSupportedDistro
instance ToDiagnostic NotSupportedDistro where
  renderDiagnostic (NotSupportedDistro) = "fossa is executing in an environment that is not supported for dynamic link detection. Redhat and Debian based linux is currently supported."

data NoDependenciesFound = NoDependenciesFound
instance ToDiagnostic NoDependenciesFound where
  renderDiagnostic (NoDependenciesFound) = "no dynamic dependencies found in target executable"
