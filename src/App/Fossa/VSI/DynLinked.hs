module App.Fossa.VSI.DynLinked (
  analyzeDynamicLinkedDeps,
) where

import App.Fossa.VSI.DynLinked.Internal.Binary (dynamicLinkedDependencies)
import App.Fossa.VSI.DynLinked.Internal.Lookup (dynamicDependencies)
import App.Fossa.VSI.DynLinked.Internal.Resolve (environmentDistro, toSourceUnit)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, ToDiagnostic, context, errCtx, fatalText, recover, renderDiagnostic, warnOnErr)
import Control.Effect.Lift (Lift)
import Control.Effect.Reader (Reader)
import Data.String.Conversion (toText)
import Diag.Diagnostic qualified as DI
import Discovery.Filters (AllFilters)
import Effect.Exec (Exec)
import Effect.Logger (Logger)
import Effect.ReadFS (ReadFS)
import Path (Abs, Dir, Path)
import Path.Extra (SomePath, resolveAbsolute)
import Srclib.Types (SourceUnit (..))

-- | Dynamic linking analysis is sufficiently different from other analysis types that it cannot be just another strategy.
analyzeDynamicLinkedDeps ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has (Reader AllFilters) sig m
  ) =>
  Path Abs Dir ->
  SomePath ->
  m (Maybe SourceUnit)
analyzeDynamicLinkedDeps root (target) = context "Analyze dynamic deps" . recover . warnOnErr (SkippingDynamicDep target) $ do
  environment <- context "Inspect environment OS" environmentDistro
  case environment of
    Nothing -> do
      errCtx NotSupportedDistro $ fatalText "Unsupported operating system"
    Just distro -> do
      linkedFiles <- context ("Analyze target: " <> toText target) . dynamicLinkedDependencies $ resolveAbsolute root target
      if null linkedFiles
        then do
          errCtx NoDependenciesFound $ fatalText "No dynamically linked dependencies referenced in target"
        else do
          linkedDeps <- context "Resolve linked dependencies to packages" $ dynamicDependencies root linkedFiles
          toSourceUnit root distro linkedDeps

newtype SkippingDynamicDep = SkippingDynamicDep (SomePath)
instance ToDiagnostic SkippingDynamicDep where
  renderDiagnostic (SkippingDynamicDep path) = do
    let header = "Skipping dynamic analysis for target: " <> toText (show path)
    DI.DiagnosticInfo (Just header) Nothing Nothing Nothing Nothing Nothing Nothing

data NotSupportedDistro = NotSupportedDistro
instance ToDiagnostic NotSupportedDistro where
  renderDiagnostic (NotSupportedDistro) = do
    let ctx = "Fossa is executing in an environment that is not supported for dynamic link detection. Redhat and Debian based linux is currently supported."
    DI.DiagnosticInfo Nothing Nothing Nothing Nothing Nothing (Just ctx) Nothing

data NoDependenciesFound = NoDependenciesFound
instance ToDiagnostic NoDependenciesFound where
  renderDiagnostic (NoDependenciesFound) = do
    let ctx = "no dynamic dependencies found in target executable"
    DI.DiagnosticInfo Nothing Nothing Nothing Nothing Nothing (Just ctx) Nothing
