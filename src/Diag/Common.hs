module Diag.Common (
  MissingDeepDeps (..),
  MissingEdges (..),
  AllDirectDeps (..),
) where

import Diag.Diagnostic (ToDiagnostic (renderDiagnostic))
import Errata (Errata (..))

data MissingDeepDeps = MissingDeepDeps
instance ToDiagnostic MissingDeepDeps where
  renderDiagnostic (MissingDeepDeps) = do
    let header = "Could not analyze deep dependencies."
    Errata (Just header) [] Nothing

data MissingEdges = MissingEdges
instance ToDiagnostic MissingEdges where
  renderDiagnostic (MissingEdges) = do
    let header = "Could not analyze edges between dependencies."
    Errata (Just header) [] Nothing

data AllDirectDeps = AllDirectDeps
instance ToDiagnostic AllDirectDeps where
  renderDiagnostic (AllDirectDeps) = do
    let header = "Could not differentiate between direct and deep dependencies, all dependencies will be reported as direct."
    Errata (Just header) [] Nothing
