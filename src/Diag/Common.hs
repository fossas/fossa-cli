module Diag.Common (
  MissingDeepDeps (..),
  MissingEdges (..),
  AllDirectDeps (..),
) where

import Diag.Diagnostic (ToDiagnostic (renderDiagnostic))

data MissingDeepDeps = MissingDeepDeps
instance ToDiagnostic MissingDeepDeps where
  renderDiagnostic (MissingDeepDeps) =
    "Could not analyze deep dependencies."

data MissingEdges = MissingEdges
instance ToDiagnostic MissingEdges where
  renderDiagnostic (MissingEdges) =
    "Could not analyze edges between dependencies."

data AllDirectDeps = AllDirectDeps
instance ToDiagnostic AllDirectDeps where
  renderDiagnostic (AllDirectDeps) =
    "Could not differentiate between direct and deep dependencies, all dependencies will be reported as direct."
