module Diag.Common (
  MissingDeepDeps (..),
  MissingEdges (..),
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
