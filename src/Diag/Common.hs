module Diag.Common (
  MissingDeepDeps (..),
  MissingEdges (..),
  AllDirectDeps (..),
) where

import Diag.Diagnostic (DiagnosticInfo (..), ToDiagnostic (renderDiagnostic))

data MissingDeepDeps = MissingDeepDeps
instance ToDiagnostic MissingDeepDeps where
  renderDiagnostic (MissingDeepDeps) = do
    let header = "Could not analyze deep dependencies."
    DiagnosticInfo (Just header) Nothing Nothing Nothing Nothing Nothing Nothing

data MissingEdges = MissingEdges
instance ToDiagnostic MissingEdges where
  renderDiagnostic (MissingEdges) = do
    let header = "Could not analyze edges between dependencies."
    DiagnosticInfo (Just header) Nothing Nothing Nothing Nothing Nothing Nothing

data AllDirectDeps = AllDirectDeps
instance ToDiagnostic AllDirectDeps where
  renderDiagnostic (AllDirectDeps) = do
    let header = "Could not differentiate between direct and deep dependencies, all dependencies will be reported as direct."
    DiagnosticInfo (Just header) Nothing Nothing Nothing Nothing Nothing Nothing
