{-# LANGUAGE RecordWildCards #-}

module Data.Diagnostic (
  -- * ToDiagnostic
  ToDiagnostic (..),
  SomeDiagnostic (..),

  -- * FailureBundle
  FailureBundle (..),
  renderFailureBundle,
  renderSomeDiagnostic,

  -- * Warnings (temporary)
  REPLACEME (..),
  MissingDeps (..),
  MissingEdges (..),
) where

import Control.Exception (SomeException (SomeException))
import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.List (intersperse)
import Data.Text (Text)
import Effect.Logger

-- | A class of diagnostic types that can be rendered in a user-friendly way
class ToDiagnostic a where
  renderDiagnostic :: a -> Doc AnsiStyle

instance ToDiagnostic (Doc AnsiStyle) where
  renderDiagnostic = id

instance ToDiagnostic Text where
  renderDiagnostic = pretty

instance ToDiagnostic SomeException where
  renderDiagnostic (SomeException exc) =
    "An exception occurred: " <> pretty (show exc)

-- | An error with a ToDiagnostic instance and an associated stack trace
data SomeDiagnostic where
  SomeDiagnostic :: ToDiagnostic a => [Text] -> a -> SomeDiagnostic

instance ToJSON SomeDiagnostic where
  toJSON (SomeDiagnostic path cause) =
    object
      [ "errorPath" .= path
      , "errorCause" .= show (renderDiagnostic cause)
      ]

---------- Failure bundles

data FailureBundle = FailureBundle
  { failureWarnings :: [SomeDiagnostic]
  , failureCause :: SomeDiagnostic
  }

instance Show FailureBundle where
  show = show . renderFailureBundle

renderFailureBundle :: FailureBundle -> Doc AnsiStyle
renderFailureBundle FailureBundle{..} =
  vsep $
    [ annotate (color Yellow) "----------"
    , annotate (color Yellow) "An error occurred:"
    , ""
    , indent 4 (renderSomeDiagnostic failureCause)
    , ""
    ]
      ++ if null failureWarnings
        then []
        else
          [ ">>>"
          , ""
          , indent 2 (annotate (color Yellow) "Relevant warnings include:")
          , ""
          , indent 4 (renderWarnings failureWarnings)
          ]

renderSomeDiagnostic :: SomeDiagnostic -> Doc AnsiStyle
renderSomeDiagnostic (SomeDiagnostic stack cause) =
  renderDiagnostic cause
    <> line
    <> line
    <> annotate (color Cyan) "Traceback:"
    <> line
    <> indent 2 (vsep (map (pretty . ("- " <>)) stack))

renderWarnings :: [SomeDiagnostic] -> Doc AnsiStyle
renderWarnings = vsep . intersperse (line <> "--" <> line) . map renderSomeDiagnostic

---------- Warnings. FIXME: delete or move to another module as appropriate

data REPLACEME = REPLACEME

instance ToDiagnostic REPLACEME where
  renderDiagnostic = const "TODO: REPLACEME"

data MissingDeps = MissingDeps

instance ToDiagnostic MissingDeps where
  renderDiagnostic = const "TODO: MissingDeps"

data MissingEdges = MissingEdges

instance ToDiagnostic MissingEdges where
  renderDiagnostic = const "TODO: MissingEdges"
