-- | The ToDiagnostic typeclass
module Diag.Diagnostic (
  -- * ToDiagnostic
  ToDiagnostic (..),
  SomeDiagnostic (..),

  -- * Warnings (temporary)
  MissingDeps (..),
  MissingEdges (..),
) where

import Control.Exception (SomeException (SomeException))
import Data.Aeson (ToJSON, object, toJSON, (.=))
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

---------- Warnings. FIXME: delete or move to another module as appropriate

data MissingDeps = MissingDeps

instance ToDiagnostic MissingDeps where
  renderDiagnostic = const "TODO: MissingDeps"

data MissingEdges = MissingEdges

instance ToDiagnostic MissingEdges where
  renderDiagnostic = const "TODO: MissingEdges"
