-- | The ToDiagnostic typeclass
module Diag.Diagnostic (
  -- * ToDiagnostic
  ToDiagnostic (..),
  SomeDiagnostic (..),
) where

import Control.Exception (SomeException (SomeException))
import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.String.Conversion (toText)
import Data.Text (Text)
import Errata (Errata (..))

-- | A class of diagnostic types that can be rendered in a user-friendly way
class ToDiagnostic a where
  renderDiagnostic :: a -> Errata

instance ToDiagnostic Text where
  renderDiagnostic t = Errata (Just t) [] Nothing

instance ToDiagnostic String where
  renderDiagnostic s = Errata (Just $ toText s) [] Nothing

instance ToDiagnostic SomeException where
  renderDiagnostic (SomeException exc) = Errata (Just $ "An exception occurred:" <> toText (show exc)) [] Nothing

-- | An error with a ToDiagnostic instance and an associated stack trace
data SomeDiagnostic where
  SomeDiagnostic :: ToDiagnostic a => [Text] -> a -> SomeDiagnostic

instance ToJSON SomeDiagnostic where
  toJSON (SomeDiagnostic path cause) =
    object
      [ "errorPath" .= path
      , "errorCause" .= show (renderDiagnostic cause)
      ]
