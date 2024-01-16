-- | The ToDiagnostic typeclass
module Diag.Diagnostic (
  -- * ToDiagnostic
  ToDiagnostic (..),
  SomeDiagnostic (..),
  DiagnosticInfo (..),
) where

import Control.Exception (SomeException (SomeException))
import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.Error (SourceLocation)
import Data.String.Conversion (toText)
import Data.Text (Text)

data DiagnosticInfo = DiagnosticInfo
  { header :: Maybe Text
  , content :: Maybe Text
  , documentation :: Maybe [Text]
  , support :: Maybe Text
  , help :: Maybe Text
  , context :: Maybe Text
  , sourceLocation :: Maybe SourceLocation
  }
  deriving (Eq, Ord, Show)

class ToDiagnostic a where
  renderDiagnostic :: a -> DiagnosticInfo

instance ToDiagnostic Text where
  renderDiagnostic t = DiagnosticInfo (Just t) Nothing Nothing Nothing Nothing Nothing Nothing

instance ToDiagnostic SomeException where
  renderDiagnostic (SomeException exc) = DiagnosticInfo (Just $ "An exception occurred:" <> toText (show exc)) Nothing Nothing Nothing Nothing Nothing Nothing

-- | A class of diagnostic types that can be rendered in a user-friendly way
-- class ToDiagnostic a where
--   renderDiagnostic :: a -> Doc AnsiStyle

-- instance ToDiagnostic (Doc AnsiStyle) where
--   renderDiagnostic = id

-- instance ToDiagnostic Text where
--   renderDiagnostic = pretty

-- instance ToDiagnostic String where
--   renderDiagnostic = pretty

-- instance ToDiagnostic Errata where
--   renderDiagnostic err = pretty $ renderErrors [err]

-- instance ToDiagnostic SomeException where
--   renderDiagnostic (SomeException exc) = "An exception occurred: " <> pretty (show exc)

-- | An error with a ToDiagnostic instance and an associated stack trace
data SomeDiagnostic where
  SomeDiagnostic :: ToDiagnostic a => [Text] -> a -> SomeDiagnostic

instance ToJSON SomeDiagnostic where
  toJSON (SomeDiagnostic path cause) =
    object
      [ "errorPath" .= path
      , "errorCause" .= show (renderDiagnostic cause)
      ]
