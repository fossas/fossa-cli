{-# LANGUAGE RecordWildCards #-}

-- | The ToDiagnostic typeclass
module Diag.Diagnostic (
  -- * ToDiagnostic
  ToDiagnostic (..),
  SomeDiagnostic (..),

  -- * Helpers
  ActionableDiagCtx (..),
  renderActionable,
) where

import Control.Exception (SomeException (SomeException))
import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.List.NonEmpty (NonEmpty, toList)
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

data ActionableDiagCtx = ActionableDiagCtx
  { description :: Doc AnsiStyle
  , documentationLinks :: NonEmpty Text
  , troubleshootingSteps :: Maybe (NonEmpty Text)
  }
  deriving (Show)

renderActionable :: ActionableDiagCtx -> Doc AnsiStyle
renderActionable ActionableDiagCtx{..} =
  vsep
    [ description
    , ""
    , ""
    , "Relevant Documentation:"
    , renderList documentationLinks
    , ""
    , ""
    , renderTroubleshooting troubleshootingSteps
    ]
  where
    renderTroubleshooting :: Maybe (NonEmpty Text) -> Doc AnsiStyle
    renderTroubleshooting maybeSelfHelpMsg = case maybeSelfHelpMsg of
      Nothing -> ""
      Just doc ->
        vsep
          [ "Troubleshooting:"
          , renderList doc
          , ""
          ]

    renderList :: Pretty a => NonEmpty a -> Doc AnsiStyle
    renderList items = vsep $ map ((" - " <>) . pretty) (toList items)
