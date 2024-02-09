{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Error (
  SourceLocation (..),
  DiagnosticStyle (..),
  getSourceLocation,
  createEmptyBlock,
  createBody,
  createErrataWithHeaderOnly,
  renderErrataStack,
  applyDiagnosticStyle,
  combineErrataHeaders,
) where

import Data.Maybe (catMaybes, fromMaybe)
import Data.String.Conversion (ToText (toText))
import Data.Text (Text, intercalate)
import Effect.Logger (renderIt, vsep)
import Errata (Block (..), Errata (..), prettyErrors)
import Errata.Source (Source (emptySource))
import Errata.Styles (fancyStyle)
import Errata.Types (Header)
import GHC.Generics (Generic)
import GHC.Stack (CallStack, SrcLoc (..), getCallStack)
import Prettyprinter (Doc, annotate, pretty)
import Prettyprinter.Render.Terminal (
  AnsiStyle,
  Color (..),
  color,
 )

-- SourceLocation captures the file path, line, and col at a given call site
-- SourceLocation will be used in conjunction with our errors
data SourceLocation = SourceLocation
  { filePath :: FilePath
  , line :: Int
  , col :: Int
  }
  deriving (Eq, Ord, Show, Generic)

-- getSourceLocation returns SourceLocation with the filePath, line, col of the call site
getSourceLocation :: (?callStack :: CallStack) => SourceLocation
getSourceLocation = case getCallStack ?callStack of
  (_, loc) : _ -> SourceLocation (srcLocFile loc) (srcLocStartLine loc) (srcLocStartCol loc)
  _ -> SourceLocation "Unknown" 0 0

createErrataWithHeaderOnly :: Text -> Errata
createErrataWithHeaderOnly header = Errata (Just header) [] Nothing

-- wrapper to create an Errata block
createEmptyBlock :: SourceLocation -> Block
createEmptyBlock SourceLocation{..} =
  Block
    fancyStyle
    (filePath, line, col)
    Nothing
    []
    Nothing

createBody :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Text
createBody maybeContent maybeDocumentation maybeSupport maybeHelp maybeContext = do
  let content = fromMaybe "" maybeContent
      documentation = maybe "" (buildMessageWithDiagnosticStyle DocumentationStyle) maybeDocumentation
      support = maybe "" (buildMessageWithDiagnosticStyle SupportStyle) maybeSupport
      help = maybe "" (buildMessageWithDiagnosticStyle HelpStyle) maybeHelp
      context = maybe "" (buildMessageWithDiagnosticStyle ContextStyle) maybeContext

  content <> documentation <> support <> help <> context

data DiagnosticStyle
  = ErrorStyle
  | WarningStyle
  | DocumentationStyle
  | SupportStyle
  | HelpStyle
  | ContextStyle

instance ToText DiagnosticStyle where
  toText = \case
    ErrorStyle -> renderIt $ vsep [annotate (color Red) "Error: "]
    WarningStyle -> renderIt $ vsep [annotate (color Yellow) "Warn: "]
    SupportStyle -> renderIt $ vsep [annotate (color Blue) "Support: "]
    DocumentationStyle -> renderIt $ vsep [annotate (color Magenta) "Documentation: "]
    HelpStyle -> renderIt $ vsep [annotate (color Cyan) "Help: "]
    ContextStyle -> renderIt $ vsep [annotate (color Green) "Context: "]

applyDiagnosticStyle :: DiagnosticStyle -> Errata -> Errata
applyDiagnosticStyle style Errata{..} = case errataHeader of
  Just header -> Errata (Just $ toText style <> header) errataBlocks errataBody
  _ -> Errata errataHeader errataBlocks errataBody

buildMessageWithDiagnosticStyle :: DiagnosticStyle -> Text -> Text
buildMessageWithDiagnosticStyle style msg = "\n" <> toText style <> msg

renderErrataStack :: [Errata] -> Doc AnsiStyle
renderErrataStack =
  pretty
    . prettyErrors @String
      emptySource

{--
combineErrataHeaders is used in conjunction with ErrSupport, ErrHelp, ErrDoc, and ErrCtx.
The listed Err types are used to provide contextual details about a given error.
In order to attach these err details to an error, we need extract their contents,
which is currently stored in the Header field of the Errata object.
This function takes a list of Errata objects, extracts the contents from the header of each Errata,
and joins them together with a new line seperator.

We are choosing not to use the `renderErrataStack` function because it will display the errors with an extra new line seperator
between each Errata object.
  e.g.
    Help: Message 1

    Help: Message 2

Whereas combineErrataHeaders will display the errors as:
  Help: Message 1
  Help: Message 2
--}

combineErrataHeaders :: [Errata] -> Errata
combineErrataHeaders erratas =
  Errata
    { errataHeader = combineHeaders $ map errataHeader erratas
    , errataBlocks = []
    , errataBody = Nothing
    }
  where
    combineHeaders :: [Maybe Header] -> Maybe Header
    combineHeaders maybeHeaders = do
      let headers = catMaybes maybeHeaders
      if null headers
        then Nothing
        else Just $ intercalate "\n" headers
