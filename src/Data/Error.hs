{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Error (
    SourceLocation (..),
    DiagnosticStyle (..),
    getSourceLocation,
    createBlock,
    createBody,
    createError,
    renderErrataStack,
    applyDiagnosticStyle,
) where

import Data.Maybe (fromMaybe)
import Data.String.Conversion (ToText (toText))
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Errata (Block (..), Errata (..), prettyErrors)
import Errata.Source (Source (emptySource))
import Errata.Styles (fancyStyle)
import GHC.Generics (Generic)
import GHC.Stack (CallStack, SrcLoc (..), getCallStack)

-- SourceLocation captures the file path, line, and col at a given call site
-- SourceLocation will be used in conjuction with our errors
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

createError :: Maybe Text -> [Block] -> Maybe Text -> Errata
createError = Errata

-- wrapper to create an Errata block
createBlock :: SourceLocation -> Maybe Text -> Maybe Text -> Block
createBlock SourceLocation{..} maybeHeader =
    Block
        fancyStyle
        (filePath, line, col)
        maybeHeader
        []

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
        -- red ANSI escape code
        ErrorStyle -> "\x1b[31m" <> "Error:" <> resetColor <> " "
        -- yellow ANSI escape code
        WarningStyle -> "\x1b[33m" <> "Warn:" <> resetColor <> " "
        -- blue ANSI escape code
        SupportStyle -> "\x1b[34m" <> "Support:" <> resetColor <> " "
        -- magenta ANSI escape code
        DocumentationStyle -> "\x1b[35m" <> "Documentation:" <> resetColor <> " "
        -- cyan ANSI escape code
        HelpStyle -> "\x1b[36m" <> "Help:" <> resetColor <> " "
        -- green ANSI escape code
        ContextStyle -> "\x1b[32m" <> "Context:" <> resetColor <> " "

-- ANSI escape code to reset foreground text color
resetColor :: Text
resetColor = "\x1b[39m"

applyDiagnosticStyle :: DiagnosticStyle -> Errata -> Errata
applyDiagnosticStyle style Errata{..} = case errataHeader of
    Just header -> Errata (Just $ toText style <> header) errataBlocks errataBody
    _ -> Errata errataHeader errataBlocks errataBody

buildMessageWithDiagnosticStyle :: DiagnosticStyle -> Text -> Text
buildMessageWithDiagnosticStyle style msg = toText style <> msg <> "\n"

renderErrataStack :: [Errata] -> TL.Text
renderErrataStack =
    prettyErrors @String
        emptySource