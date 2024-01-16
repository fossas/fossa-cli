{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Error (
    SourceLocation (..),
    getSourceLocation,
    buildErrorMessage,
    buildHelpMessage,
    buildDocumentationMessage,
    buildContextMessage,
    createBlock,
    createBody,
    createError,
    renderErrors,
) where

import Algebra.Graph.Export (render)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Errata (Block (..), Pointer (..), blockSimple, errataSimple, prettyErrors)
import Errata.Source (Source (emptySource))
import Errata.Styles (basicPointer, basicStyle, fancyRedPointer, fancyRedStyle, fancyStyle)
import Errata.Types (Errata (..))
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
        documentation = maybe "" buildDocumentationMessage maybeDocumentation
        support = maybe "" buildSupportMessage maybeSupport
        help = maybe "" buildHelpMessage maybeHelp
        context = maybe "" buildContextMessage maybeContext

    content <> documentation <> support <> help <> context

-- red ANSI escape code
errorColor :: Text
errorColor = "\x1b[31m"

-- yellow ANSI escape code
warningColor :: Text
warningColor = "\x1b[33m"

-- blue ANSI escape code
supportColor :: Text
supportColor = "\x1b[34m"

-- magenta ANSI escape code
documentationColor :: Text
documentationColor = "\x1b[35m"

-- cyan ANSI escape code
helpColor :: Text
helpColor = "\x1b[36m"

-- green ANSI escape code
contextColor :: Text
contextColor = "\x1b[32m"

-- ANSI escape code to reset foreground text color
resetColor :: Text
resetColor = "\x1b[39m"

buildErrorMessage :: Text -> Text
buildErrorMessage msg = errorColor <> "Error:" <> resetColor <> " " <> msg

buildSupportMessage :: Text -> Text
buildSupportMessage msg = supportColor <> "Support:" <> resetColor <> " " <> msg <> "\n"

buildDocumentationMessage :: Text -> Text
buildDocumentationMessage msg = documentationColor <> "Documentation:" <> resetColor <> " " <> msg <> "\n"

buildHelpMessage :: Text -> Text
buildHelpMessage msg = helpColor <> "Help:" <> resetColor <> " " <> msg <> "\n"

buildContextMessage :: Text -> Text
buildContextMessage msg = contextColor <> "Context:" <> resetColor <> " " <> msg

renderErrors :: [Errata] -> TL.Text
renderErrors =
    prettyErrors @String
        emptySource