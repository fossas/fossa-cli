-- | Module      : Strategy.Xcode.PbxprojParser
--
-- Provides elementary parsing of xcode's pbxproj.project file.
-- Xcode uses plist ascii, encoded in UTF-8 to perform record configurations.
--
-- There is no official spec, for the file format.
--
-- It can represents data in:
--  * Binary
--  * Date
--  * String
--  * Number
--  * List
--  * Dictionary
--
-- Relevant References:
--   * For ASCII types: https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/PropertyLists/OldStylePlists/OldStylePLists.html
--   * Unofficial References:
--     * http://www.monobjc.net/xcode-project-file-format.html
--
-- We intentionally parse all types into one of String, List, and Dictionary.
-- We do not distinguish between types of Xcode specific configurations.
module Strategy.Swift.Xcode.PbxprojParser (
  parsePbxProj,
  PbxProj (..),
  AsciiValue (..),
  objectsFromIsa,
  lookupText,
  textOf,
  lookupTextFromAsciiDict,

  -- * for testing only
  parseAsciiText,
  parseAsciiList,
  parseAsciiDict,
  parseAsciiValue,
) where

import Data.Functor (void)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.String.Conversion (ToText (toText))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (
  MonadParsec (takeWhile1P, try),
  Parsec,
  between,
  many,
  noneOf,
  sepEndBy,
  some,
  (<?>),
  (<|>),
 )
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer qualified as Lexer

type Parser = Parsec Void Text

sc :: Parser ()
sc =
  Lexer.space
    (void $ some $ char ' ' <|> char '\t' <|> char '\n' <|> char '\r')
    (Lexer.skipLineComment "//")
    (Lexer.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc

symbol :: Text -> Parser Text
symbol = Lexer.symbol sc

betweenCurlyBrackets :: Parser a -> Parser a
betweenCurlyBrackets = between (symbol "{") (symbol "}")

betweenParentheses :: Parser a -> Parser a
betweenParentheses = between (symbol "(") (symbol ")")

parseQuotedText :: Parser Text
parseQuotedText = between (symbol "\"") (symbol "\"") quoted
  where
    quoted :: Parser Text
    quoted = toText <$> many (nullifiedQuote <|> notEscapedQuote)

    nullifiedQuote :: Parser Char
    nullifiedQuote = string "\\\"" >> pure '"'

    notEscapedQuote :: Parser Char
    notEscapedQuote = noneOf ['\"']

parseText :: Parser Text
parseText = takeWhile1P (Just "text") (\c -> c `notElem` [';', ',', ')', ' ', '\t', '\n', '\r'])

-- | Potential type represented in Ascii plist file.
-- Reference : https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/PropertyLists/OldStylePlists/OldStylePLists.html
data AsciiValue
  = -- | Represents SomeText or "SomeText"
    -- Since we are only interested in textual representation of package name, and package version
    -- We represent potential binary, date, boolean ascii type as text.
    AText Text
  | -- | Represents {key = value;}
    ADict (Map Text AsciiValue)
  | -- | Represents (A, B,)
    AList [AsciiValue]
  deriving (Show, Eq, Ord)

data AsciiKeyValue = AsciiKeyValue Text AsciiValue deriving (Show, Eq, Ord)

parseAsciiText :: Parser AsciiValue
parseAsciiText = AText <$> lexeme (try parseQuotedText <|> parseText)

parseAsciiList :: Parser AsciiValue
parseAsciiList = AList <$> betweenParentheses (sepEndBy parseAsciiValue (symbol ","))

parseAsciiValue :: Parser AsciiValue
parseAsciiValue = try parseAsciiDict <|> try parseAsciiList <|> parseAsciiText

parseAsciiDict :: Parser AsciiValue
parseAsciiDict = ADict . Map.fromList <$> (lexeme (betweenCurlyBrackets $ sepEndBy (try parseAsciiKeyValue) (symbol ";")))

parseAsciiKeyValue :: Parser (Text, AsciiValue)
parseAsciiKeyValue = do
  key <- lexeme parseText <* symbol "="
  value <- lexeme $ try parseAsciiList <|> try parseAsciiDict <|> parseAsciiText
  pure (key, value)

-- | Represents Xcode's pbxproj.project file elementary structure.
-- Reference: http://www.monobjc.net/xcode-project-file-format.html
data PbxProj = PbxProj
  { archiveVersion :: Text
  , objectVersion :: Text
  , rootObject :: Text
  , classes :: Maybe AsciiValue
  , objects :: Maybe AsciiValue
  }
  deriving (Show, Eq, Ord)

lookupTextFromAsciiDict :: AsciiValue -> Text -> Maybe AsciiValue
lookupTextFromAsciiDict (AText _) _ = Nothing
lookupTextFromAsciiDict (AList _) _ = Nothing
lookupTextFromAsciiDict (ADict val) key = Map.lookup key val

textOf :: AsciiValue -> Maybe Text
textOf (AText t) = Just t
textOf _ = Nothing

lookupText :: AsciiValue -> Text -> Maybe Text
lookupText v key = (v `lookupTextFromAsciiDict` key) >>= textOf

supportedEncoding :: Text
supportedEncoding = "UTF8"

parsePbxProj :: Parser PbxProj
parsePbxProj = do
  _ <- symbol ("// !$*" <> supportedEncoding <> "*$!") <?> "to have UTF8 Encoding!"
  allValues <- parseAsciiDict

  archiveVersion <- case (textOf =<< (allValues `lookupTextFromAsciiDict` "archiveVersion")) of
    Nothing -> fail "could not find archiveVersion"
    Just av -> pure av

  objectVersion <- case (textOf =<< (allValues `lookupTextFromAsciiDict` "objectVersion")) of
    Nothing -> fail "could not find objectVersion"
    Just ov -> pure ov

  rootObject <- case (textOf =<< (allValues `lookupTextFromAsciiDict` "rootObject")) of
    Nothing -> fail "could not find rootObject"
    Just ro -> pure ro

  let classes = (allValues `lookupTextFromAsciiDict` "classes")
  let objects = (allValues `lookupTextFromAsciiDict` "objects")
  pure $ PbxProj archiveVersion objectVersion rootObject classes objects

-- | Gets list of objects with given isa value.
objectsFromIsa :: Text -> AsciiValue -> [Map Text AsciiValue]
objectsFromIsa _ (AText _) = []
objectsFromIsa _ (AList _) = []
objectsFromIsa key (ADict val) = mapMaybe getDict $ Map.elems filteredMap
  where
    filteredMap :: Map Text AsciiValue
    filteredMap = Map.filterWithKey (\_ v -> Just key == (textOf =<< v `lookupTextFromAsciiDict` "isa")) val

    getDict :: AsciiValue -> Maybe (Map Text AsciiValue)
    getDict (ADict v) = Just v
    getDict _ = Nothing
