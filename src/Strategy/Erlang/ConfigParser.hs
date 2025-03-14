{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Strategy.Erlang.ConfigParser (
  parseConfig,
  parseErlValue,
  parseAtom,
  parseNumber,
  parseRadixLiteral,
  parseErlString,
  parseTuple,
  parseErlArray,
  parseCharNum,
  parseIntLiteral,
  parseFloatLiteral,
  atom,
  AtomText (..),
  ErlValue (..),
  ConfigValues (..),
  intLiteralInBase,
  alphaNumToInt,
) where

import Data.Aeson.Types (ToJSON (toJSON))
import Data.Char qualified as C
import Data.Functor (($>))
import Data.List (foldl')
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void
import GHC.Generics (Generic)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

newtype AtomText = AtomText {unAtomText :: Text} deriving (Eq, Ord, Show, ToJSON)
newtype ConfigValues = ConfigValues {unConfigValues :: [ErlValue]} deriving (Eq, Ord, Show)

data ErlValue
  = ErlAtom AtomText
  | ErlString Text
  | ErlInt Int
  | ErlFloat Double
  | ErlArray [ErlValue]
  | ErlTuple [ErlValue]
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ErlValue where
  toJSON erl = case erl of
    ErlAtom a -> toJSON a
    ErlString a -> toJSON a
    ErlInt a -> toJSON a
    ErlFloat a -> toJSON a
    ErlArray a -> toJSON a
    ErlTuple a -> toJSON a

alphaNumSeq :: [Char]
alphaNumSeq = ['0' .. '9'] <> ['A' .. 'Z']

atom :: Text -> ErlValue
atom = ErlAtom . AtomText

parseConfig :: Parser ConfigValues
parseConfig = ConfigValues <$ scn <*> parseTuple `endBy1` symbol "."

parseErlValue :: Parser ErlValue
parseErlValue = parseErlArray <|> parseTuple <|> parseMap <|> parseBinary <|> parseNumber <|> parseErlString <|> parseAtom

parseNumber :: Parser ErlValue
parseNumber = try parseRadixLiteral <|> parseCharNum <|> try parseFloatLiteral <|> parseIntLiteral

{-  Erlang-specific: base#value
    Parse the string 'value' as an integer in base 'base'
    Base must be in set [2, 36] (inclusive)
    A-Z characters are treated as a continuation of a case-insensitive hexadecimal pattern
-}
parseRadixLiteral :: Parser ErlValue
parseRadixLiteral = do
  base <- L.decimal
  _ <- char '#'
  digitStr <- some alphaNumChar
  if 2 <= base && base <= 36
    then stringMatchesRadix base digitStr $> ErlInt (intLiteralInBase base digitStr)
    else fail "Base out of range [2, 36]"

stringMatchesRadix :: Int -> String -> Parser ()
stringMatchesRadix rad str = do
  let accept = take rad alphaNumSeq
  if all ((`elem` accept) . C.toUpper) str
    then pure ()
    else fail $ "Bad input for base " ++ show rad ++ ": " ++ str

-- | Returns the 'raw' string parsed in base 'base'
intLiteralInBase :: Int -> [Char] -> Int
intLiteralInBase base = foldl' accum 0
  where
    accum :: Int -> Char -> Int
    accum value c = value * base + alphaNumToInt c

-- | Return the ordinal offset for computing numerical values of letters, such as hexadecimal.  Offset is case-insensitive.
-- Note that this function is designed for [0-9A-Za-z] characters, but will not fail on other characters.
-- Callers should validate their own input, as this function may return useless values with unexpected characters.
alphaNumToInt :: Char -> Int
alphaNumToInt c =
  if C.isDigit c
    then C.digitToInt c
    else -- Get ascii code offset from 'A' (65), but start at 10 (A is 10 in hex)
      C.ord (C.toUpper c) - C.ord 'A' + 10

{-  Erlang-specific: $char
    evals to the ordinal of the character literal following the '$' symbol.
    $A = 65
    $\n = 10
-}
parseCharNum :: Parser ErlValue
parseCharNum = ErlInt . C.ord <$> (char '$' *> L.charLiteral)

-- Normal Floats
parseFloatLiteral :: Parser ErlValue
parseFloatLiteral = ErlFloat <$> L.signed (pure ()) L.float

-- Normal decimal Integers
parseIntLiteral :: Parser ErlValue
parseIntLiteral = ErlInt <$> L.signed (pure ()) L.decimal

parseAtom :: Parser ErlValue
parseAtom = ErlAtom <$> parseAtomText

parseAtomText :: Parser AtomText
parseAtomText = AtomText <$> lexeme (rawAtom <|> quotedAtom)
  where
    quotedAtom :: Parser Text
    quotedAtom = enclosed "'" "'" $ takeWhile1P (Just "quoted Atom") (/= '\'')
    --
    rawAtom :: Parser Text
    rawAtom = Text.cons <$> firstChar <*> rawAtomTail
    isRawAtomChar :: Char -> Bool
    isRawAtomChar c = C.isAlphaNum c || c == '_' || c == '@'
    firstChar :: Parser Char
    firstChar = satisfy (\c -> C.isAsciiLower c && C.isAlpha c)
    rawAtomTail :: Parser Text
    rawAtomTail = takeWhileP (Just "atom tail") isRawAtomChar

parseErlArray :: Parser ErlValue
parseErlArray = ErlArray <$> enclosed "[" "]" (parseErlValue `sepBy` symbol ",")

parseErlString :: Parser ErlValue
parseErlString = ErlString . toText . concat <$> some (lexeme quotedString)

quotedString :: Parser String
quotedString = char '\"' *> manyTill takeOne (char '\"')
  where
    takeOne :: Parser Char
    takeOne = L.charLiteral <|> takeOneEscaped

    takeOneEscaped = char '\\' *> escapedChar
    escapedChar = label "escaped character in string" anySingle

parseTuple :: Parser ErlValue
parseTuple = ErlTuple <$ symbol "{" <*> parseErlValue `sepBy1` symbol "," <* symbol "}"

enclosed :: Text -> Text -> Parser a -> Parser a
enclosed open close = between (symbol open) (symbol close)

symbol :: Text -> Parser Text
symbol = L.symbol scn

scn :: Parser ()
scn = L.space space1 (L.skipLineComment "%") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme scn

parseMap :: Parser ErlValue
parseMap = do
    _ <- symbol "#"
    -- Parse map contents between curly braces
    _ <- symbol "{"
    -- Parse key-value pairs separated by commas
    _ <- parseMapPairs `sepBy` symbol ","
    _ <- symbol "}"
    -- Return an empty tuple as a placeholder since we don't need the map contents
    pure $ ErlTuple []
  where
    parseMapPairs :: Parser ()
    parseMapPairs = do
      -- Parse key (any ErlValue)
      _ <- parseErlValue
      -- Parse the arrow operator '=>'
      _ <- symbol "=>"
      -- Parse value (any ErlValue)
      _ <- parseErlValue
      pure ()

-- Parse Erlang binary syntax << ... >>
parseBinary :: Parser ErlValue
parseBinary = do
    _ <- symbol "<<"
    -- Parse binary contents separated by commas
    _ <- parseErlValue `sepBy` symbol ","
    _ <- symbol ">>"
    -- Return an empty tuple as placeholder since we don't need the binary contents
    pure $ ErlTuple []
