module Data.Text.Jsonc (
  stripJsonc,
) where

import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec (
  Parsec,
  anySingle,
  eof,
  lookAhead,
  manyTill,
  runParser,
  satisfy,
  takeWhileP,
  try,
  (<|>),
 )
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void Text

-- | Strip JSONC artifacts (single-line comments, block comments,
-- and trailing commas) from text, producing valid JSON.
--
-- Handles:
--
--   * @\/\/@ single-line comments (to end of line)
--   * @\/\* ... \*\/@ block comments
--   * Trailing commas before @}@ or @]@
--
-- Strings (delimited by @"@) are preserved verbatim, including
-- any comment-like or comma characters they contain.
stripJsonc :: Text -> Either String Text
stripJsonc input = case runParser jsoncParser "jsonc" input of
  Left err -> Left (errorBundlePretty err)
  Right chunks -> Right (Text.concat chunks)

-- | Parse the entire JSONC input into chunks of valid JSON text.
jsoncParser :: Parser [Text]
jsoncParser = manyTill chunk eof
  where
    chunk :: Parser Text
    chunk =
      quotedString
        <|> lineComment
        <|> blockComment
        <|> trailingComma
        <|> plainText

-- | Parse a JSON string literal, preserving its contents verbatim.
quotedString :: Parser Text
quotedString = do
  _ <- char '"'
  contents <- manyTill stringChar (char '"')
  pure $ "\"" <> Text.concat contents <> "\""
  where
    stringChar :: Parser Text
    stringChar = escapedChar <|> (Text.singleton <$> anySingle)

    escapedChar :: Parser Text
    escapedChar = do
      _ <- char '\\'
      c <- anySingle
      pure $ "\\" <> Text.singleton c

-- | Parse a @\/\/@ line comment and discard it.
lineComment :: Parser Text
lineComment = do
  _ <- try (string "//")
  _ <- takeWhileP Nothing (/= '\n')
  _ <- (char '\n' $> ()) <|> eof
  pure ""

-- | Parse a @\/\* ... \*\/@ block comment and discard it.
blockComment :: Parser Text
blockComment = do
  _ <- try (string "/*")
  _ <- manyTill anySingle (string "*/")
  pure ""

-- | Parse a trailing comma (comma followed by optional whitespace
-- then @}@ or @]@) and discard only the comma, preserving whitespace.
trailingComma :: Parser Text
trailingComma = do
  _ <- try $ do
    _ <- char ','
    _ <- lookAhead (takeWhileP Nothing isJsonWhitespace *> satisfy isClosingBracket)
    pure ()
  pure ""
  where
    isClosingBracket :: Char -> Bool
    isClosingBracket c = c == '}' || c == ']'

-- | Parse one or more characters that aren't special
-- (not a quote, slash, or comma).
plainText :: Parser Text
plainText = do
  c <- anySingle
  if c == '/'
    then pure (Text.singleton c)
    else do
      rest <- takeWhileP Nothing (\x -> x /= '"' && x /= '/' && x /= ',')
      pure $ Text.singleton c <> rest

isJsonWhitespace :: Char -> Bool
isJsonWhitespace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'
