module Strategy.Ruby.Gemspec (
  rubyString,
  parseRubyAssignment,
  Assignment (..),
  readAssignments,
  parseRubyArray,
  parseRubyWordsArray,
  rubyLicenseValuesP,
) where

import Control.Applicative ((<|>))
import Data.Char (isSpace)
import Data.Functor (($>))
import Data.List.Extra (singleton)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof), Parsec, anySingle, anySingleBut, between, choice, lookAhead, many, optional, sepBy, skipManyTill, takeWhile1P, takeWhileP, try)
import Text.Megaparsec.Char (char, space, space1, string)

type Parser = Parsec Void Text

-- |Given a single start delimiter, return start/end delimiters
selectDelim :: Char -> (Char, Char)
selectDelim = \case
  '{' -> ('{', '}')
  '<' -> ('<', '>')
  '(' -> ('(', ')')
  '[' -> ('[', ']')
  c -> (c, c)

rubyString :: Parser Text
rubyString =
  -- '.freeze' is a ruby idiom that turns a string into an immutable version of
  -- itself. In this case it's safe to just ignore it and take the string it's
  -- attached to.
  mconcat <$> (stringText <* optional (string ".freeze"))
  where
    betweenDelim :: Char -> Parser [Text]
    betweenDelim c =
      let (d1, d2) = selectDelim c
          -- only the close delimiter can stop parsing, so only check
          -- for escaped versions of that.
          delimEscape = string $ "\\" <> toText d2
       in between
            (char d1)
            (char d2)
            ( many
                ( try delimEscape
                    <|> (toText <$> anySingleBut d2)
                )
            )
    pctQ = optional (choice [char 'q', char 'Q'])
    arbitraryDelim = try $
      do (char '%' *> pctQ *> lookAhead anySingle) >>= betweenDelim
    stringText =
      (betweenDelim '"')
        <|> (betweenDelim '\'')
        <|> try arbitraryDelim

data Assignment a = Assignment
  { label :: Text
  , value :: a
  }
  deriving (Eq, Show)

parseRubyAssignment ::
  -- | Parser for the right-hand side of an assignment
  Parser a ->
  Parser (Assignment a)
parseRubyAssignment rhs = Assignment <$> (labelP <* space <* char '=' <* space) <*> valueP
  where
    -- edge-case: assumes that the initial label is on one line
    labelP = takeWhile1P Nothing (\c -> c /= '=' && not (isSpace c))
    valueP = rhs

lexeme :: Parser a -> Parser a
lexeme p = space *> p <* space

parseRubyArray :: Parser a -> Parser [a]
parseRubyArray p = char '[' *> sepBy (lexeme p) (char ',') <* char ']'

parseRubyWordsArray :: Parser [Text]
parseRubyWordsArray =
  char '%'
    *> (choice [char 'W', char 'w'])
    *> char '('
    *> lexeme (sepBy (takeWhileP (Just "word array element") wordChar) space1)
    <* char ')'
  where
    wordChar c = not $ isSpace c || c == ')'

-- |Try to parse any value that could potentially be a license to a list of text
rubyLicenseValuesP :: Parser [Text]
rubyLicenseValuesP = rubyArrayP <|> (singleton <$> rubyString)
  where
    rubyArrayP =
      try parseRubyWordsArray
        <|> parseRubyArray rubyString

-- | Parser to extract all assignments out of a section of text, ignoring everything else.
readAssignments ::
  -- | A parser for text appearing on the right side of an assignment
  Parser a ->
  Parser [Assignment a]
readAssignments rhs = findAssignments
  where
    tryAssignment = try . lexeme $ parseRubyAssignment rhs
    findAssignments = do
      res <-
        skipManyTill
          anySingle
          ( Just <$> tryAssignment
              <|> (eof $> Nothing)
          )
      case res of
        Nothing -> pure []
        Just assignment -> (assignment :) <$> findAssignments
