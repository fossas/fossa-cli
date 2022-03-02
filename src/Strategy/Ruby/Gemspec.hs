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

-- |This is a parser for a ruby string literal. The strings it parses could look
-- these:
--
-- > "foo"
-- > 'foo'
--
-- The ending ' or " can be escaped within the string by '\'
--
-- > %q{foo}
-- > %Q{foo}
-- > %#foo#
-- > %^foo^
--
-- The character after the 'Q' or '%' in the above examples is the delimiter
-- for the string. If that character is '{', '(', '<', or '[' then its
-- matching right-hand side is the closing delimiter. As with quotes, the
-- ending delimiter can be escaped using '\'
--
-- Beyond the above, this parser also will consume and ignore '.freeze' or
-- '.freeze()' that appears at the end of the string. It is a ruby idiom that
-- turns a string into an immutable version of itself. It appears in some
-- gemspec files and is safe to ignore.
--
-- An edge-case I ignore here is string interpolation. Ruby allows strings
-- to contain text like `#{<expr>}`. The `<expr>` is evaluated and inserted
-- into the string in place of the interpolation text. This parser ignores
-- these and treats them like regular text.
rubyString :: Parser Text
rubyString =
  mconcat <$> (stringText <* optional freezeMethod)
  where
    freezeMethod = string ".freeze" >> optional (string "()")
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

-- |Parser for a single assignment statement of the form:
--
-- > righthand.side = some-value
--
-- whitespace around the '=' is ignored. '<lhs> = ' portion of the assignment
-- must appear on one line. The right-hand side is taken care of by the argument
-- parser.
parseRubyAssignment ::
  -- | Parser for the right-hand side of an assignment
  Parser a ->
  Parser (Assignment a)
parseRubyAssignment rhs = Assignment <$> (labelP <* lexeme (char '=')) <*> valueP
  where
    -- edge-case: assumes that the initial label is on one line
    labelP = takeWhile1P Nothing (\c -> c /= '=' && not (isSpace c))
    valueP = rhs

lexeme :: Parser a -> Parser a
lexeme p = space *> p <* space

-- | Ruby array literals look like this:
--
-- > [a, b , c]
--
-- This parser is parameterized by a parser `p` which parses the elements
-- in an array. Whitespace around array elements and the commas between them
-- are consumed and ignored.
parseRubyArray :: Parser a -> Parser [a]
parseRubyArray p = char '[' *> sepBy (lexeme p) (char ',') <* char ']'

-- |Ruby has a special syntax for making an array of strings that looks like
-- these examples:
--
-- > %w(foo bar)
-- > %W(foo bar baz)
--
-- This is interpreted as an array of strings. The 'W' variant also allows
-- interpolation, but as with 'rubyString' these are treated as ordinary text.
parseRubyWordsArray :: Parser [Text]
parseRubyWordsArray =
  char '%'
    *> (choice [char 'W', char 'w'])
    *> char '('
    *> lexeme (sepBy (takeWhileP (Just "word array element") wordChar) space1)
    <* char ')'
  where
    wordChar c = not $ isSpace c || c == ')'

-- |Try to parse any value that could potentially be a license.
-- This parser only works for licenses that are a string literal or an
-- array of string literals.
rubyLicenseValuesP :: Parser [Text]
rubyLicenseValuesP = rubyArrayP <|> (singleton <$> rubyString)
  where
    rubyArrayP =
      try parseRubyWordsArray
        <|> parseRubyArray rubyString

-- | Parser to extract all assignments out of a section of text, ignoring everything else.
--
-- An assignment is any text on a line followed by '=' followed by text that
-- could be read by the argument parser. All other non-assigment text is
-- ignored. Example:
--
-- > ignored text
-- > righthand.side = some-value
-- > more ignored text
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
