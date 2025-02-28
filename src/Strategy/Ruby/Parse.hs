module Strategy.Ruby.Parse (
  rubyString,
  parseRubyAssignment,
  Assignment (..),
  readAssignments,
  parseRubyArray,
  parseRubyWordsArray,
  gemspecLicenseValuesP,
  Symbol (..),
  parseRubySymbol,
  parseRubyDict,
  PodSpecAssignmentValue (..),
  podspecAssignmentValuesP,
  findBySymbol,
) where

import Control.Applicative ((<|>))
import Data.Char (isSpace)
import Data.Functor (($>))
import Data.List (find)
import Data.List.Extra (singleton)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof), Parsec, anySingle, anySingleBut, between, choice, lookAhead, many, manyTill, optional, sepBy, skipManyTill, takeWhile1P, takeWhileP, try)
import Text.Megaparsec.Char (char, space1, string)

type Parser = Parsec Void Text

-- | Given a single start delimiter, return start/end delimiters
selectDelim :: Char -> (Char, Char)
selectDelim = \case
  '{' -> ('{', '}')
  '<' -> ('<', '>')
  '(' -> ('(', ')')
  '[' -> ('[', ']')
  c -> (c, c)

betweenDelim :: (Char, Char) -> Parser Text
betweenDelim (d1, d2) =
  mconcat
    <$> between
      (char d1)
      (char d2)
      ( many
          ( try delimEscape
              <|> (toText <$> anySingleBut d2)
          )
      )
  where
    -- only the close delimiter can stop parsing, so only check
    -- for escaped versions of that. The intent here is to consume the
    -- escaped ending delimiter before 'between' sees it and stops parsing.
    delimEscape = string $ "\\" <> toText d2

-- | This is a parser for a ruby string literal. The strings it parses could look
--  these:
--
--  > "foo"
--  > 'foo'
--
--  The ending ' or " can be escaped within the string by '\'
--
--  > %q{foo}
--  > %Q{foo}
--  > %#foo#
--  > %^foo^
--
--  The character after the 'Q' or '%' in the above examples is the delimiter
--  for the string. If that character is '{', '(', '<', or '[' then its
--  matching right-hand side is the closing delimiter. As with quotes, the
--  ending delimiter can be escaped using '\'
--
--  Beyond the above, this parser also will consume and ignore '.freeze' or
--  '.freeze()' that appears at the end of the string. It is a ruby idiom that
--  turns a string into an immutable version of itself. It appears in some
--  gemspec files and is safe to ignore.
--
--  An edge-case I ignore here is string interpolation. Ruby allows strings
--  to contain text like `#{<expr>}`. The `<expr>` is evaluated and inserted
--  into the string in place of the interpolation text. This parser ignores
--  these and treats them like regular text.
rubyString :: Parser Text
rubyString = stringText <* optional freezeMethod
  where
    freezeMethod = string ".freeze" >> optional (string "()")
    pctQ = optional (choice [char 'q', char 'Q'])
    betweenSelectedDelim = betweenDelim . selectDelim
    arbitraryDelim = try $
      do (char '%' *> pctQ *> lookAhead anySingle) >>= betweenSelectedDelim
    stringText =
      (betweenSelectedDelim '"')
        <|> (betweenSelectedDelim '\'')
        <|> try arbitraryDelim

data Assignment a = Assignment
  { label :: Text
  , value :: a
  }
  deriving (Eq, Show)

-- | Parser for a single assignment statement of the form:
--
--  > righthand.side = some-value
--
--  whitespace around the '=' is ignored. '<lhs> = ' portion of the assignment
--  must appear on one line. The right-hand side is taken care of by the argument
--  parser.
parseRubyAssignment ::
  -- | Parser for the right-hand side of an assignment
  Parser a ->
  Parser (Assignment a)
parseRubyAssignment rhs = Assignment <$> (labelP <* lexeme (char '=')) <*> valueP
  where
    -- edge-case: assumes that the initial label is on one line
    labelP = takeWhile1P Nothing (\c -> c /= '=' && not (isSpace c))
    valueP = rhs

-- | Consume 0 or more spaces or comments. A comment in ruby starts with
--  '#' and extends to the end of the line.
rubySpc :: Parser ()
rubySpc = many (space1 <|> commentP) $> ()
  where
    commentP = char '#' *> takeWhileP (Just "comment text") (/= '\n') $> ()

lexeme :: Parser a -> Parser a
lexeme p = rubySpc *> p <* rubySpc

-- | Ruby array literals look like this:
--
-- > [a, b , c]
--
-- This parser is parameterized by a parser `p` which parses the elements
-- in an array. Whitespace around array elements and the commas between them
-- are consumed and ignored.
parseRubyArray :: Parser a -> Parser [a]
parseRubyArray p = char '[' *> sepBy (lexeme p) (char ',') <* char ']'

newtype Symbol = Symbol {unSymbol :: Text}
  deriving (Show, Eq)

-- | Parses a ruby symbol. Ex:
--  > :this_is_a_symbol
--  > :"this is also a symbol"
--  > :'single quote symbol'
--
--  The top-most example stops when a space or '=>' appears. There are likely
--  other strings that should stop the parsing of a keyword in this case, but
--  this parser is designed specifically for usages where the keyword is used
--  as the key in a Ruby dictionary.
parseRubySymbol :: Parser Symbol
parseRubySymbol =
  Symbol
    <$> ( char ':'
            *> ( doubleQuoteSymbol
                  <|> singleQuoteSymbol
                  <|> simpleSymbol
               )
        )
  where
    simpleSymbolStop =
      choice
        [ lookAhead space1
        , lookAhead $ string "=>" $> ()
        , eof
        ]
    simpleSymbol = toText <$> manyTill anySingle simpleSymbolStop

    doubleQuoteSymbol = betweenDelim ('"', '"')
    singleQuoteSymbol = betweenDelim ('\'', '\'')

-- | Parse a dictionary of the form:
--
--  > { :key => val, :key2 => val2 }
--
--  The keys in the text should be symbols that 'parseRubySymbol' can parse.
parseRubyDict :: Parser a -> Parser [(Symbol, a)]
parseRubyDict rhs = between (char '{') (char '}') (sepBy keyValParse $ char ',')
  where
    keyValParse =
      (,)
        <$> lexeme parseRubySymbol
        <*> (lexeme (string "=>") *> lexeme rhs)

data PodSpecAssignmentValue
  = PodspecStr Text
  | PodspecDict [(Symbol, Text)]

findBySymbol :: Symbol -> [(Symbol, Text)] -> Maybe (Symbol, Text)
findBySymbol sym = find ((== sym) . fst)

podspecAssignmentValuesP :: Parser PodSpecAssignmentValue
podspecAssignmentValuesP =
  (PodspecStr <$> rubyString) <|> (PodspecDict <$> parseRubyDict rubyString)

-- | Ruby has a special syntax for making an array of strings that looks like
--  these examples:
--
--  > %w(foo bar)
--  > %W[foo bar baz]
--
--  This is interpreted as an array of strings. The delimiter after the 'w' can
--  be arbitrary as with '%q'. The 'W' variant also allows interpolation, but as
--  with 'rubyString' these are treated as ordinary text.
parseRubyWordsArray :: Parser [Text]
parseRubyWordsArray = do
  (d1, d2) <- parsePrefix
  let escapedEndDelim = toText '\\' <> toText d2
      arrayWord =
        mconcat
          <$> many
            ( takeWhile1P (Just "word element") (\c -> not $ isSpace c || c == d2 || c == '\\')
                <|> string escapedEndDelim
            )
  char d1 *> lexeme (sepBy arrayWord space1) <* char d2
  where
    parsePrefix =
      selectDelim
        <$> ( char '%'
                *> (choice [char 'W', char 'w'])
                *> lookAhead anySingle
            )

-- | Try to parse any value that could potentially be a license.
--  This parser only works for licenses that are a string literal or an
--  array of string literals.
gemspecLicenseValuesP :: Parser [Text]
gemspecLicenseValuesP = rubyArrayP <|> (singleton <$> rubyString)
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
