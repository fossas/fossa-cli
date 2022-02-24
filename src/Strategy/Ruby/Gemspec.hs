module Strategy.Ruby.Gemspec (
  rubyString,
  parseRubyAssignment,
  Assignment (..),
  readAssignments,
) where

import Control.Applicative ((<|>))
import Data.Char (isSpace)
import Data.Functor (($>))
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof), Parsec, anySingle, anySingleBut, between, many, skipManyTill, takeWhile1P, try)
import Text.Megaparsec.Char (char, space)

type Parser = Parsec Void Text

rubyString :: Parser Text
rubyString =
  toText <$> (betweenDelim '"' <|> betweenDelim '\'')
  where
    betweenDelim :: Char -> Parsec Void Text String
    betweenDelim c = between (char c) (char c) (many (anySingleBut c))

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

-- | Parser to extract all assignments out of a section of text, ignoring everything else.
readAssignments ::
  -- | A parser for text appearing on the right side of an assignment
  Parser a ->
  Parser [Assignment a]
readAssignments rhs = findAssignments
  where
    tryAssignment = try $ parseRubyAssignment rhs <* space
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
