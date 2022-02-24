module Strategy.Ruby.Gemspec (
  rubyString,
  parseRubyAssignment,
  Assignment (..),
) where

import Control.Applicative ((<|>))
import Data.Char (isSpace)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingleBut, between, many, takeWhile1P)
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

spaceAround :: Parser a -> Parser a
spaceAround p = space *> p <* space

-- | Parse a ruby assignment statement where `rhs` is a parser for the
-- right-hand side of the equation, spaces on either side will be stripped out
-- before running rhs.
parseRubyAssignment :: Parser Text -> Parser (Assignment Text)
parseRubyAssignment rhs = Assignment <$> (space *> labelP) <* space <* char '=' <*> valueP
  where
    -- edge-case: assumes that the initial label is on one line
    labelP = takeWhile1P Nothing (\c -> c /= '=' && not (isSpace c))
    valueP = spaceAround rhs
