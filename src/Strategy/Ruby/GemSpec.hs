module Strategy.Ruby.Gemspec (rubyString) where

import Control.Applicative ((<|>))
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingleBut, between, many)
import Text.Megaparsec.Char (char)

type Parser = Parsec Void Text

rubyString :: Parser Text
rubyString =
  toText
    <$> ( betweenDelim '"'
            <|> betweenDelim '\''
        )
  where
    betweenDelim :: Char -> Parsec Void Text String
    betweenDelim c = between (char c) (char c) (many (anySingleBut c))
