module Data.Aeson.Extra (
  TextLike (..),
) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Text (Text)
import Data.Text qualified as T

-- | A Text-like field
--
-- Sometimes fields in yaml files get incorrectly classified as numbers instead of text
--
-- This can happen when you have a field like @two@ in
--
-- @
--     myCoolField:
--       one: 1.0.0
--       two: 2
-- @
--
-- This makes things really hard to parse.
--
-- As a workaround, we try parsing as Text, then Int, then Double
newtype TextLike = TextLike {unTextLike :: Text}

instance FromJSON TextLike where
  parseJSON val = parseAsText <|> parseAsInt <|> parseAsDouble
    where
      parseAsText = TextLike <$> parseJSON val
      parseAsInt = TextLike . T.pack . show <$> parseJSON @Int val
      parseAsDouble = TextLike . T.pack . show <$> parseJSON @Double val
