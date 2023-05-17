module Data.Aeson.Extra (
  forbidMembers,
  neText,
  TextLike (..),
  encodeJSONToText,
) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Object
import Data.Aeson.Types (FromJSON (parseJSON), Key, Object, Parser, ToJSON, (.:))
import Data.Foldable (traverse_)
import Data.String.Conversion (ToString, decodeUtf8, toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text (null, strip)

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
newtype TextLike = TextLike {unTextLike :: Text} deriving (Eq, Ord)

instance Show TextLike where
  show (TextLike val) = toString val

instance ToString TextLike where
  toString (TextLike val) = toString val

instance FromJSON TextLike where
  parseJSON val = parseAsText <|> parseAsInt <|> parseAsDouble
    where
      parseAsText = TextLike <$> parseJSON val
      parseAsInt = TextLike . toText . show <$> parseJSON @Int val
      parseAsDouble = TextLike . toText . show <$> parseJSON @Double val

-- | Parser insert to prevent specific fields from being used in parsers
-- Primarily useful for rejecting Aeson fields which should be reported with custom error messages
--
-- >  parseJSON = withObject "MyDataType" $ \obj ->
-- >   MyDataType <$> obj .: "my-data-field"
-- >     <* forbidMembers "Custom error message" ["badfield1", "badfield2"] obj
forbidMembers :: Text -> [Key] -> Object -> Parser ()
forbidMembers typename names obj = traverse_ (badMember obj) names
  where
    badMember hashmap name =
      when (Object.member name hashmap) $
        fail . toString $
          "Invalid field name for " <> typename <> ": " <> toText name

-- | Parses non-empty value. It considers string of value with only
-- whitespaces to be empty.
--
-- >  parseJSON = withObject "MyDataType" $ \obj ->
-- >   MyDataType <$> obj `neText` "my-data-field"
neText :: (ToString a, FromJSON a) => Object -> Key -> Parser a
neText obj key = do
  (val :: a) <- obj .: key
  onlyNonEmpty key val

onlyNonEmpty :: (ToString a) => Key -> a -> Parser a
onlyNonEmpty key val =
  if (Text.null . Text.strip . toText . toString $ val)
    then fail $ "expected field '" <> toString key <> "' to be non-empty, but received: '" <> toString val <> "'"
    else pure val

-- | Like 'Data.Aeson.encode', but produces @Text@ instead of @ByteString@
encodeJSONToText :: ToJSON a => a -> Text
encodeJSONToText = decodeUtf8 . Aeson.encode
