module Data.Text.Extra (
  splitOnceOn,
  splitOnceOnEnd,
  breakOnAndRemove,
  breakOnEndAndRemove,
  underBS,
  showT,
  dropPrefix,
) where

import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.String.Conversion (decodeUtf8, encodeUtf8, toText)
import Data.Text (Text)
import Data.Text qualified as Text

splitOnceOn :: Text -> Text -> (Text, Text)
splitOnceOn needle haystack = (first, strippedRemaining)
  where
    len = Text.length needle
    (first, remaining) = Text.breakOn needle haystack
    strippedRemaining = Text.drop len remaining

splitOnceOnEnd :: Text -> Text -> (Text, Text)
splitOnceOnEnd needle haystack = (strippedInitial, end)
  where
    len = Text.length needle
    (initial, end) = Text.breakOnEnd needle haystack
    strippedInitial = Text.dropEnd len initial

-- | Like Text.breakOn, but with two differences:
-- 1. This removes the text that was broken on, e.g., `Text.breakOn "foo" "foobar" == ("", "foobar")` `breakOnAndRemove "foo" "foobar" == ("", "bar")`
-- 2. This returns a `Maybe` value if the substring wasn't able to be found
--
-- >>> breakOnAndRemove "foo" "bazfoobar"
-- Just ("baz","bar")
--
-- >>> breakOnAndRemove "foo" "bar"
-- Nothing
breakOnAndRemove :: Text -> Text -> Maybe (Text, Text)
breakOnAndRemove needle haystack = do
  let (before, after) = Text.breakOn needle haystack
  if needle `Text.isPrefixOf` after
    then pure (before, Text.drop (Text.length needle) after)
    else Nothing

-- | Removes last needle (ordering from left to right) from haystack.
--
-- >> breakOnEndAndRemove ":" "a:b:c"
-- Just ("a:b", "c")
--
-- >> breakOnEndAndRemove ":" "a-b-c"
-- Nothing
-- -
breakOnEndAndRemove :: Text -> Text -> Maybe (Text, Text)
breakOnEndAndRemove needle haystack = do
  let (before, after) = Text.breakOnEnd needle haystack
  if needle `Text.isSuffixOf` before
    then pure (Text.dropEnd (Text.length needle) before, after)
    else Nothing

underBS :: (ByteString -> ByteString) -> Text -> Text
underBS f = decodeUtf8 . f . encodeUtf8

showT :: Show a => a -> Text
showT = toText . show

dropPrefix :: Text -> Text -> Text
dropPrefix pre txt = fromMaybe txt (Text.stripPrefix pre txt)
