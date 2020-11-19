module Data.Text.Extra
  ( splitOnceOn,
    splitOnceOnEnd,
    breakOnAndRemove,
  )
where

import Data.Text (Text)
import qualified Data.Text as T

splitOnceOn :: Text -> Text -> (Text, Text)
splitOnceOn needle haystack = (first, strippedRemaining)
  where
    len = T.length needle
    (first, remaining) = T.breakOn needle haystack
    strippedRemaining = T.drop len remaining

splitOnceOnEnd :: Text -> Text -> (Text, Text)
splitOnceOnEnd needle haystack = (strippedInitial, end)
  where
    len = T.length needle
    (initial, end) = T.breakOnEnd needle haystack
    strippedInitial = T.dropEnd len initial

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
breakOnAndRemove needle haystack
  | (before,after) <- T.breakOn needle haystack
  , T.isPrefixOf needle after = Just (before, T.drop (T.length needle) after)
  | otherwise = Nothing
