module Data.Text.Extra
  ( splitOnceOn,
    splitOnceOnEnd,
  )
where

import Data.Text (Text)
import qualified Data.Text as T

splitOnceOn :: Text -> Text -> (Text, Text)
splitOnceOn needle haystack = (head, strippedTail)
  where
    len = T.length needle
    (head, tail) = T.breakOn needle haystack
    strippedTail = T.drop len tail

splitOnceOnEnd :: Text -> Text -> (Text, Text)
splitOnceOnEnd needle haystack = (strippedHead, tail)
  where
    len = T.length needle
    (head, tail) = T.breakOnEnd needle haystack
    strippedHead = T.dropEnd len head